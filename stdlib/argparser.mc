-- Miking is licensed under the MIT license.
-- Copyright (C) David Broman. See file LICENSE.txt
--
-- An argument parser library.
--
-- TODO(johnwikman, 2020-10-05): OrderedPositional, and UnorderedPositional (partially done, need to fix setup of relations...)
-- TODO(johnwikman, 2020-10-04): Add APDefault modifier.
-- TODO(johnwikman, 2020-10-12): Remove Match Conditions, does not give a whole lot, just causes a bunch of ambiguity...
-- TODO(johnwikman, 2020-10-17): Add built-in --help option.
--
-- birka update
-- birka download <package>
-- birka source add <URL...>
-- birka source remove <URL>
-- birka configure text
--
-- birka = progname
-- update|download|source|configure = Positional [Name "mode", Required, Once, Position 0, Values ["update", "download", "source", "configure"]]
-- add|remove = Positional [Name "srcmode", Required, Once, Position 1, Parent ("mode", "source"), Values ["add", "remove"]]
-- text = Positional [Name "confmode", Required, Once, Position 1, Parent ("mode", "configure"), Values ["text"]]
--
-- OPT(johnwikman, 2020-10-12):
-- Maybe skip the distinction of ordered positionals and unordered positionals?
-- The ordered positionals could just be a special case of the unordered
-- positionals where the ordered positionals follow a tree hierarchy and the
-- the root of that tree is the first required positional. Though this would
-- probably take some thinking to get it right, such that it does not limit the
-- expressiveness of the Enabled/Disabled functionality of the unordered
-- positionals. Would also make it more complicated to print the usage if no
-- simple procedure is found to handle if one of these tree structures does not
-- appear as the first positional. If done right, this would likely be the
-- optimal solution.
--
-- OPT(johnwikman, 2020-10-14): It could probably be possible to reexpress the
-- whole parent-child relation into specifying who enables a positional, which
-- positional that should have precedent ("the first"), and which positionals
-- that it enables. Then make sure that the argparser in unambiguous by
-- constructing a DFA for it and check that there any no collisions on
-- transaction. The only issue is that this could result in an exponential
-- number of states, making it too slow to use in practice. However, it could
-- be implemented for the utest-part instead.
--
-- So an ordered positional now becomes an enabled positional that has "OnlyThis"
-- priority, such that no other enabled positionals can be matched on when the
-- ordered positional is enabled. This moves the special case from the configuration
-- to the usage print, which has to keep track of leading "OnlyThis" positionals.
-- The sanity checking required now is that:
--  - Two "OnlyThis" positionals cannot be enabled at the same time.
--  - Values specified in two non-OnlyThis positionals active at the same time
--    cannot overlap. There can only be one such positional active at any time
--    that have no values specified. There are potentially an exponential
--    number of cases to check here, so this should not be enforced as standard.
--  - Each specified positional should be reachable and each of its enabling
--    and disabling policy should relate to a valid value in its specified
--    positional. The disabling and enabling policies should also not be in
--    conflict.

include "bool.mc"
include "char.mc"
include "either.mc"
include "hashmap.mc"
include "math.mc"
include "option.mc"
include "seq.mc"
include "string.mc"


-- Local hashmap definitions
let _str_traits = hashmapStrTraits
let _str_empty = hashmapEmpty
let _str_size = hashmapSize _str_traits
let _str_mem = hashmapMem _str_traits
let _str_lookupOrElse = hashmapLookupOrElse _str_traits
let _str_lookupOr = hashmapLookupOr _str_traits
let _str_lookup = hashmapLookup _str_traits
let _str_insert = hashmapInsert _str_traits
let _str_remove = hashmapRemove _str_traits
let _str_filter = hashmapFilter _str_traits
let _str_filterKeys = hashmapFilterKeys _str_traits
let _str_filterValues = hashmapFilterValues _str_traits
let _str_keys = hashmapKeys _str_traits
let _str_values = hashmapValues _str_traits

let _char_traits = {eq = eqChar, hashfn = char2int}
let _char_empty = hashmapEmpty
let _char_size = hashmapSize _char_traits
let _char_mem = hashmapMem _char_traits
let _char_lookupOrElse = hashmapLookupOrElse _char_traits
let _char_lookupOr = hashmapLookupOr _char_traits
let _char_lookup = hashmapLookup _char_traits
let _char_insert = hashmapInsert _char_traits
let _char_remove = hashmapRemove _char_traits
let _char_filter = hashmapFilter _char_traits
let _char_filterKeys = hashmapFilterKeys _char_traits
let _char_filterValues = hashmapFilterValues _char_traits
let _char_keys = hashmapKeys _char_traits
let _char_values = hashmapValues _char_traits

let _optstr_traits = {
  eq = lam x. lam y.
    match (x,y) with (Some a, Some b) then
      eqString a b
    else match (x,y) with (None (), None ()) then
      true
    else
      false,
  hashfn = optionMapOr 0 hashmapStrTraits.hashfn
}
let _optstr_empty = hashmapEmpty
let _optstr_size = hashmapSize _optstr_traits
let _optstr_mem = hashmapMem _optstr_traits
let _optstr_lookupOrElse = hashmapLookupOrElse _optstr_traits
let _optstr_lookupOr = hashmapLookupOr _optstr_traits
let _optstr_lookup = hashmapLookup _optstr_traits
let _optstr_insert = hashmapInsert _optstr_traits
let _optstr_remove = hashmapRemove _optstr_traits
let _optstr_filter = hashmapFilter _optstr_traits
let _optstr_filterKeys = hashmapFilterKeys _optstr_traits
let _optstr_filterValues = hashmapFilterValues _optstr_traits
let _optstr_keys = hashmapKeys _optstr_traits
let _optstr_values = hashmapValues _optstr_traits


-- Returns true iff both first values match and both second values are None, or
-- if both first values match and both second values contains the same string.
let _eqStrOptTuple: (String, Option String) -> (String, Option String) -> Bool =
  lam t1. lam t2.
  if not (eqString t1.0 t2.1) then
    false
  else match (t1.0, t2.0) with (None (), None ()) then
    true
  else match (t1.0, t2.0) with (Some v1, Some v2) then
    eqString v1 v2
  else
    false


-- Public types
type APModifier a
-- OPTION ONLY MODIFIERS
con APShort: Char -> APModifier a
con APLong: String -> APModifier a
con APMetavar: String -> APModifier a
con APApply: (a -> a) -> APModifier a
-- POSITIONAL ONLY MODIFIERS
con APName: String -> APModifier a
con APOnlyThis: () -> APModifier a
con APEnabledBy: (String, Option String) -> APModifier a
con APDisabledBy: (String, Option String) -> APModifier a
-- GENERIC MODIFIERS
con APDescription: String -> APModifier a
con APApplyVal: (String -> a -> a) -> APModifier a
con APValue: (String, String) -> APModifier a
con APValues: [String] -> APModifier a
con APValueDescription: (String, String) -> APModifier a
con APRequired: () -> APModifier a
con APPostCond: ((a -> Bool), String) -> APModifier a -- A check that will be performed if this has been matched with an input argument

type APConfiguration a
con APOption: [APModifier a] -> APConfiguration a
con APPositional: [APModifier a] -> APConfiguration a
con APFlag: {short: Char, long: String, description: String, apply: a -> a} -> APConfiguration a
con APLongFlag: {long: String, description: String, apply: a -> a} -> APConfiguration a
con APMetavarFlag: {short: Char, long: String, metavar: String, description: String, apply: String -> a -> a} -> APConfiguration a
con APLongMetavarFlag: {long: String, metavar: String, description: String, apply: String -> a -> a} -> APConfiguration a
con APMutuallyExclusiveOptions: [String] -> APConfiguration a
con APAny: {name: String, description: String, apply: String -> a -> a} -> APConfiguration a
con APMany: {name: String, description: String, apply: String -> a -> a} -> APConfiguration a
con APMode: {name: String, values: [String], description: String, apply: String -> a -> a} -> APConfiguration a
con APSubmode: {name: String, parent: String, values: [String], description: String, apply: String -> a -> a} -> APConfiguration a
con APSubmodeSpecific: {name: String, parent: String, parentval: String, values: [String], description: String, apply: String -> a -> a} -> APConfiguration a
con APUniqueOnlyThisEnable: String -> APConfiguration a

-- Invalid characters
let _invalidChars = ['-', '=', ' ', '\r', '\n', '\t']
let _isInvalidChar = lam c. any (eqChar c) _invalidChars


-- Internal types
type APOptionItem_ a = {
  short: Option Char,
  long: String,
  metavar: Option String,
  description: Option String,
  apply: String -> a -> a,
  -- values: <ValueName> -> <Description>
  values: HashMap String (Option String),
  required: Bool,
  postconds: [((a -> Bool), String)]
}

type APPositionalItem_ a = {
  name: String,
  description: Option String,
  apply: String -> a -> a,
  onlyThis: Bool,
  -- enables/disables map "value match" -> positonal name. None () value
  -- indicates a wildcard.
  enables: HashMap (Option String) [String],
  disables: HashMap (Option String) [String]
  -- values: <ValueName> -> <Description>
  values: HashMap String (Option String),
  required: Bool,
  postconds: [((a -> Bool), String)],
  -- INTERMEDIARY VALUES
  enabledBy: [(String, Option String)],
  disabledBy: [(String, Option String)]
}

type ArgParser_ a = {
  -- The name of the program being run, specified by `head args`.
  name: String,
  -- Contains all options as a map from long option name -> option record
  options: HashMap String (APOptionItem_ a),
  -- Lookup from short option name -> long option name
  shortOptLookup: HashMap Char String,
  -- A map over long option names -> other options than cannot be specified with this option
  optExclusions: HashMap String [String],
  -- All positionals as a map name -> positional record
  positionals: HashMap String (APPositionalItem_ a),
  -- Default enabled positionals
  initiallyEnabledPositionals: [String],
  -- Lines of error messages that has been produced during the argument
  -- parsing. An empty list indicates that no error has occurred. These should
  -- ideally be printed as `strJoin "\n" ap.errors`. The reason for not
  -- embedding this into an Either type is to allow accumulation of as many
  -- errors as possible when forming the parser.
  errors: [String]
}


-- Takes the settings list to an APOption and returns either a tuple
-- containing the long option name with list of error messages, or a
-- well-formed option.
let _formOption: [APModifier a] -> Either (String, [String]) (APOptionItem_ a) = lam mods.
  let opt: APOptionItem_ a = {
    short = None (),
    long = "",
    metavar = None (),
    description = None (),
    apply = lam _. lam o. o,
    values = _str_empty,
    required = false,
    postconds = []
  } in

  let accrecord = {opt = opt, errs = [], hasLong = false, hasApply = false, unprocessed = []} in

  -- Set basic properties
  let state = foldl (lam acc. lam mod.
    -- VALID OPTION MODIFIERS
    match mod with APShort c then
      if _isInvalidChar c then
        {acc with errs = snoc acc.errs (join ["Invalid short modifier ", showChar c])}
      else if optionIsNone acc.opt.short then
        {acc with opt = {acc.opt with short = Some c}}
      else
        {acc with errs = snoc acc.errs "Multiple short modifiers"}
    else match mod with APLong s then
      match find _isInvalidChar s with Some c then
        {acc with errs = snoc acc.errs (join ["Invalid character ", showChar c, " in long modifier"])}
      else if null s then
        {acc with errs = snoc acc.errs "Empty long modifier"}
      else if not acc.hasLong then
        {{acc with opt = {acc.opt with long = s}}
              with hasLong = true}
      else
        {acc with errs = snoc acc.errs "Multiple long modifiers"}
    else match mod with APMetavar mv then
      if optionIsNone acc.opt.metavar then
        {acc with opt = {acc.opt with metavar = Some mv}}
      else
        {acc with errs = snoc acc.errs "Multiple metavars"}
    else match mod with APDescription s then
      if optionIsNone app.opt.description then
        {acc with opt = {acc.opt with description = s}}
      else
        {acc with errs = snoc acc.errs "Multiple descriptions"}
    else match mod with APValue (val, desc) then
      if _str_mem val acc.opt.values then
        {acc with errs = snoc acc.errs (join ["Specified duplicate value \"", val, "\""])}
      else
        {acc with opt = {acc.opt with values = _str_insert val (Some desc) acc.opt.values}}
    else match mod with APValues vs then
      foldl (lam accInternal. lam v.
        if _str_mem v accInternal.opt.values then
          {accInternal with errs = snoc accInternal.errs (join ["Specified duplicate value \"", v, "\""])}
        else
          {accInternal with opt = {accInternal.opt with values = _str_insert v (None ()) accInternal.opt.values}}
      ) acc vs
    else match mod with APRequired _ then
      {acc with opt = {acc.opt with required = true}}
    else match mod with APPostCond pctup then
      {acc with opt = {acc.opt with postconds = cons pctup acc.opt.postconds}}

    -- OPTION MODIFIERS TO BE PROCESSED LATER
    else match mod with APValueDescription _ then
      {acc with unprocessed = snoc acc.unprocessed mod}
    else match mod with APApply _ then
      {acc with unprocessed = snoc acc.unprocessed mod}
    else match mod with APApplyVal _ then
      {acc with unprocessed = snoc acc.unprocessed mod}

    -- INVALID OPTION MODIFIERS
    else match mod with APName _ then
      {acc with errs = snoc acc.errs "Invalid option setting \"APName\""}
    else match mod with APFirst _ then
      {acc with errs = snoc acc.errs "Invalid option setting \"APOnlyThis\""}
    else match mod with APEnabledBy _ then
      {acc with errs = snoc acc.errs "Invalid option setting \"APEnabledBy\""}
    else match mod with APDisabledBy _ then
      {acc with errs = snoc acc.errs "Invalid option setting \"APDisabledBy\""}

    -- This should be a complete match
    else never
  ) state mods in

  -- Process options that depends on previously processed settings
  let state = foldl (lam acc. lam mod.
    match mod with APValueDescription (val, desc) then
      if not (_str_mem val acc.opt.values) then
        {acc with errs = snoc acc.errs (join ["Cannot set description for non-existent value \"", val, "\""])}
      else if optionIsSome (_str_lookupOr (None ()) val acc.opt.values) then
        {acc with errs = snoc acc.errs (join ["Duplicate description for value \"", val, "\""])}
      else
        {acc with opt = {acc.opt with values = _str_insert val (Some desc) acc.opt.values}}
    else match mod with APApply fn then
      if acc.hasApply then
        {acc with errs = snoc acc.errs "Multiple apply functions"}
      else
        {{acc with opt = {acc.opt with apply = lam _. fn}}
              with hasApply = true}
    else match mod with APApplyVal fn then
      if optionIsNone acc.opt.metavar then
        {acc with errs = snoc acc.errs "Cannot apply value to an option without a metavar"}
      else if opt.hasApply then
        {acc with errs = snoc acc.errs "Multiple apply functions"}
      else
        {{acc with opt = {acc.opt with apply = fn}}
              with hasApply = true}
    else
      -- This should be unreachable
      {state with errs = snoc state.errs "INTERNAL ERROR: Unprocessed option modifier"}
  ) {state with unprocessed = []} state.unprocessed in

  -- Check that an option has a long name
  let state =
    if not state.hasLong then
      {{state with errs = snoc state.errs "Missing long option name"}
              with opt = {state.opt with long = "<unnamed>"}}
    else
      state
  in

  -- Check that a method of applying the option exists
  let state =
    if not state.hasApply then
      {state with errs = snoc state.errs "Missing option apply function"}
    else
      state
  in

  if not (null state.errs) then
    Left (state.opt.long, state.errs)
  else
    Right state.opt


-- Takes the settings list to an APPositional and returns either a tuple that
-- contains identifying name with a list of error messages, or a well-formed
-- positional.
let _formPositional: [APModifier a] -> Either (String, [String]) (APPositionalItem_ a) = lam mods.
  let pos: APPositionalItem_ a = {
    name = "",
    description = None (),
    apply = lam _. lam o. o,
    onlyThis = false,
    enables = _optstr_empty,
    disables = _optstr_empty,
    values = _str_empty,
    required = false,
    postconds = [],
    enabledBy = [],
    disabledBy = []
  } in

  let state = {pos = pos, errs = [], hasName = false, hasApply = false, unprocessed = []} in

  -- Set basic properties
  let state = foldl (lam acc. lam mod.
    -- VALID ORDERED POSITIONAL MODIFIERS
    match mod with APName s then
      if null s then
        {acc with errs = snoc acc.errs "Empty name specified for positional"}
      else if not acc.hasName then
        {{acc with pos = {acc.pos with name = s}}
              with hasName = true}
      else
        {acc with errs = snoc acc.errs "Multiple names for positional"}
    else match mod with APDescription s then
      if optionIsNone app.pos.description then
        {acc with pos = {acc.pos with description = s}}
      else
        {acc with errs = snoc acc.errs "Multiple descriptions"}
    else match mod with APApplyVal fn then
      if pos.hasApply then
        {acc with errs = snoc acc.errs "Multiple apply functions"}
      else
        {{acc with pos = {acc.pos with apply = fn}}
              with hasApply = true}
    else match mod with APOnlyThis _ then
      {acc with pos = {acc.pos with onlyThis = true}}
    else match mod with EnabledBy t1 then
      -- NOTE(johnwikman, 2020-10-18): No sanity checking here for either
      -- EnabledBy or DisabledBy since these lists might get modified after an
      -- invocation to this function.
      {acc with pos = {acc.pos with enabledBy = snoc acc.pos.enabledBy t1}}
    else match mod with DisabledBy t1 then
      {acc with pos = {acc.pos with disabledBy = snoc acc.pos.disabledBy t1}}
    else match mod with APValue (val, desc) then
      if _str_mem val acc.pos.values then
        {acc with errs = snoc acc.errs (join ["Specified duplicate value \"", val, "\""])}
      else
        {acc with pos = {acc.pos with values = _str_insert val (Some desc) acc.pos.values}}
    else match mod with APValues vs then
      foldl (lam accInternal. lam v.
        if _str_mem v accInternal.pos.values then
          {accInternal with errs = snoc accInternal.errs (join ["Specified duplicate value \"", v, "\""])}
        else
          {accInternal with pos = {accInternal.pos with values = _str_insert v (None ()) accInternal.pos.values}}
      ) acc vs
    else match mod with APRequired _ then
      {acc with pos = {acc.pos with required = true}}
    else match mod with APPostCond pctup then
      {acc with pos = {acc.pos with postconds = cons pctup acc.pos.postconds}}

    -- ORDERED POSITIONAL MODIFIERS TO BE PROCESSED LATER
    else match mod with APValueDescription _ then
      {acc with unprocessed = snoc acc.unprocessed mod}

    -- INVALID ORDERED POSITIONAL MODIFIERS
    else match mod with APShort _ then
      {acc with errs = snoc acc.errs "Invalid ordered positional setting \"APShort\""}
    else match mod with APLong _ then
      {acc with errs = snoc acc.errs "Invalid ordered positional setting \"APLong\""}
    else match mod with APMetavar _ then
      {acc with errs = snoc acc.errs "Invalid ordered positional setting \"APMetavar\""}
    else match mod with APApply _ then
      {acc with errs = snoc acc.errs "Invalid ordered positional setting \"APApply\""}

    -- This should be a complete match
    else never
  ) state mods in

  -- Process options that depends on previously processed settings
  let state = foldl (lam acc. lam mod.
    match mod with APValueDescription (val, desc) then
      if not (_str_mem val acc.pos.values) then
        {acc with errs = snoc acc.errs (join ["Cannot set description for non-existent value \"", val, "\""])}
      else if optionIsSome (_str_lookupOr (None ()) val acc.pos.values) then
        {acc with errs = snoc acc.errs (join ["Duplicate description for value \"", val, "\""])}
      else
        {acc with pos = {acc.pos with values = _str_insert val (Some desc) acc.pos.values}}
    else
      -- This should be unreachable
      {state with errs = snoc state.errs "INTERNAL ERROR: Unprocessed ordered positional modifier"}
  ) {state with unprocessed = []} state.unprocessed in

  -- Check that positional has a name
  let state =
    if not state.hasName then
      {{state with errs = snoc state.errs "Missing positional name"}
              with pos = {state.pos with name = "<unnamed>"}}
    else
      state
  in

  -- Check that a method of applying the positional exists
  let state =
    if not state.hasApply then
      {state with errs = snoc state.errs "Missing apply function"}
    else
      state
  in

  -- Check, if non-first, that it has a parent
  let state =
    if and (not (state.pos.first)) (optionIsNone state.pos.parent) then
      {state with errs = snoc state.errs "Non-first ordered positional missing parent"}
    else
      state
  in

  if not (null state.errs) then
    Left (state.pos.name, state.errs)
  else
    Right state.pos


-- Adds a parsed option result to the ArgParser if it passes all checks.
-- Otherwise concatenates the error messages to the internal list of error
-- messages.
let _addOption: Either (String, [String]) (APOptionItem_ a) -> ArgParser_ a -> ArgParser_ a =
  lam eitherOpt. lam ap.
  match eitherOpt with Left (long, errs) then
    {ap with errors = join [
      ap.errors,
      [join ["Misformed option \"", long, "\":"]],
      map (concat "  - ") errs -- <- apply indendation
    ]}
  else match eitherOpt with Right opt then
    if _str_mem opt.long ap.options then
      {ap with errors = snoc ap.errors (
        join ["Duplicate long option name \"", opt.long, "\""]
       )}
    else if optionMapOr false (lam c. _char_mem c ap.shortOptLookup) opt.short then
      {ap with errors = snoc ap.errors (
        -- We know that opt.short is `Some c`, only using '-' as a placeholder.
        join ["Duplicate short option name \"", optionGetOr '-' opt.short, "\""]
       )}
    else
      -- Insert short lookup (if it exists)
      let ap = optionMapOr ap (lam c. _char_insert c opt.long ap.shortOptLookup) opt.short in
      {ap with options = _str_insert opt.long opt ap.options}
  else never

-- Adds a parsed positional result to the ArgParser if it passes all checks.
-- Otherwise concatenates the error messages to the internal list of error
-- messages.
let _addPositional: Either (String, [String]) (APPositionalItem_ a) -> ArgParser_ a -> ArgParser_ a =
  lam eitherPos. lam ap.
  match eitherPos with Left (name, errs) then
    {ap with errors = join [
      ap.errors,
      [join ["Misformed positional \"", name, "\":"]],
      map (concat "  - ") errs -- <- apply indendation
    ]}
  else match eitherPos with Right pos then
    if _str_mem pos.name ap.positionals then
      {ap with errors = join [
        ap.errors,
        [join ["Duplicate positional name \"", name, "\":"]],
        map (concat "  - ") errs -- <- apply indendation
      ]}
    else
    {ap with positionals = _str_insert pos.name pos ap.positionals}
  else never


-- Translates a list of ArgParser configurations into an internal ArgParser representation.
recursive let _translateConfig: [APConfiguration a] -> [APConfiguration a] -> ArgParser_ a -> ArgParser_ a =
  lam configs. lam postconfigs. lam ap.

  -- SCAN PRIMARY CONFIGURATION
  match configs with [conf] ++ remaining then
    match config with APOption mods then
      let optret = _formOption mods in
      _translateConfig remaining postconfigs (_addOption optret ap)
    else match conf with APOrderedPositional mods then
      let posret = _formOrderedPositional mods in
      _translateConfig remaining postconfigs (_addOrderedPositional posret ap)
    else match conf with APUnorderedPositional mods then
      let posret = _formUnorderedPositional mods in
      _translateConfig remaining postconfigs (_addUnorderedPositional posret ap)
    else match conf with APFlag r then
      let optret = _formOption [
        APShort r.short,
        APLong r.long,
        APDescription r.description,
        APApply r.apply
      ] in
      _translateConfig remaining postconfigs (_addOption optret ap)
    else match conf with APLongFlag r then
      let optret = _formOption [
        APLong r.long,
        APDescription r.description,
        APApply r.apply
      ] in
      _translateConfig remaining postconfigs (_addOption optret ap)
    else match conf with APMetavarFlag r then
      let optret = _formOption [
        APShort r.short,
        APLong r.long,
        APMetavar r.metavar,
        APDescription r.description,
        APApplyVal r.apply
      ] in
      _translateConfig remaining postconfigs (_addOption optret ap)
    else match conf with APLongMetavarFlag r then
      let optret = _formOption [
        APLong r.long,
        APMetavar r.metavar,
        APDescription r.description,
        APApplyVal r.apply
      ] in
      _translateConfig remaining postconfigs (_addOption optret ap)
    else match conf with APAny r then
      let posret = _formPositional [
        APName r.name,
        APDescription r.description,
        APApplyVal r.apply
      ] in
      _translateConfig remaining postconfigs (_addPositional posret ap)
    else match conf with APMany r then
      let posret = _formPositional [
        APName r.name,
        APDescription r.description,
        APRequired (),
        APApplyVal r.apply
      ] in
      _translateConfig remaining postconfigs (_addPositional posret ap)
    else match conf with APMode r then
      -- FIX THIS!!!!
      let posret = _formPositional [
        APName r.name,
        APDescription r.description,
        APValues r.values,
        APApplyVal r.apply,
        APOnlyThis (),
        APRequired (),
        APDisabledBy (r.name, None ()) -- disable itself directly after match
      ] in
      _translateConfig remaining postconfigs (_addPositional posret ap)
    else match conf with APSubmode r then
      let posret = _formPositional [
        APName r.name,
        APDescription r.description,
        APValues r.values,
        APApplyVal r.apply,
        APOnlyThis (),
        APRequired (),
        APEnabledBy (r.parent, None ()), -- wildcard match
        APDisabledBy (r.name, None ()) -- disable itself directly after match
      ] in
      -- Make sure that this enable is unique, i.e. that it does not collide
      -- with a specific submode, by adding the uniqueness to the postconfig
      _translateConfig remaining
                       (snoc postconfigs (APUniqueOnlyThisEnable r.name))
                       (_addPositional posret ap)
    else match conf with APSubmodeSpecific r then
      let posret = _formPositional [
        APName r.name,
        APDescription r.description,
        APValues r.values,
        APApplyVal r.apply,
        APOnlyThis (),
        APRequired (),
        APEnabledBy (r.parent, Some r.parentval),
        APDisabledBy (r.name, None ()) -- disable itself directly after match
      ] in
      _translateConfig remaining postconfigs (_addPositional posret ap)

    -- Scan these as secondary configurations, as they might depend on other
    -- configurations being scanned first.
    else match conf with APMutuallyExclusiveOptions _ then
      _translateConfig remaining (snoc postconfigs conf) ap
    else match conf with APUniqueOnlyThisEnable _ then
      _translateConfig remaining (snoc postconfigs conf) ap

    -- Should be a complete match
    else never

  -- SCAN SECONDARY CONFIGURATION (that depends on primary configurations)
  else match postconfigs with [conf] ++ remaining then
    match conf with APMutuallyExclusiveOptions longs then
      -- Check that all specified names actually exists
      let invalidLongs = filter (lam l. not (_str_mem l ap.options)) longs in
      if not (null invalidLongs) then
        _translateConfig [] remaining {ap with errors = snoc ap.errors (
          join ["Cannot set mutual exclusion for non-existent options: ",
                strJoin ", " invalidLongs])}
      else
        let updatedAp = foldl (lam accap. lam long.
          let existing = _str_lookupOrElse (lam _. []) long accap.optExclusions in
          -- Mark all _other_ longs as mutually exclusive to this one
          let mutExludes = filter (lam s. not (eqString long s)) longs in
          {accap with optExclusions = _str_insert long (concat existing mutExludes) accap.optExclusions}
        ) ap longs in
        _translateConfig [] remaining updatedAp
    else match conf with APUniqueOnlyThisEnable name then
      -- Check that the specified name is an actual positional
      match _str_lookup name ap.positionals with Some pos then
        if not (pos.onlyThis) then
          _translateConfig [] remaining {ap with errors = snoc ap.errors (
            join ["Cannot set unique OnlyThis enable for non-OnlyThis positional \"", name, "\""])}
        else --continue
          let candidatePositionals = _str_filterValues (lam p. and p.onlyThis (not (eqString pos.name p.name))) ap.positionals in
          let candidateEnables = join (map (lam p. p.enabledBy) candidatePositionals) in
          -- Do not disable on wildcard!
          let toBeDisabledBy = filter (lam e. optionIsSome e.1) candidateEnables in

          let newPos = {pos with disabledBy = concat pos.disabledBy toBeDisabledBy} in
          _translateConfig [] remaining {ap with positionals = _str_insert newPos.name newPos ap.positionals}
      else
        _translateConfig [] remaining {ap with errors = snoc ap.errors (
          join ["Cannot set unique OnlyThis enable for non-existent positional \"", name, "\""])}
    else
      _translateConfig [] remaining {ap with errors = snoc ap.errors "INTERNAL ERROR: Unprocessed configuration"}

  -- No more configs to translate!
  else
    ap
end


-- BRIEF: Sets up the enables+disables relations for positionals
-- Also check that the enable-disable logic is sound for individual positionals.
let _setupPositionalRelations: ArgParser_ a -> ArgParser_ a = lam ap.
  foldl (lam ap. lam pos.
    let ap =
      foldl (lam ap. lam enable.
        -- Sanity check that this enable is not also a disable
        -- NOTE(johnwikman, 2020-10-18): This really does not even need to be
        -- performed, as something being directly disabled after being enabled
        -- is the same effect as not being enabled in the first place. However,
        -- being disabled just after being enabled is quite funky and something
        -- that is most likely not intended.
        let isCollision: (String, Option String) -> Bool = lam disable.
          if not (eqString enable.0 disable.0) then
            false
          else match (enable.1, disable.1) with (Some ev, Some dv) then
            eqString ev dv
          else match (enable.1, disable.1) with (_, None ()) then
            true -- disabled in all cases, which collides with the enable
          else
            -- disable is specific, and enable is None. Should probably have a
            -- "never" after this, but not sure if this is too advanced to check.
            false
        in
        if any isCollision pos.disabledBy then
          {ap with errors = snoc ap.errors (join ["Incompatible enable and disable ",
           "conditions for positional \"", pos.name, "\" that is both enabled and ",
           "disabled by \"", enable.0, "\""])}
        else -- Sanity Check: OK

        -- Add this enable to its referenced parent
        match _str_lookup enable.0 ap.positionals with Some parent then
          let newParent = {parent with enables = _optstr_insert enable.1 pos.name parent.enables} in
          {ap with positionals = _str_insert parent.name newParent ap.positionals}
        else
          {ap with errors = snoc ap.errors (join ["Referenced enabler positional \"",
           enable.0, "\" from positional \"", pos.name, "\" does not exist."])}
      ) ap pos.enabledBy
    in

    -- Add disables in the same way as for enables, except that it is not
    -- necessary to perform sanity checking here.
    foldl (lam ap. lam disable.
      match _str_lookup disable.0 ap.positionals with Some parent then
        let newParent = {parent with disables = _optstr_insert disable.1 pos.name parent.disables} in
        {ap with positionals = _str_insert parent.name newParent ap.positionals}
      else
        {ap with errors = snoc ap.errors (join ["Referenced disabler positional \"",
         disable.0, "\" from positional \"", pos.name, "\" does not exist."])}
    ) ap pos.disabledBy
  ) ap (_str_values ap.positionals)


-- BRIEF: Sets up the positionals that should be enabled by default. These are
-- the positionals without an explicit enabler.
let _setupInitialPositionals: ArgParser_ a -> ArgParser_ a = lam ap.
  let initial = _str_filterValues (lam pos. null pos.enabledBy) ap.positionals in
  {ap with initiallyEnabledPositionals = map (lam pos. pos.name) initial}


-- BRIEF: Verify that a state cannot be reached where a match would be ambiguous.
--
-- This is potentially very costly and should probably not be used in runtime.
let _checkPositionalAmbiguity: ArgParser_ a -> ArgParser_ a = lam ap.
  -- Convert a sequence of strings to a single string representation. Use the
  -- invalid '-' character as a separator.
  let seqAsString: [String] -> String = lam seq. strJoin "-" seq in

  -- Add an element to the visited list, putting it in sorted order.
  let insertIntoSeq: String -> [String] -> [String] = lam s. lam seq.
    TODO
  in
  TODO


-- BRIEF: Sets up and verifies the ordered positionals in the ArgParser
--
-- This assumes that each ordered positional is well-formed. This just
-- configures the relations between them.
let _setupOrderedPositionalRelations: ArgParser_ a -> ArgParser_ a = lam ap.
  if leqi (_str_size ap.orderedPositionals) 0 then
    -- Does not contain any ordered positionals, ignore setup
    ap
  else -- continue

  -- STAGE 1: Check that one and exactly one positional is marked as the first
  let firsts = _str_filterValues (lam p. p.first) ap.orderedPositionals in
  let ap =
    match firsts with [] then
      {ap with errors = snoc ap.errors "Missing a first ordered positional"}
    else match firsts with [fst] then
      {ap with firstPositional = Some fst} -- OK, exactly one first ord. pos.
    else
      {ap with errors = snoc ap.errors (join ["Multiple first ordered positionals: ",
                                              strJoin ", " firsts])}
  in

  -- STAGE 2: Convert from parent data to children data
  let ap =
    foldl (lam apAcc. lam pos.
      match pos.parent with Some (pname, pvalue) then
        match _str_lookup pname apAcc.orderedPositionals with Some parent then
          match _optstr_lookup pvalue parent.children with Some childname then
            -- Two children with exact same relation to parent
            let name = optionMapOr "wildcard" (lam s. ['\"', s, '\"']) pvalue in
            {apAcc with errors = snoc apAcc.errors (join ["Ordered positionals \"", pname,
                                                          "\" and \"", childname, "\" have ",
                                                          "exact same relation to parent."])}
          else
            let newParent =
              {parent with children = _optstr_insert pvalue pos.name parent.children}
            in
            -- If parent has a limited range of values and parent value is
            -- specified, then verify that parent has that value in range.
            match (_str_keys parent.values, pvalue) with ([_] + _, Some val) then
              if _str_mem val parent.values then
                {apAcc with orderedPositionals = _str_insert pname newParent apAcc.orderedPositionals}
              else
                {apAcc with errors = snoc apAcc.errors (join ["Parent \"", pname, "\" (of positional \"",
                                                              pos.name, "\") does not have \"",
                                                              val, "\" in its valid range of values."])}
            else
              {apAcc with orderedPositionals = _str_insert pname newParent apAcc.orderedPositionals}
        else
          {apAcc with errors = snoc apAcc.errors (join ["Parent \"", pname, "\" does not exist for positional \"", pos.name "\""])}
      else
        -- No parent
        apAcc
    ) ap (_str_values ap.orderedPositionals)
  in

  -- STAGE 3: Check for relational loops
  recursive let traversePosTree: [String] -> ArgParser_ a -> String -> ArgParser_ a =
    lam visited. lam apAcc. lam current.
    -- Check loop
    match lastIndex (eqString current) visited with Some idx then
      let path = snoc (splitAt visited idx).1 current in
      {apAcc with errors = snoc apAcc.errors (join ["Ordered positional loop detected: ",
                                                    strJoin " -> " path])}
    else
      -- No loop so far
      let positional = _str_lookupOrElse (lam _. error "unreachable") current apAcc.orderedPositionals in
      foldl (lam apAcc. lam child.
        traversePosTree (snoc visited current) apAcc child
      ) apAcc (_optstr_values positional.children)
  in
  let ap = optionMapOr ap (traversePosTree [] ap) ap.firstPositional in

  -- All checks done
  ap

/-
- BRIEF: Sets up and verifies the unordered positionals in the ArgParser
-
- This assumes that each unordered positional is well-formed. This just
- configures the relations between them and the ordered positionals. This also
- assumes that the setup of the ordered positionals was successful without any
- errors.
-
- This checks for the obvious ambiguities, that might arise from
- misconfiguration. This does NOT evaluate any of the specified match
- conditions the check for ambiguity that was, mainly due to that we do not
- know at this stage how computationally heavy those are.
-
- TODO(johnwikman, 2020-10-12): Add option to perform extra ambiguity checks,
-                               though not enabled by default.
-/
let _setupUnorderedPositionalRelations: ArgParser_ a -> ArgParser_ a = lam ap.
  let allUnorderedPositionals = _str_values ap.unorderedPositionals in

  -- STAGE 1: Set a unordered positionals to be enabled from the start, those who are not configured to be enabled by anything
  let initiallyEnabled = filter (lam pos. null pos.enabledBy) allUnorderedPositionals in
  let ap = {ap with initiallyEnabledUnorderedPositionals = initiallyEnabled} in


  -- Check that the enabled unordered positionals are not known at this stage to have an ambiguous match
  let checkAmbiguity: String -> [String] -> [String] -> ArgParser_ a -> ArgParser_ a =
    lam errmsgPrefix. lam enabledUnordered. lam enabledOrdered. lam ap.

    -- Extract all the positionals that match the entered names
    let unordered = _str_filterValues (lam uop. any (eqString uop.name) enabledUnordered) ap.unorderedPositionals in
    let ordered = _str_filterValues (lam op. any (eqString op.name) enabledOrdered) ap.orderedPositionals in

    -- Set ambiguity if two unordered positionals lack match-conditions and specified values
    let ap =
      let ambiguous = filter (lam uop. and (null uop.matchconds) (neqi (_str_size uop.values))) unordered in
      let ambiguousNames = map (lam uop. uop.name) ambiguous in

      let wildcardOrdered = filter (lam op. TODO) ordered in

      if geqi (length ambiguousNames) 2 then
        {ap with errors = snoc ap.errors (join [errmsgPrefix, "Ambiguous unordered positionals ",
                                                strJoin ", " ambiguousNames])}
      else
        ASDF
    in

    -- Extract all values

    TODO!
  in

  -- STAGE 2: Setup each ordered Positional such that if it is enabled by something, then it is also enabled by its children, unless one of its children instead disables it.
  --          A.k.a. propagate the enabling
  --
  -- STAGE 2.5: Check ambiguity at the same time, between the active unordered positionals, but also against a possible non-required ordered positional
  recursive let traverseEnableDisable: [String] -> ArgParser_ a -> String -> ArgParser_ a =
    lam enabled. lam apAcc. lam posname.

    let newEnables = filter (lam uop. any (eqString posname) uop.enabledBy) allUnorderedPositionals in
    let newEnableNames = map (lam uop. uop.name) newEnables in

    let newDisables = filter (lam uop. any (eqString posname) uop.enabledBy) allUnorderedPositionals in
    let newDisableNames = map (lam uop. uop.name) newDisables in

    -- Add the new enables (and make sure to remove duplicates)
    let enabled = distinct eqString (concat enabled newEnableNames) in

    -- Remove those listed as newly disabled
    let enabled = filter (lam s. not (any (eqString s) newDisableNames)) enabled in

    let pos = _str_lookupOrElse (lam _. error "unreachable") posname ap.orderedPositionals in
    let pos = {pos with enables = enabled} in

    -- IF: This ordered positional is not required to be filled in, then there cannot be any ambiguity whether
    -- the argparser should match any of the unordered positionals or the "optional" ordered positional. Or also,
    -- if this does not have a wildcard child, then it should also make sure that none of its children also
    -- produce an ambiguous match with the enabled unordered positionals.
    TODO!
  in

  let ap =
    match ap.firstOrderedPositional with Some name then
      traverseEnableDisable ap.initiallyEnabledUnorderedPositionals ap name
    else
      ap
  in

  -- STAGE 2.5: Check that no two unordered positionals, active at the same time, lack 
  --          match conditions. In which case they are known already at this stage to
  --          cause ambiguous parsing. Though they still might cause ambiguous parsing
  --          later on.
  TODO ))))
  ap


-- Constructs a parser based on the provided configuration
let _createParser: [APConfiguration a] -> String -> ArgParser_ a =
  lam configs. lam progname.
  let ap = {
    name = progname,
    options = hashmapEmpty,
    shortOptLookup = hashmapEmpty,
    optExclusions = hashmapEmpty,
    positionals = hashmapEmpty,
    firstPositional = None (),
    initiallyEnabledUnorderedPositionals = [],
    errors = []
  } in

  -- Scan configuration and form initial parser
  let ap = translateConfig_ configs [] ap in
  if not (null ap.errors) then
    ap
  else -- continue

  let ap = _setupOrderedPositionalRelations ap in
  if not (null ap.errors) then
    ap
  else -- continue

  let ap = _setupUnorderedPositionalRelations ap in
  if not (null ap.errors) then
    ap
  else -- continue

  ap


-- Checks whether the argparser is well-formed. Returns None () on success.
-- Otherwise Some String containing the error message.
let argparserCheckError: [APConfiguration a] -> Some String = lam configs.
  let ap = _createParser configs "<confcheck>" in
  if null ap.errors then
    None ()
  else
    Some (strJoin "\n" (cons "Misformed ArgParser:" errs))


-- argparserParse. Parse
let argparserParse: [APConfiguration a] -> a -> [String] -> Either String a =
  lam configs. lam defaults. lam args.
  let apret = _createParser configs (head args) in
  match apret with Left errs then
    Left (strJoin "\n" (cons "Misformed ArgParser:" errs))
  else match apret with Right ap then
    --TODO:
    -- SET UP DEFAULT STATE (enable every positional by default that does not have an explicit enabler)
    defaults
  else never

mexpr

type TestArgs = {
  help: Bool,
  version: Bool,
  debugParser: Bool,
  optLevel: Int,
  defines: [String],
  mode: String,
  confmode: String,
  isOn: Bool
} in

let defaults: TestArgs = {
  help = false,
  version = false,
  debugParser = false,
  optLevel = 0,
  defines = [],
  mode = "<none>",
  confmode = "<none>",
  isOn = false
} in

let apconfig = [
  APFlag {short = 'h', long = "help",
          description = "Prints a help message and exits.",
          apply = lam o. {o with help = true}},
  APFlag {short = 'v', long = "version",
          description = "Prints version and exits.",
          apply = lam o. {o with version = true}},
  APLongFlag {long = "debug-parser",
              description = "Show debug prints during parsing.",
              apply = lam o. {o with debugParser = true}},

  -- Options with a metavar
  APMetavarFlag {short = 'O', long = "optimization-level", metavar = "LEVEL",
                 description = "Set optimization level.",
                 apply = lam mv. lam o. {o with optLevel = string2int mv}},
  APMetavarFlag {short = 'D', long = "define", metavar = "DEFINITION",
                 description = "Add C preprocessor definition.",
                 apply = lam mv. lam o. {o with defines = snoc o.defines mv}},

  -- Mutually exclusive options
  APLongFlag {long = "on", description = "Turns it on!",
              apply = lam o. {o with isOn = true}},
  APLongFlag {long = "off", description = "Turns it off!",
              apply = lam o. {o with isOn = false}},
  APMutuallyExclusiveOptions ["on", "off"],

  -- Modes (a special case of positional)
  APMode {name = "mode", values = ["compile", "eval", "repl", "test", "config"],
          description = "Toolchain mode.", apply = lam v. lam o. {o with mode = v}},
  APSubmodeSpecific {name = "confmode", parent = "mode", parentval = "config",
                     values = ["get", "set"], 
                     description = "Handle global toolchain configuration.",
                     apply = lam v. lam o. {o with confmode = v}},
  APPositional [
    APName "mcore file",
    APApplyVal (lam f. lam o. {o with mcfiles = cons f o.mc}),
    APMatchOn (lam m. isSuffix eqChar ".mc" m),
    APDescription "MCore input source files. (ends in .mc)",
    -- This should not be available when specifying config
    APDisabledBy [("mode", "config")]
  ],

  -- <prog> config get <get name>:
  APPositional [
    APName "get name",
    APDescription "The configuration variable to get.",
    APParentValue ("confmode", "get"),
    APApplyVal (lam v. lam o. {o with getName = v}),
    APRequired ()
  ],

  -- <prog> config set <set name> <set value>:
  APPositional [
    APName "set name",
    APDescription "The configuration variable to set.",
    APParentValue ("confmode", "set"),
    APApplyVal (lam v. lam o. {o with setName = v}),
    APRequired ()
  ],
  APPositional [
    APName "set value",
    APDescription "The value to set the variable to.",
    APParent "set name",
    APApplyVal (lam v. lam o. {o with setValue = v}),
    APRequired ()
  ],

  -- Backend
  APOption [
    APLong "target",
    APMetavar "PLATFORM",
    APValues ["native", "ocaml", "amd64", "llvm"],
    APDefault "native",
    APDescription "Specifies compilation backend. (Default: native)",
    APApplyVal (lam t. lam o. {o with target = t}),
    APPostCond (lam o. eqString o.mode "compile",
                "Can only specify target in compile mode."),
    APOnce ()
  ]
] in

let argspec = [
  APLongFlag ("ocaml", "Targets OCaml backend", lam o. {o with ocaml = true}),
  APFlag ('V', "verbose", "Increases verbosity", lam o. {o with verbosity = addi o.verbosity 1}),
  -- What I expect to have in the end:
  AP.Positional [
    AP.Name "mcore file",
    AP.ApplyVal (lam f. lam o. {o with mcfiles = cons f o.mcfiles}),
    AP.MatchOn (Str.isSuffix ".mc"),
    AP.Description "MCore input source files. (ends in .mc)"
  ],
  AP.Positional [
    AP.Name "ragnar file",
    AP.ApplyVal (lam f. lam o. {o with rgfiles = cons f o.rgfiles}),
    AP.MatchOn (Str.isSuffix ".rg"),
    AP.Description "Ragnar input source files. (ends in .rg)"
  ],
  AP.Positional [
    AP.Name "mode",
    AP.ApplyVal (lam m. lam o. {o with mode = m}),
    AP.Position 1,
    AP.Values ["compile", "eval", "test", "repl"],
    AP.Description "The mode to use the compiler with.",
    AP.ValueDescription ("compile", "Compiles the input source files."),
    AP.ValueDescription ("eval", "Evaluates the input source files"),
    AP.ValueDescription ("test", "Runs the utest statements in the input source files."),
    AP.ValueDescription ("repl", "Launches the MCore REPL.")
  ],
  AP.Option [
    AP.Long "target",
    AP.Metavar "PLATFORM",
    AP.Values ["native", "ocaml", "amd64", "llvm"],
    AP.Default "native",
    AP.Description "Specifies compilation backend. (Default: native)",
    AP.ApplyVal (lam t. lam o. {o with o.target = t}),
    AP.Cond (lam o. eqstr o.mode "compile",
             "Can only specify target when compiling."),
    AP.Once ()
  ],
  AP.Flag ('a', "all", "Runs everything", lam o. {o with o.all = true})
] in

let args = argparserParse argspec (tail argv) in



let parser: ArgParser TestArgs = argparserNew "test" defaults in

let parser = argparserAddOption 'h' "help"
                                "Prints usage and a help message."
                                (lam o. {o with help = true})
                                parser
in

let parser = argparserAddOption 'v' "version"
                                "Prints version and exits."
                                (lam o. {o with version = true})
                                parser
in

let parser = argparserAddLongOption "debug-parser"
                                    "Shows debug prints during parsing."
                                    (lam o. {o with debugParser = true})
                                    parser
in

let parser = argparserAddParamOption 'O' "optimization-level" "LEVEL"
                                     "Sets the optimization level."
                                     (lam p. lam o. {o with optLevel = string2int p})
                                     parser
in

let parser = argparserAddParamOption 'D' "define" "DEFINITION"
                                     "Add C preprocessor definition."
                                     (lam p. lam o. {o with defines = snoc o.defines p})
                                     parser
in

let parser = argparserAddLongParamOption "a-very-long-option-name" "fooparam"
                                         "Dummy option with a very long name."
                                         (lam _. lam o. o)
                                         parser
in

-- Used to test usage print with linewidth 80. Make sure this is commented out
-- when finished testing the usage print.
let _ = print (join ["\n", argparserUsage 80 parser, "\n"]) in

utest argparserParse [] parser with defaults in
utest argparserParse ["-h"] parser with {defaults with help = true} in
utest argparserParse ["--help"] parser with {defaults with help = true} in
utest argparserParse ["--debug-parser"] parser with {defaults with debugParser = true} in

utest argparserParse ["-hv"] parser with {{defaults with help = true}
                                                    with version = true} in
utest argparserParse ["-vh"] parser with {{defaults with help = true}
                                                    with version = true} in

utest argparserParse ["-v", "--help"] parser with {{defaults with help = true}
                                                             with version = true} in

utest argparserParse ["--optimization-level=2"] parser with {defaults with optLevel = 2} in
utest argparserParse ["--optimization-level", "71"] parser with {defaults with optLevel = 71} in
utest argparserParse ["-O=2"] parser with {defaults with optLevel = 2} in
utest argparserParse ["-O", "2"] parser with {defaults with optLevel = 2} in
utest argparserParse ["-O2"] parser with {defaults with optLevel = 2} in
utest argparserParse ["-O42"] parser with {defaults with optLevel = 42} in

utest argparserParse ["-vhO2"] parser with {{{defaults with help = true}
                                                       with version = true}
                                                       with optLevel = 2} in

utest argparserParse ["-vhO", "2"] parser with {{{defaults with help = true}
                                                           with version = true}
                                                           with optLevel = 2} in

utest argparserParse ["-DMCORE"] parser with {defaults with defines = ["MCORE"]} in

utest argparserParse ["-Dh", "--define", "TEST"] parser with {defaults with defines = ["h", "TEST"]} in

()