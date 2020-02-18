
mexpr
recursive let factorial = lam n.
	if eqi n 0 then 1 else (muli n (factorial (subi n 1)))
in

utest factorial 3 with 6 in
utest factorial 4 with 24 in
utest factorial 5 with 120 in


--TmRecLets ([("factorial",None ,TmLam ("n",None ,TmIf (TmApp (TmApp (TmVar "eqi",TmVar "n"),TmConst (CInt 0)),TmConst (CInt 1),TmApp (TmVar "factorial",TmApp (TmApp (TmVar "subi",TmVar "n"),TmConst (CInt 1))))))],TmUtest (TmApp (TmVar "factorial",TmConst (CInt 3)),TmConst (CInt 6),TmUtest (TmApp (TmVar "factorial",TmConst (CInt 4)),TmConst (CInt 24),TmUtest (TmApp (TmVar "factorial",TmConst (CInt 5)),TmConst (CInt 120),TmConst (CUnit )))))


()
