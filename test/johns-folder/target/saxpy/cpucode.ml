open Printf
open Array

external gpuhost_fun62_saxpy_int: int array -> float array -> int array -> int array = "gpuhost_fun62_saxpy_int"
external gpuhost_fun77_id2f_ignore2nd: int array -> float array -> int array -> float array = "gpuhost_fun77_id2f_ignore2nd"
external gpuhost_fun66_saxpy_float: int array -> float array -> float array -> float array = "gpuhost_fun66_saxpy_float"
external gpuhost_fun71_saxpy_intseq: int array -> float array -> int array -> int array -> int array = "gpuhost_fun71_saxpy_intseq"

let main =
    let fun1_head arg0_s =
        Array.get (arg0_s) (0)
    in
    let fun3_tail arg2_s =
        (fun xs start len -> Array.sub xs (min ((Array.length xs) - 1) start) (min ((Array.length xs) - start) len)) (arg2_s) (1) (Array.length (arg2_s))
    in
    let fun5_null arg4_l =
        ( = ) (Array.length (arg4_l)) (0)
    in
    let fun8_map arg6_f arg7_seq =
        Array.map (arg6_f) (arg7_seq)
    in
    let fun11_mapi arg9_f arg10_seq =
        Array.mapi (arg9_f) (arg10_seq)
    in
    let fun14_seqInit arg12_size arg13_f =
        Array.init (arg12_size) (arg13_f)
    in
    let rec fun16_int2string_rechelper arg17_n =
            if ( < ) (arg17_n) (10) then
                [|char_of_int (( + ) (arg17_n) (int_of_char ('0')))|]
            else
                let var18_d  =
                    [|char_of_int (( + ) (( mod ) (arg17_n) (10)) (int_of_char ('0')))|]
                in
                Array.append (fun16_int2string_rechelper (( / ) (arg17_n) (10))) (var18_d)
    in
    let fun19_int2string arg15_n =
        if ( < ) (arg15_n) (0) then
            (fun x xs -> Array.append [|x|] xs) ('-') (fun16_int2string_rechelper (( ~- ) (arg15_n)))
        else
            fun16_int2string_rechelper (arg15_n)
    in
    let fun21_float2string arg20_f =
        Array.of_seq (String.to_seq (string_of_float (arg20_f)))
    in
    let rec fun22_strJoin arg23_delim arg24_strs =
            if ( = ) (Array.length (arg24_strs)) (0) then
                [||]
            else
                if ( = ) (Array.length (arg24_strs)) (1) then
                    fun1_head (arg24_strs)
                else
                    Array.append (Array.append (fun1_head (arg24_strs)) (arg23_delim)) (fun22_strJoin (arg23_delim) (fun3_tail (arg24_strs)))
    in
    let fun26_printint arg25_i =
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun19_int2string (arg25_i))
    in
    let fun28_printintln arg27_i =
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (Array.append (fun19_int2string (arg27_i)) ([|'\n'|]))
    in
    let rec fun31_printloop arg37_arr arg33_arr arg32_i =
            if ( = ) (arg32_i) (Array.length (arg33_arr)) then
                ()
            else
                let var34__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; ' '; ' '; ' '|])
                in
                let var35__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun19_int2string (arg32_i))
                in
                let var36__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|':'; ' '|])
                in
                let var38__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun19_int2string (Array.get (arg37_arr) (arg32_i)))
                in
                let var39__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
                in
                fun31_printloop (arg37_arr) (arg33_arr) (( + ) (arg32_i) (1))
    in
    let fun43_printintarr arg29_name arg30_arr =
        let var40__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'C'; 'o'; 'n'; 't'; 'e'; 'n'; 't'; 's'; ' '; 'o'; 'f'; ' '|])
        in
        let var41__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (arg29_name)
        in
        let var42__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|':'; '\n'|])
        in
        fun31_printloop (arg30_arr) (arg30_arr) (0)
    in
    let rec fun46_printloop arg52_arr arg48_arr arg47_i =
            if ( = ) (arg47_i) (Array.length (arg48_arr)) then
                ()
            else
                let var49__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; ' '; ' '; ' '|])
                in
                let var50__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun19_int2string (arg47_i))
                in
                let var51__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|':'; ' '|])
                in
                let var53__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (Array.get (arg52_arr) (arg47_i)))
                in
                let var54__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
                in
                fun46_printloop (arg52_arr) (arg48_arr) (( + ) (arg47_i) (1))
    in
    let fun58_printfloatarr arg44_name arg45_arr =
        let var55__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'C'; 'o'; 'n'; 't'; 'e'; 'n'; 't'; 's'; ' '; 'o'; 'f'; ' '|])
        in
        let var56__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (arg44_name)
        in
        let var57__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|':'; '\n'|])
        in
        fun46_printloop (arg45_arr) (arg45_arr) (0)
    in
    let fun62_saxpy_int arg59_x arg60_y arg61_a =
        ( + ) (( * ) (arg61_a) (arg59_x)) (arg60_y)
    in
    let fun66_saxpy_float arg63_x arg64_y arg65_a =
        ( +. ) (( *. ) (arg65_a) (arg63_x)) (arg64_y)
    in
    let fun71_saxpy_intseq arg67_a arg68_y arg69_i arg70_x =
        ( + ) (( * ) (arg67_a) (arg70_x)) (Array.get (arg68_y) (arg69_i))
    in
    let fun74_id_ignore2nd arg72_x arg73_y =
        arg72_x
    in
    let fun77_id2f_ignore2nd arg75_x arg76_y =
        float_of_int (arg75_x)
    in
    let var78_res  =
        gpuhost_fun62_saxpy_int [|32; 17; 11|] [||] ([|15; 1|])
    in
    let var79__  =
        fun43_printintarr ([|'s'; 'a'; 'x'; 'p'; 'y'; ' '; '1'; '7'; ' '; '1'; '1'; ' '; '['; '1'; '5'; ','; ' '; '1'; ']'; ' '; 'r'; 'e'; 's'; 'u'; 'l'; 't'|]) (var78_res)
    in
    let var80_res  =
        gpuhost_fun77_id2f_ignore2nd [|512|] [||] (Array.make (5000) (0))
    in
    let var81_res  =
        gpuhost_fun66_saxpy_float [|512|] [|1.70e+1; 1.100000e+1|] (var80_res)
    in
    let var82_res  =
        gpuhost_fun71_saxpy_intseq [|32; 17|] [||] ([|9; 8|]) ([|15; 1|])
    in
    let var83__  =
        fun43_printintarr ([|'s'; 'a'; 'x'; 'p'; 'y'; '_'; 'i'; 'n'; 't'; 's'; 'e'; 'q'; ' '; '('; 'a'; ':'; ' '; '1'; '7'; ')'; ' '; '('; 'x'; ':'; ' '; '['; '1'; '5'; ','; ' '; '1'; ']'; ')'; ' '; '('; 'y'; ':'; ' '; '['; '9'; ','; ' '; '8'; ']'; ')'; ' '; 'r'; 'e'; 's'; 'u'; 'l'; 't'|]) (var82_res)
    in
    ()