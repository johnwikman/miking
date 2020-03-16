open Printf
open Array

external gpuhost_fun76_factidx: int array -> float array -> int array -> int array = "gpuhost_fun76_factidx"

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
    let fun60_id arg59_x =
        arg59_x
    in
    let rec fun61_factorial arg62_n =
            if ( = ) (arg62_n) (0) then
                1
            else
                ( * ) (arg62_n) (fun61_factorial (( - ) (arg62_n) (1)))
    in
    let rec fun63_fib_helper arg64_i arg65_n arg66_prev arg67_current =
            if ( = ) (arg64_i) (arg65_n) then
                arg67_current
            else
                fun63_fib_helper (( + ) (arg64_i) (1)) (arg65_n) (arg67_current) (( + ) (arg66_prev) (arg67_current))
    in
    let fun69_fib arg68_n =
        fun63_fib_helper (0) (arg68_n) (1) (0)
    in
    let fun76_factidx arg74_i arg75_ignored_arg =
        fun61_factorial (arg74_i)
    in
    let var70_v  =
        10
    in
    let var71_res  =
        fun61_factorial (var70_v)
    in
    let var72_printstr  =
        fun22_strJoin ([||]) ([|[|'f'; 'a'; 'c'; 't'; 'o'; 'r'; 'i'; 'a'; 'l'; ' '|]; fun19_int2string (var70_v); [|' '; '='; ' '|]; fun19_int2string (var71_res); [|'\n'|]|])
    in
    let var73__  =
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (var72_printstr)
    in
    let var77_res  =
        gpuhost_fun76_factidx [|8|] [||] (Array.make (16) (0))
    in
    let var78__  =
        fun43_printintarr ([|'c'; 'u'; 'd'; 'a'; 'M'; 'a'; 'p'; 'i'; ' '; 'f'; 'a'; 'c'; 't'; 'i'; 'd'; 'x'; ' '; 'r'; 'e'; 's'; 'u'; 'l'; 't'|]) (var77_res)
    in
    ()