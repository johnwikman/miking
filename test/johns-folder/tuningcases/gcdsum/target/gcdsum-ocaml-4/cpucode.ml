open Array
open Printf



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
    let rec fun31_printloop arg33_arr arg32_i =
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
                let var37__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun19_int2string (Array.get (arg33_arr) (arg32_i)))
                in
                let var38__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
                in
                fun31_printloop (arg33_arr) (( + ) (arg32_i) (1))
    in
    let fun42_printintarr arg29_name arg30_arr =
        let var39__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'C'; 'o'; 'n'; 't'; 'e'; 'n'; 't'; 's'; ' '; 'o'; 'f'; ' '|])
        in
        let var40__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (arg29_name)
        in
        let var41__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|':'; '\n'|])
        in
        fun31_printloop (arg30_arr) (0)
    in
    let rec fun45_printloop arg47_arr arg46_i =
            if ( = ) (arg46_i) (Array.length (arg47_arr)) then
                ()
            else
                let var48__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; ' '; ' '; ' '|])
                in
                let var49__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun19_int2string (arg46_i))
                in
                let var50__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|':'; ' '|])
                in
                let var51__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (Array.get (arg47_arr) (arg46_i)))
                in
                let var52__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
                in
                fun45_printloop (arg47_arr) (( + ) (arg46_i) (1))
    in
    let fun56_printfloatarr arg43_name arg44_arr =
        let var53__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'C'; 'o'; 'n'; 't'; 'e'; 'n'; 't'; 's'; ' '; 'o'; 'f'; ' '|])
        in
        let var54__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (arg43_name)
        in
        let var55__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|':'; '\n'|])
        in
        fun45_printloop (arg44_arr) (0)
    in
    let rec fun58_gcd arg59_a arg60_b =
            if ( = ) (arg60_b) (0) then
                arg59_a
            else
                fun58_gcd (arg60_b) (( mod ) (arg59_a) (arg60_b))
    in
    let rec fun64_recloop arg69_yvec arg68_x arg67_n arg65_acc arg66_i =
            if ( = ) (arg66_i) (arg67_n) then
                arg65_acc
            else
                fun64_recloop (arg69_yvec) (arg68_x) (arg67_n) (( + ) (arg65_acc) (fun58_gcd (arg68_x) (Array.get (arg69_yvec) (arg66_i)))) (( + ) (arg66_i) (1))
    in
    let fun70_gcdsum arg61_yvec arg62_n arg63_x =
        fun64_recloop (arg61_yvec) (arg63_x) (arg62_n) (0) (0)
    in
    let fun72_vecXinitfun arg71_i =
        ( mod ) (( * ) (( - ) (51897342) (arg71_i)) (( + ) (13) (arg71_i))) (74)
    in
    let fun74_vecYinitfun arg73_i =
        ( mod ) (( * ) (( - ) (923487321) (arg73_i)) (( + ) (1) (arg73_i))) (53)
    in
    let rec fun79_printloop arg82_vec arg81_size arg80_i =
            if ( = ) (arg80_i) (arg81_size) then
                ()
            else
                let var83__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun19_int2string (Array.get (arg82_vec) (arg80_i)))
                in
                let var84__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '|])
                in
                fun79_printloop (arg82_vec) (arg81_size) (( + ) (arg80_i) (1))
    in
    let fun86_printVec arg77_size arg78_vec =
        let var85__  =
            fun79_printloop (arg78_vec) (arg77_size) (0)
        in
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
    in
    let var57_vecsize  =
        4
    in
    let var75_vecX  =
        fun14_seqInit (var57_vecsize) (fun72_vecXinitfun)
    in
    let var76_vecY  =
        fun14_seqInit (var57_vecsize) (fun74_vecYinitfun)
    in
    let var87_vecS  =
        fun8_map (fun70_gcdsum (var76_vecY) (var57_vecsize)) (var75_vecX)
    in
    let var88__  =
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; 'v'; 'e'; 'c'; 'X'; ':'; '\n'|])
    in
    let var89__  =
        fun86_printVec (var57_vecsize) (var75_vecX)
    in
    let var90__  =
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; 'v'; 'e'; 'c'; 'Y'; ':'; '\n'|])
    in
    let var91__  =
        fun86_printVec (var57_vecsize) (var76_vecY)
    in
    let var92__  =
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; 'v'; 'e'; 'c'; 'S'; ':'; '\n'|])
    in
    let var93__  =
        fun86_printVec (var57_vecsize) (var87_vecS)
    in
    ()