open Printf
open Array

external gpuhost_fun81_saxpy: int array -> float array -> float array -> float array = "gpuhost_fun81_saxpy"

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
    let rec fun59_printloop arg62_vec arg61_size arg60_i =
            if ( = ) (arg60_i) (arg61_size) then
                ()
            else
                let var63__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun19_int2string (Array.get (arg62_vec) (arg60_i)))
                in
                let var64__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '|])
                in
                fun59_printloop (arg62_vec) (arg61_size) (( + ) (arg60_i) (1))
    in
    let fun66_printSeqi arg57_size arg58_vec =
        let var65__  =
            fun59_printloop (arg58_vec) (arg57_size) (0)
        in
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
    in
    let rec fun69_printloop arg72_vec arg71_size arg70_i =
            if ( = ) (arg70_i) (arg71_size) then
                ()
            else
                let var73__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (Array.get (arg72_vec) (arg70_i)))
                in
                let var74__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '|])
                in
                fun69_printloop (arg72_vec) (arg71_size) (( + ) (arg70_i) (1))
    in
    let fun76_printSeqf arg67_size arg68_vec =
        let var75__  =
            fun69_printloop (arg68_vec) (arg67_size) (0)
        in
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
    in
    let fun81_saxpy arg78_a arg79_y arg80_xelem =
        ( +. ) (( *. ) (arg78_a) (arg80_xelem)) (arg79_y)
    in
    let fun83_vecXinitfun arg82_i =
        float_of_int (( mod ) (( * ) (( * ) (arg82_i) (arg82_i)) (( - ) (500000000) (arg82_i))) (5073))
    in
    let var77_vecsize  =
        10000000
    in
    let var84_scalarA  =
        2.389999e+2
    in
    let var85_vecX  =
        fun14_seqInit (var77_vecsize) (fun83_vecXinitfun)
    in
    let var86_scalarY  =
        1.90e+1
    in
    let var87_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun81_saxpy (var84_scalarA) (var86_scalarY)) (var85_vecX)) else (gpuhost_fun81_saxpy [|1|] [|var84_scalarA; var86_scalarY|] (var85_vecX))
    in
    let var88_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun81_saxpy (var84_scalarA) (var86_scalarY)) (var85_vecX)) else (gpuhost_fun81_saxpy [|1|] [|var84_scalarA; var86_scalarY|] (var85_vecX))
    in
    let var89_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun81_saxpy (var84_scalarA) (var86_scalarY)) (var85_vecX)) else (gpuhost_fun81_saxpy [|1|] [|var84_scalarA; var86_scalarY|] (var85_vecX))
    in
    let var90_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun81_saxpy (var84_scalarA) (var86_scalarY)) (var85_vecX)) else (gpuhost_fun81_saxpy [|1|] [|var84_scalarA; var86_scalarY|] (var85_vecX))
    in
    let var91_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun81_saxpy (var84_scalarA) (var86_scalarY)) (var85_vecX)) else (gpuhost_fun81_saxpy [|1|] [|var84_scalarA; var86_scalarY|] (var85_vecX))
    in
    let var92_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun81_saxpy (var84_scalarA) (var86_scalarY)) (var85_vecX)) else (gpuhost_fun81_saxpy [|1|] [|var84_scalarA; var86_scalarY|] (var85_vecX))
    in
    let var93_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun81_saxpy (var84_scalarA) (var86_scalarY)) (var85_vecX)) else (gpuhost_fun81_saxpy [|1|] [|var84_scalarA; var86_scalarY|] (var85_vecX))
    in
    let var94_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun81_saxpy (var84_scalarA) (var86_scalarY)) (var85_vecX)) else (gpuhost_fun81_saxpy [|1|] [|var84_scalarA; var86_scalarY|] (var85_vecX))
    in
    let var95_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun81_saxpy (var84_scalarA) (var86_scalarY)) (var85_vecX)) else (gpuhost_fun81_saxpy [|1|] [|var84_scalarA; var86_scalarY|] (var85_vecX))
    in
    let var96_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var85_vecX)) (( + ) (50) (16))) (( + ) (( + ) (( * ) (( + ) (Array.length (var85_vecX)) (Array.length (var85_vecX))) (10)) (40000000)) (( * ) (59) (( / ) (( + ) (Array.length (var85_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun81_saxpy (var84_scalarA) (var86_scalarY)) (var85_vecX)) else (gpuhost_fun81_saxpy [|1|] [|var84_scalarA; var86_scalarY|] (var85_vecX))
    in
    ()