open Printf
open Array

external gpuhost_fun90_gcdsum: int array -> float array -> int array -> int array -> int array = "gpuhost_fun90_gcdsum"

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
    let rec fun78_gcd arg79_a arg80_b =
            if ( = ) (arg80_b) (0) then
                arg79_a
            else
                fun78_gcd (arg80_b) (( mod ) (arg79_a) (arg80_b))
    in
    let rec fun84_recloop arg89_yvec arg88_x arg87_n arg85_acc arg86_i =
            if ( = ) (arg86_i) (arg87_n) then
                arg85_acc
            else
                fun84_recloop (arg89_yvec) (arg88_x) (arg87_n) (( + ) (arg85_acc) (fun78_gcd (arg88_x) (Array.get (arg89_yvec) (arg86_i)))) (( + ) (arg86_i) (1))
    in
    let fun90_gcdsum arg81_yvec arg82_n arg83_x =
        fun84_recloop (arg81_yvec) (arg83_x) (arg82_n) (0) (0)
    in
    let fun92_vecXinitfun arg91_i =
        ( mod ) (( * ) (( - ) (51897342) (arg91_i)) (( + ) (1) (arg91_i))) (74)
    in
    let fun94_vecYinitfun arg93_i =
        ( mod ) (( * ) (( - ) (923487321) (arg93_i)) (( + ) (1) (arg93_i))) (53)
    in
    let var77_vecsize  =
        512
    in
    let var95_vecX  =
        fun14_seqInit (var77_vecsize) (fun92_vecXinitfun)
    in
    let var96_vecY  =
        fun14_seqInit (var77_vecsize) (fun94_vecYinitfun)
    in
    let var97_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var98_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var99_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var100_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var101_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var102_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var103_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var104_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var105_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var106_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var107_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var108_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var109_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var110_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var111_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var112_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var113_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var114_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var115_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var116_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var117_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var118_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var119_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var120_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var121_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var122_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var123_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var124_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var125_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var126_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var127_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var128_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var129_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var130_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var131_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var132_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var133_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var134_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var135_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var136_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var137_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var138_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var139_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var140_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var141_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var142_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var143_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var144_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var145_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    let var146_vecS  =
        let _ = printf "ocamlcost: %d\n" (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) in let _ = printf "cudacost: %d\n" (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536)))) in if (( < ) (( * ) (Array.length (var95_vecX)) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (9) (if ( < ) (1) (( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (37) (( + ) (6) (( + ) (1) (( + ) (1) (( + ) (15) (( + ) (0) (( * ) (51) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var95_vecX)) (Array.length (var96_vecY))) (Array.length (var95_vecX))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (10) (( + ) (0) (( * ) (( * ) (1) (if ( < ) (if ( < ) (1) (var77_vecsize) then
            var77_vecsize
        else
            1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            if ( < ) (1) (var77_vecsize) then
                var77_vecsize
            else
                1)) (( + ) (86) (if ( < ) (1) (( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
            Array.get (var96_vecY) (0)
        else
            1)))))))) then
            ( + ) (205) (( + ) (17) (( + ) (12) (( + ) (1) (( + ) (305) (( + ) (0) (( * ) (353) (if ( < ) (1) (Array.get (var96_vecY) (0)) then
                Array.get (var96_vecY) (0)
            else
                1)))))))
        else
            1))))))) (( / ) (( + ) (Array.length (var95_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun90_gcdsum (var96_vecY) (var77_vecsize)) (var95_vecX)) else (gpuhost_fun90_gcdsum [|1; var77_vecsize|] [||] (var96_vecY) (var95_vecX))
    in
    ()