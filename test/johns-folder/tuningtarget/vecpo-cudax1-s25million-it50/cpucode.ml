open Array
open Printf

external gpuhost_fun69_plusone: int array -> float array -> int array -> int array = "gpuhost_fun69_plusone"

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
    let fun66_printVec arg57_size arg58_vec =
        let var65__  =
            fun59_printloop (arg58_vec) (arg57_size) (0)
        in
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
    in
    let fun69_plusone arg68_x =
        ( + ) (arg68_x) (1)
    in
    let fun71_vecXinitfun arg70_i =
        arg70_i
    in
    let var67_vecsize  =
        25000000
    in
    let var72_vecX  =
        fun14_seqInit (var67_vecsize) (fun71_vecXinitfun)
    in
    let var73_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var74_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var75_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var76_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var77_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var78_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var79_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var80_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var81_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var82_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var83_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var84_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var85_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var86_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var87_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var88_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var89_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var90_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var91_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var92_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var93_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var94_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var95_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var96_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var97_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var98_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var99_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var100_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var101_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var102_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var103_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var104_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var105_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var106_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var107_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var108_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var109_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var110_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var111_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var112_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var113_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var114_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var115_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var116_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var117_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var118_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var119_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var120_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var121_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    let var122_vecS  =
        gpuhost_fun69_plusone [|1|] [||] (var72_vecX)
    in
    ()