open Printf
open Array

external gpuhost_fun124_matrixMuliWorker: int array -> float array -> int array -> int array -> int array = "gpuhost_fun124_matrixMuliWorker"

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
    let fun62_matrixMki arg59_rows arg60_cols arg61_v =
        Array.make (( * ) (arg59_rows) (arg60_cols)) (arg61_v)
    in
    let fun68_matrixGeti arg63_row arg64_col arg65_m_rows arg66_m_cols arg67_m =
        Array.get (arg67_m) (( + ) (( * ) (arg66_m_cols) (arg63_row)) (arg64_col))
    in
    let fun78_seqInitFun arg77_f arg75_cols arg73_cols arg72_i =
        let var74_row  =
            ( / ) (arg72_i) (arg73_cols)
        in
        let var76_col  =
            ( mod ) (arg72_i) (arg75_cols)
        in
        arg77_f (var74_row) (var76_col)
    in
    let fun79_matrixIniti arg69_rows arg70_cols arg71_f =
        fun14_seqInit (( * ) (arg69_rows) (arg70_cols)) (fun78_seqInitFun (arg71_f) (arg70_cols) (arg70_cols))
    in
    let rec fun83_printrc arg92_m arg91_m_cols arg90_m_rows arg87_m_cols arg86_m_rows arg84_row arg85_col =
            if ( = ) (arg84_row) (arg86_m_rows) then
                [||]
            else
                let var88_next_col  =
                    ( mod ) (( + ) (arg85_col) (1)) (arg87_m_cols)
                in
                let var89_next_row  =
                    if ( = ) (var88_next_col) (0) then
                        ( + ) (arg84_row) (1)
                    else
                        arg84_row
                in
                fun22_strJoin ([||]) ([|fun19_int2string (fun68_matrixGeti (arg84_row) (arg85_col) (arg90_m_rows) (arg91_m_cols) (arg92_m)); if ( = ) (var88_next_col) (0) then
                    [|'\n'|]
                else
                    [|' '|]; fun83_printrc (arg92_m) (arg91_m_cols) (arg90_m_rows) (arg87_m_cols) (arg86_m_rows) (var89_next_row) (var88_next_col)|])
    in
    let fun93_matrix2stri arg80_m_rows arg81_m_cols arg82_m =
        fun83_printrc (arg82_m) (arg81_m_cols) (arg80_m_rows) (arg81_m_cols) (arg80_m_rows) (0) (0)
    in
    let rec fun97_printrc arg106_m arg105_m_cols arg104_m_rows arg101_m_cols arg100_m_rows arg98_row arg99_col =
            if ( = ) (arg98_row) (arg100_m_rows) then
                [||]
            else
                let var102_next_col  =
                    ( mod ) (( + ) (arg99_col) (1)) (arg101_m_cols)
                in
                let var103_next_row  =
                    if ( = ) (var102_next_col) (0) then
                        ( + ) (arg98_row) (1)
                    else
                        arg98_row
                in
                let var107__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun22_strJoin ([||]) ([|fun19_int2string (fun68_matrixGeti (arg98_row) (arg99_col) (arg104_m_rows) (arg105_m_cols) (arg106_m)); if ( = ) (var102_next_col) (0) then
                        [|'\n'|]
                    else
                        [|' '|]|]))
                in
                fun97_printrc (arg106_m) (arg105_m_cols) (arg104_m_rows) (arg101_m_cols) (arg100_m_rows) (var103_next_row) (var102_next_col)
    in
    let fun108_printMatrixi arg94_m_rows arg95_m_cols arg96_m =
        fun97_printrc (arg96_m) (arg95_m_cols) (arg94_m_rows) (arg95_m_cols) (arg94_m_rows) (0) (0)
    in
    let rec fun109_matrixMuliWorkerReduce arg110_innerDim arg111_b_cols arg112_a arg113_b arg114_acc arg115_p arg116_a_offset arg117_b_offset =
            if ( = ) (arg115_p) (arg110_innerDim) then
                arg114_acc
            else
                fun109_matrixMuliWorkerReduce (arg110_innerDim) (arg111_b_cols) (arg112_a) (arg113_b) (( + ) (arg114_acc) (( * ) (Array.get (arg112_a) (arg116_a_offset)) (Array.get (arg113_b) (arg117_b_offset)))) (( + ) (arg115_p) (1)) (( + ) (arg116_a_offset) (1)) (( + ) (arg117_b_offset) (arg111_b_cols))
    in
    let fun124_matrixMuliWorker arg118_innerDim arg119_a_rows arg120_b_cols arg121_a arg122_b arg123_idx =
        fun109_matrixMuliWorkerReduce (arg118_innerDim) (arg120_b_cols) (arg121_a) (arg122_b) (0) (0) (( * ) (arg118_innerDim) (( / ) (arg123_idx) (arg120_b_cols))) (( mod ) (arg123_idx) (arg120_b_cols))
    in
    let fun131_matrixMuli arg125_a_rows arg126_a_cols arg127_a arg128_b_rows arg129_b_cols arg130_b =
        if ( <> ) (arg126_a_cols) (arg128_b_rows) then
            (fun s -> printf "ERROR: %s
" (String.of_seq (Array.to_seq s)); exit 1) ([|'m'; 'a'; 't'; 'r'; 'i'; 'x'; 'M'; 'u'; 'l'; 'i'; ':'; ' '; 'I'; 'n'; 'n'; 'e'; 'r'; ' '; 'd'; 'i'; 'm'; 'e'; 'n'; 's'; 'i'; 'o'; 'n'; 's'; ' '; 'd'; 'i'; 'f'; 'f'; 'e'; 'r'; '.'|])
        else
            fun14_seqInit (( * ) (arg125_a_rows) (arg129_b_cols)) (fun124_matrixMuliWorker (arg126_a_cols) (arg125_a_rows) (arg129_b_cols) (arg127_a) (arg130_b))
    in
    let fun138_matrixMuliCUDA arg132_a_rows arg133_a_cols arg134_a arg135_b_rows arg136_b_cols arg137_b =
        if ( <> ) (arg133_a_cols) (arg135_b_rows) then
            (fun s -> printf "ERROR: %s
" (String.of_seq (Array.to_seq s)); exit 1) ([|'m'; 'a'; 't'; 'r'; 'i'; 'x'; 'M'; 'u'; 'l'; 'i'; 'C'; 'U'; 'D'; 'A'; ':'; ' '; 'I'; 'n'; 'n'; 'e'; 'r'; ' '; 'd'; 'i'; 'm'; 'e'; 'n'; 's'; 'i'; 'o'; 'n'; 's'; ' '; 'd'; 'i'; 'f'; 'f'; 'e'; 'r'; '.'|])
        else
            gpuhost_fun124_matrixMuliWorker [|32; arg133_a_cols; arg132_a_rows; arg136_b_cols; ( * ) (arg132_a_rows) (arg136_b_cols)|] [||] (arg134_a) (arg137_b)
    in
    let fun141_matAinitfun arg139_row arg140_col =
        ( + ) (( * ) (arg139_row) (arg139_row)) (arg140_col)
    in
    let fun144_matBinitfun arg142_row arg143_col =
        ( mod ) (( / ) (( * ) (( + ) (arg142_row) (19)) (17)) (( + ) (arg143_col) (13))) (( + ) (arg142_row) (11))
    in
    let fun147_matAinitfun_v2 arg145_row arg146_col =
        ( - ) (( mod ) (( + ) (( * ) (arg145_row) (arg145_row)) (arg146_col)) (3)) (1)
    in
    let fun150_matBinitfun_v2 arg148_row arg149_col =
        ( mod ) (( / ) (( * ) (( + ) (arg148_row) (19)) (17)) (( + ) (arg149_col) (13))) (2)
    in
    let var151_matA_rows  =
        1024
    in
    let var152_matA_cols  =
        1024
    in
    let var153_matA  =
        fun79_matrixIniti (var151_matA_rows) (var152_matA_cols) (fun147_matAinitfun_v2)
    in
    let var154_matB_rows  =
        1024
    in
    let var155_matB_cols  =
        1024
    in
    let var156_matB  =
        fun79_matrixIniti (var154_matB_rows) (var155_matB_cols) (fun150_matBinitfun_v2)
    in
    let var157_matAxB  =
        gpuhost_fun124_matrixMuliWorker [|64; var152_matA_cols; var151_matA_rows; var155_matB_cols; ( * ) (var151_matA_rows) (var155_matB_cols)|] [||] (var153_matA) (var156_matB)
    in
    let var158_matAxB_rows  =
        var151_matA_rows
    in
    let var159_matAxB_cols  =
        var155_matB_cols
    in
    ()