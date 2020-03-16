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
    let rec fun115_dotprod arg123_b_cols arg122_b arg121_a arg120_innerDim arg116_acc arg117_p arg118_a_offset arg119_b_offset =
            if ( = ) (arg117_p) (arg120_innerDim) then
                arg116_acc
            else
                fun115_dotprod (arg123_b_cols) (arg122_b) (arg121_a) (arg120_innerDim) (( + ) (arg116_acc) (( * ) (Array.get (arg121_a) (arg118_a_offset)) (Array.get (arg122_b) (arg119_b_offset)))) (( + ) (arg117_p) (1)) (( + ) (arg118_a_offset) (1)) (( + ) (arg119_b_offset) (arg123_b_cols))
    in
    let fun124_matrixMuliWorker arg109_innerDim arg110_a_rows arg111_b_cols arg112_a arg113_b arg114_idx =
        fun115_dotprod (arg111_b_cols) (arg113_b) (arg112_a) (arg109_innerDim) (0) (0) (( * ) (arg109_innerDim) (( / ) (arg114_idx) (arg111_b_cols))) (( mod ) (arg114_idx) (arg111_b_cols))
    in
    let rec fun129_dotprod arg138_cols arg137_cols arg136_a arg135_a arg134_rows arg130_acc arg131_p arg132_aT_offset arg133_a_offset =
            if ( = ) (arg131_p) (arg134_rows) then
                arg130_acc
            else
                fun129_dotprod (arg138_cols) (arg137_cols) (arg136_a) (arg135_a) (arg134_rows) (( + ) (arg130_acc) (( * ) (Array.get (arg135_a) (arg132_aT_offset)) (Array.get (arg136_a) (arg133_a_offset)))) (( + ) (arg131_p) (1)) (( + ) (arg132_aT_offset) (arg137_cols)) (( + ) (arg133_a_offset) (arg138_cols))
    in
    let fun139_matrixATAWorker arg125_rows arg126_cols arg127_a arg128_idx =
        fun129_dotprod (arg126_cols) (arg126_cols) (arg127_a) (arg127_a) (arg125_rows) (0) (0) (( / ) (arg128_idx) (arg126_cols)) (( mod ) (arg128_idx) (arg126_cols))
    in
    let fun142_matAinitfun arg140_row arg141_col =
        ( + ) (( * ) (arg140_row) (arg140_row)) (arg141_col)
    in
    let fun145_matBinitfun arg143_row arg144_col =
        ( mod ) (( / ) (( * ) (( + ) (arg143_row) (19)) (17)) (( + ) (arg144_col) (13))) (( + ) (arg143_row) (11))
    in
    let fun148_matAinitfun_v2 arg146_row arg147_col =
        ( - ) (( mod ) (( + ) (( * ) (arg146_row) (arg146_row)) (arg147_col)) (3)) (1)
    in
    let fun151_matBinitfun_v2 arg149_row arg150_col =
        ( mod ) (( / ) (( * ) (( + ) (arg149_row) (19)) (17)) (( + ) (arg150_col) (13))) (2)
    in
    let var152_matA_rows  =
        1024
    in
    let var153_matA_cols  =
        1024
    in
    let var154_matA  =
        fun79_matrixIniti (var152_matA_rows) (var153_matA_cols) (fun148_matAinitfun_v2)
    in
    let var155_matB_rows  =
        1024
    in
    let var156_matB_cols  =
        1024
    in
    let var157_matB  =
        fun79_matrixIniti (var155_matB_rows) (var156_matB_cols) (fun151_matBinitfun_v2)
    in
    let var158_innerDim  =
        var153_matA_cols
    in
    let var159_matAxB  =
        fun14_seqInit (( * ) (var152_matA_rows) (var156_matB_cols)) (fun124_matrixMuliWorker (var158_innerDim) (var152_matA_rows) (var156_matB_cols) (var154_matA) (var157_matB))
    in
    let var160_matAxB_rows  =
        var152_matA_rows
    in
    let var161_matAxB_cols  =
        var156_matB_cols
    in
    let var162__  =
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; 'm'; 'a'; 't'; 'A'; 'x'; 'B'; ':'; '\n'|])
    in
    let var163__  =
        fun108_printMatrixi (var160_matAxB_rows) (var161_matAxB_cols) (var159_matAxB)
    in
    ()