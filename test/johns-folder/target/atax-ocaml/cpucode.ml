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
    let rec fun119_dotprod arg127_b_cols arg126_b arg125_a arg124_innerDim arg120_acc arg121_p arg122_a_offset arg123_b_offset =
            if ( = ) (arg121_p) (arg124_innerDim) then
                arg120_acc
            else
                fun119_dotprod (arg127_b_cols) (arg126_b) (arg125_a) (arg124_innerDim) (( + ) (arg120_acc) (( * ) (Array.get (arg125_a) (arg122_a_offset)) (Array.get (arg126_b) (arg123_b_offset)))) (( + ) (arg121_p) (1)) (( + ) (arg122_a_offset) (1)) (( + ) (arg123_b_offset) (arg127_b_cols))
    in
    let fun128_matrixMuliWorker arg109_innerDim arg110_a_rows arg111_b_cols arg112_a arg113_b arg114_idx =
        let var115_row  =
            ( / ) (arg114_idx) (arg111_b_cols)
        in
        let var116_col  =
            ( mod ) (arg114_idx) (arg111_b_cols)
        in
        let var117_a_start_offset  =
            ( * ) (arg109_innerDim) (var115_row)
        in
        let var118_b_start_offset  =
            var116_col
        in
        fun119_dotprod (arg111_b_cols) (arg113_b) (arg112_a) (arg109_innerDim) (0) (0) (var117_a_start_offset) (var118_b_start_offset)
    in
    let rec fun139_dotprod arg148_outerDim arg147_outerDim arg146_a arg145_a arg144_innerDim arg140_acc arg141_p arg142_aT_offset arg143_a_offset =
            if ( = ) (arg141_p) (arg144_innerDim) then
                arg140_acc
            else
                fun139_dotprod (arg148_outerDim) (arg147_outerDim) (arg146_a) (arg145_a) (arg144_innerDim) (( + ) (arg140_acc) (( * ) (Array.get (arg145_a) (arg142_aT_offset)) (Array.get (arg146_a) (arg143_a_offset)))) (( + ) (arg141_p) (1)) (( + ) (arg142_aT_offset) (arg147_outerDim)) (( + ) (arg143_a_offset) (arg148_outerDim))
    in
    let fun149_matrixATAWorker arg129_rows arg130_cols arg131_a arg132_idx =
        let var133_innerDim  =
            arg129_rows
        in
        let var134_outerDim  =
            arg130_cols
        in
        let var135_row  =
            ( / ) (arg132_idx) (arg130_cols)
        in
        let var136_col  =
            ( mod ) (arg132_idx) (arg130_cols)
        in
        let var137_aT_start_offset  =
            var135_row
        in
        let var138_a_start_offset  =
            var136_col
        in
        fun139_dotprod (var134_outerDim) (var134_outerDim) (arg131_a) (arg131_a) (var133_innerDim) (0) (0) (var137_aT_start_offset) (var138_a_start_offset)
    in
    let var150_matA  =
        [|1; 3; 5; 2; 4; 6|]
    in
    let var151_matA_rows  =
        2
    in
    let var152_matA_cols  =
        3
    in
    let var153_vecX  =
        [|11; 31; 17|]
    in
    let var154_vecX_rows  =
        3
    in
    let var155_vecX_cols  =
        1
    in
    let var156__  =
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; 'm'; 'a'; 't'; 'A'; ':'; '\n'|])
    in
    let var157__  =
        fun108_printMatrixi (var151_matA_rows) (var152_matA_cols) (var150_matA)
    in
    let var158__  =
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; 'v'; 'e'; 'c'; 'X'; ':'; '\n'|])
    in
    let var159__  =
        fun108_printMatrixi (var154_vecX_rows) (var155_vecX_cols) (var153_vecX)
    in
    let var160_matATA  =
        fun14_seqInit (( * ) (var152_matA_cols) (var152_matA_cols)) (fun149_matrixATAWorker (var151_matA_rows) (var152_matA_cols) (var150_matA))
    in
    let var161_matATA_rows  =
        var152_matA_cols
    in
    let var162_matATA_cols  =
        var152_matA_cols
    in
    let var163__  =
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; 'm'; 'a'; 't'; 'A'; 'T'; 'A'; ':'; '\n'|])
    in
    let var164__  =
        fun108_printMatrixi (var161_matATA_rows) (var162_matATA_cols) (var160_matATA)
    in
    let var165_matATAx  =
        fun14_seqInit (( * ) (var161_matATA_rows) (var155_vecX_cols)) (fun128_matrixMuliWorker (var162_matATA_cols) (var161_matATA_rows) (var155_vecX_cols) (var160_matATA) (var153_vecX))
    in
    let var166_matATAx_rows  =
        var161_matATA_rows
    in
    let var167_matATAx_cols  =
        var155_vecX_cols
    in
    let var168__  =
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; 'm'; 'a'; 't'; 'A'; 'T'; 'A'; 'x'; ':'; '\n'|])
    in
    let var169__  =
        fun108_printMatrixi (var166_matATAx_rows) (var167_matATAx_cols) (var165_matATAx)
    in
    ()