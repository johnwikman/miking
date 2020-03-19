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
    let fun60_matrixMki arg57_rows arg58_cols arg59_v =
        Array.make (( * ) (arg57_rows) (arg58_cols)) (arg59_v)
    in
    let fun66_matrixGeti arg61_row arg62_col arg63_m_rows arg64_m_cols arg65_m =
        Array.get (arg65_m) (( + ) (( * ) (arg64_m_cols) (arg61_row)) (arg62_col))
    in
    let fun75_seqInitFun arg74_f arg71_cols arg70_i =
        let var72_row  =
            ( / ) (arg70_i) (arg71_cols)
        in
        let var73_col  =
            ( mod ) (arg70_i) (arg71_cols)
        in
        arg74_f (var72_row) (var73_col)
    in
    let fun76_matrixIniti arg67_rows arg68_cols arg69_f =
        fun14_seqInit (( * ) (arg67_rows) (arg68_cols)) (fun75_seqInitFun (arg69_f) (arg68_cols))
    in
    let rec fun80_printrc arg87_m arg84_m_cols arg83_m_rows arg81_row arg82_col =
            if ( = ) (arg81_row) (arg83_m_rows) then
                [||]
            else
                let var85_next_col  =
                    ( mod ) (( + ) (arg82_col) (1)) (arg84_m_cols)
                in
                let var86_next_row  =
                    if ( = ) (var85_next_col) (0) then
                        ( + ) (arg81_row) (1)
                    else
                        arg81_row
                in
                fun22_strJoin ([||]) ([|fun19_int2string (fun66_matrixGeti (arg81_row) (arg82_col) (arg83_m_rows) (arg84_m_cols) (arg87_m)); if ( = ) (var85_next_col) (0) then
                    [|'\n'|]
                else
                    [|' '|]; fun80_printrc (arg87_m) (arg84_m_cols) (arg83_m_rows) (var86_next_row) (var85_next_col)|])
    in
    let fun88_matrix2stri arg77_m_rows arg78_m_cols arg79_m =
        fun80_printrc (arg79_m) (arg78_m_cols) (arg77_m_rows) (0) (0)
    in
    let rec fun92_printrc arg99_m arg96_m_cols arg95_m_rows arg93_row arg94_col =
            if ( = ) (arg93_row) (arg95_m_rows) then
                [||]
            else
                let var97_next_col  =
                    ( mod ) (( + ) (arg94_col) (1)) (arg96_m_cols)
                in
                let var98_next_row  =
                    if ( = ) (var97_next_col) (0) then
                        ( + ) (arg93_row) (1)
                    else
                        arg93_row
                in
                let var100__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun22_strJoin ([||]) ([|fun19_int2string (fun66_matrixGeti (arg93_row) (arg94_col) (arg95_m_rows) (arg96_m_cols) (arg99_m)); if ( = ) (var97_next_col) (0) then
                        [|'\n'|]
                    else
                        [|' '|]|]))
                in
                fun92_printrc (arg99_m) (arg96_m_cols) (arg95_m_rows) (var98_next_row) (var97_next_col)
    in
    let fun101_printMatrixi arg89_m_rows arg90_m_cols arg91_m =
        fun92_printrc (arg91_m) (arg90_m_cols) (arg89_m_rows) (0) (0)
    in
    let rec fun112_dotprod arg120_b_cols arg119_b arg118_a arg117_innerDim arg113_acc arg114_p arg115_a_offset arg116_b_offset =
            if ( = ) (arg114_p) (arg117_innerDim) then
                arg113_acc
            else
                fun112_dotprod (arg120_b_cols) (arg119_b) (arg118_a) (arg117_innerDim) (( + ) (arg113_acc) (( * ) (Array.get (arg118_a) (arg115_a_offset)) (Array.get (arg119_b) (arg116_b_offset)))) (( + ) (arg114_p) (1)) (( + ) (arg115_a_offset) (1)) (( + ) (arg116_b_offset) (arg120_b_cols))
    in
    let fun121_matrixMuliWorker arg102_innerDim arg103_a_rows arg104_b_cols arg105_a arg106_b arg107_idx =
        let var108_row  =
            ( / ) (arg107_idx) (arg104_b_cols)
        in
        let var109_col  =
            ( mod ) (arg107_idx) (arg104_b_cols)
        in
        let var110_a_start_offset  =
            ( * ) (arg102_innerDim) (var108_row)
        in
        let var111_b_start_offset  =
            var109_col
        in
        fun112_dotprod (arg104_b_cols) (arg106_b) (arg105_a) (arg102_innerDim) (0) (0) (var110_a_start_offset) (var111_b_start_offset)
    in
    let rec fun132_dotprod arg139_outerDim arg138_a arg137_innerDim arg133_acc arg134_p arg135_aT_offset arg136_a_offset =
            if ( = ) (arg134_p) (arg137_innerDim) then
                arg133_acc
            else
                fun132_dotprod (arg139_outerDim) (arg138_a) (arg137_innerDim) (( + ) (arg133_acc) (( * ) (Array.get (arg138_a) (arg135_aT_offset)) (Array.get (arg138_a) (arg136_a_offset)))) (( + ) (arg134_p) (1)) (( + ) (arg135_aT_offset) (arg139_outerDim)) (( + ) (arg136_a_offset) (arg139_outerDim))
    in
    let fun140_matrixATAWorker arg122_rows arg123_cols arg124_a arg125_idx =
        let var126_innerDim  =
            arg122_rows
        in
        let var127_outerDim  =
            arg123_cols
        in
        let var128_row  =
            ( / ) (arg125_idx) (arg123_cols)
        in
        let var129_col  =
            ( mod ) (arg125_idx) (arg123_cols)
        in
        let var130_aT_start_offset  =
            var128_row
        in
        let var131_a_start_offset  =
            var129_col
        in
        fun132_dotprod (var127_outerDim) (arg124_a) (var126_innerDim) (0) (0) (var130_aT_start_offset) (var131_a_start_offset)
    in
    let var141_matA  =
        [|1; 3; 5; 2; 4; 6|]
    in
    let var142_matA_rows  =
        2
    in
    let var143_matA_cols  =
        3
    in
    let var144_vecX  =
        [|11; 31; 17|]
    in
    let var145_vecX_rows  =
        3
    in
    let var146_vecX_cols  =
        1
    in
    let var147__  =
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; 'm'; 'a'; 't'; 'A'; ':'; '\n'|])
    in
    let var148__  =
        fun101_printMatrixi (var142_matA_rows) (var143_matA_cols) (var141_matA)
    in
    let var149__  =
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; 'v'; 'e'; 'c'; 'X'; ':'; '\n'|])
    in
    let var150__  =
        fun101_printMatrixi (var145_vecX_rows) (var146_vecX_cols) (var144_vecX)
    in
    let var151_matATA  =
        fun14_seqInit (( * ) (var143_matA_cols) (var143_matA_cols)) (fun140_matrixATAWorker (var142_matA_rows) (var143_matA_cols) (var141_matA))
    in
    let var152_matATA_rows  =
        var143_matA_cols
    in
    let var153_matATA_cols  =
        var143_matA_cols
    in
    let var154__  =
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; 'm'; 'a'; 't'; 'A'; 'T'; 'A'; ':'; '\n'|])
    in
    let var155__  =
        fun101_printMatrixi (var152_matATA_rows) (var153_matATA_cols) (var151_matATA)
    in
    let var156_matATAx  =
        fun14_seqInit (( * ) (var152_matATA_rows) (var146_vecX_cols)) (fun121_matrixMuliWorker (var153_matATA_cols) (var152_matATA_rows) (var146_vecX_cols) (var151_matATA) (var144_vecX))
    in
    let var157_matATAx_rows  =
        var152_matATA_rows
    in
    let var158_matATAx_cols  =
        var146_vecX_cols
    in
    let var159__  =
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; 'm'; 'a'; 't'; 'A'; 'T'; 'A'; 'x'; ':'; '\n'|])
    in
    let var160__  =
        fun101_printMatrixi (var157_matATAx_rows) (var158_matATAx_cols) (var156_matATAx)
    in
    ()