open Printf
open Array

external gpuhost_fun150_matrixATAWorker: int array -> float array -> int array -> int array = "gpuhost_fun150_matrixATAWorker"
external gpuhost_fun131_matrixMuliWorker: int array -> float array -> int array -> int array -> int array = "gpuhost_fun131_matrixMuliWorker"

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
    let fun70_matrixMki arg67_rows arg68_cols arg69_v =
        Array.make (( * ) (arg67_rows) (arg68_cols)) (arg69_v)
    in
    let fun76_matrixGeti arg71_row arg72_col arg73_m_rows arg74_m_cols arg75_m =
        Array.get (arg75_m) (( + ) (( * ) (arg74_m_cols) (arg71_row)) (arg72_col))
    in
    let fun85_seqInitFun arg84_f arg81_cols arg80_i =
        let var82_row  =
            ( / ) (arg80_i) (arg81_cols)
        in
        let var83_col  =
            ( mod ) (arg80_i) (arg81_cols)
        in
        arg84_f (var82_row) (var83_col)
    in
    let fun86_matrixIniti arg77_rows arg78_cols arg79_f =
        fun14_seqInit (( * ) (arg77_rows) (arg78_cols)) (fun85_seqInitFun (arg79_f) (arg78_cols))
    in
    let rec fun90_printrc arg97_m arg94_m_cols arg93_m_rows arg91_row arg92_col =
            if ( = ) (arg91_row) (arg93_m_rows) then
                [||]
            else
                let var95_next_col  =
                    ( mod ) (( + ) (arg92_col) (1)) (arg94_m_cols)
                in
                let var96_next_row  =
                    if ( = ) (var95_next_col) (0) then
                        ( + ) (arg91_row) (1)
                    else
                        arg91_row
                in
                fun22_strJoin ([||]) ([|fun19_int2string (fun76_matrixGeti (arg91_row) (arg92_col) (arg93_m_rows) (arg94_m_cols) (arg97_m)); if ( = ) (var95_next_col) (0) then
                    [|'\n'|]
                else
                    [|' '|]; fun90_printrc (arg97_m) (arg94_m_cols) (arg93_m_rows) (var96_next_row) (var95_next_col)|])
    in
    let fun98_matrix2stri arg87_m_rows arg88_m_cols arg89_m =
        fun90_printrc (arg89_m) (arg88_m_cols) (arg87_m_rows) (0) (0)
    in
    let rec fun102_printrc arg109_m arg106_m_cols arg105_m_rows arg103_row arg104_col =
            if ( = ) (arg103_row) (arg105_m_rows) then
                [||]
            else
                let var107_next_col  =
                    ( mod ) (( + ) (arg104_col) (1)) (arg106_m_cols)
                in
                let var108_next_row  =
                    if ( = ) (var107_next_col) (0) then
                        ( + ) (arg103_row) (1)
                    else
                        arg103_row
                in
                let var110__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun22_strJoin ([||]) ([|fun19_int2string (fun76_matrixGeti (arg103_row) (arg104_col) (arg105_m_rows) (arg106_m_cols) (arg109_m)); if ( = ) (var107_next_col) (0) then
                        [|'\n'|]
                    else
                        [|' '|]|]))
                in
                fun102_printrc (arg109_m) (arg106_m_cols) (arg105_m_rows) (var108_next_row) (var107_next_col)
    in
    let fun111_printMatrixi arg99_m_rows arg100_m_cols arg101_m =
        fun102_printrc (arg101_m) (arg100_m_cols) (arg99_m_rows) (0) (0)
    in
    let rec fun122_dotprod arg130_b_cols arg129_b arg128_a arg127_innerDim arg123_acc arg124_p arg125_a_offset arg126_b_offset =
            if ( = ) (arg124_p) (arg127_innerDim) then
                arg123_acc
            else
                fun122_dotprod (arg130_b_cols) (arg129_b) (arg128_a) (arg127_innerDim) (( + ) (arg123_acc) (( * ) (Array.get (arg128_a) (arg125_a_offset)) (Array.get (arg129_b) (arg126_b_offset)))) (( + ) (arg124_p) (1)) (( + ) (arg125_a_offset) (1)) (( + ) (arg126_b_offset) (arg130_b_cols))
    in
    let fun131_matrixMuliWorker arg112_innerDim arg113_a_rows arg114_b_cols arg115_a arg116_b arg117_idx =
        let var118_row  =
            ( / ) (arg117_idx) (arg114_b_cols)
        in
        let var119_col  =
            ( mod ) (arg117_idx) (arg114_b_cols)
        in
        let var120_a_start_offset  =
            ( * ) (arg112_innerDim) (var118_row)
        in
        let var121_b_start_offset  =
            var119_col
        in
        fun122_dotprod (arg114_b_cols) (arg116_b) (arg115_a) (arg112_innerDim) (0) (0) (var120_a_start_offset) (var121_b_start_offset)
    in
    let rec fun142_dotprod arg149_outerDim arg148_a arg147_innerDim arg143_acc arg144_p arg145_aT_offset arg146_a_offset =
            if ( = ) (arg144_p) (arg147_innerDim) then
                arg143_acc
            else
                fun142_dotprod (arg149_outerDim) (arg148_a) (arg147_innerDim) (( + ) (arg143_acc) (( * ) (Array.get (arg148_a) (arg145_aT_offset)) (Array.get (arg148_a) (arg146_a_offset)))) (( + ) (arg144_p) (1)) (( + ) (arg145_aT_offset) (arg149_outerDim)) (( + ) (arg146_a_offset) (arg149_outerDim))
    in
    let fun150_matrixATAWorker arg132_rows arg133_cols arg134_a arg135_idx =
        let var136_innerDim  =
            arg132_rows
        in
        let var137_outerDim  =
            arg133_cols
        in
        let var138_row  =
            ( / ) (arg135_idx) (arg133_cols)
        in
        let var139_col  =
            ( mod ) (arg135_idx) (arg133_cols)
        in
        let var140_aT_start_offset  =
            var138_row
        in
        let var141_a_start_offset  =
            var139_col
        in
        fun142_dotprod (var137_outerDim) (arg134_a) (var136_innerDim) (0) (0) (var140_aT_start_offset) (var141_a_start_offset)
    in
    let fun157_matAinitfun_v2 arg155_row arg156_col =
        ( - ) (( mod ) (( + ) (( * ) (arg155_row) (arg155_row)) (arg156_col)) (3)) (1)
    in
    let fun160_vecXinitfun arg158_row arg159_col =
        ( mod ) (( * ) (arg158_row) (10657)) (41081)
    in
    let var151_matA_rows  =
        256
    in
    let var152_matA_cols  =
        256
    in
    let var153_matB_rows  =
        256
    in
    let var154_matB_cols  =
        256
    in
    let var161_matA  =
        fun86_matrixIniti (var151_matA_rows) (var152_matA_cols) (fun157_matAinitfun_v2)
    in
    let var162_vecX_rows  =
        var152_matA_cols
    in
    let var163_vecX_cols  =
        1
    in
    let var164_vecX  =
        fun86_matrixIniti (var162_vecX_rows) (var163_vecX_cols) (fun160_vecXinitfun)
    in
    let var165_matATA  =
        gpuhost_fun150_matrixATAWorker [|1; var151_matA_rows; var152_matA_cols; ( * ) (var152_matA_cols) (var152_matA_cols)|] [||] (var161_matA)
    in
    let var166_vecATAx  =
        gpuhost_fun131_matrixMuliWorker [|1; var152_matA_cols; var152_matA_cols; var163_vecX_cols; ( * ) (var152_matA_cols) (var163_vecX_cols)|] [||] (var165_matATA) (var164_vecX)
    in
    ()