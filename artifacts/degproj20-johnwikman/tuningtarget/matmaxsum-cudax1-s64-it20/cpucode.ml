open Printf

external gpuhost_fun187_matrixMaxSumiWorker: int array -> float array -> int array -> int array -> int array = "gpuhost_fun187_matrixMaxSumiWorker"

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
    let fun80_matrixMki arg77_rows arg78_cols arg79_v =
        Array.make (( * ) (arg77_rows) (arg78_cols)) (arg79_v)
    in
    let fun86_matrixGeti arg81_row arg82_col arg83_m_rows arg84_m_cols arg85_m =
        Array.get (arg85_m) (( + ) (( * ) (arg84_m_cols) (arg81_row)) (arg82_col))
    in
    let fun95_seqInitFun arg94_f arg91_cols arg90_i =
        let var92_row  =
            ( / ) (arg90_i) (arg91_cols)
        in
        let var93_col  =
            ( mod ) (arg90_i) (arg91_cols)
        in
        arg94_f (var92_row) (var93_col)
    in
    let fun96_matrixIniti arg87_rows arg88_cols arg89_f =
        fun14_seqInit (( * ) (arg87_rows) (arg88_cols)) (fun95_seqInitFun (arg89_f) (arg88_cols))
    in
    let rec fun100_printrc arg107_m arg104_m_cols arg103_m_rows arg101_row arg102_col =
            if ( = ) (arg101_row) (arg103_m_rows) then
                [||]
            else
                let var105_next_col  =
                    ( mod ) (( + ) (arg102_col) (1)) (arg104_m_cols)
                in
                let var106_next_row  =
                    if ( = ) (var105_next_col) (0) then
                        ( + ) (arg101_row) (1)
                    else
                        arg101_row
                in
                fun22_strJoin ([||]) ([|fun19_int2string (fun86_matrixGeti (arg101_row) (arg102_col) (arg103_m_rows) (arg104_m_cols) (arg107_m)); if ( = ) (var105_next_col) (0) then
                    [|'\n'|]
                else
                    [|' '|]; fun100_printrc (arg107_m) (arg104_m_cols) (arg103_m_rows) (var106_next_row) (var105_next_col)|])
    in
    let fun108_matrix2stri arg97_m_rows arg98_m_cols arg99_m =
        fun100_printrc (arg99_m) (arg98_m_cols) (arg97_m_rows) (0) (0)
    in
    let rec fun112_printrc arg119_m arg116_m_cols arg115_m_rows arg113_row arg114_col =
            if ( = ) (arg113_row) (arg115_m_rows) then
                [||]
            else
                let var117_next_col  =
                    ( mod ) (( + ) (arg114_col) (1)) (arg116_m_cols)
                in
                let var118_next_row  =
                    if ( = ) (var117_next_col) (0) then
                        ( + ) (arg113_row) (1)
                    else
                        arg113_row
                in
                let var120__  =
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun22_strJoin ([||]) ([|fun19_int2string (fun86_matrixGeti (arg113_row) (arg114_col) (arg115_m_rows) (arg116_m_cols) (arg119_m)); if ( = ) (var117_next_col) (0) then
                        [|'\n'|]
                    else
                        [|' '|]|]))
                in
                fun112_printrc (arg119_m) (arg116_m_cols) (arg115_m_rows) (var118_next_row) (var117_next_col)
    in
    let fun121_printMatrixi arg109_m_rows arg110_m_cols arg111_m =
        fun112_printrc (arg111_m) (arg110_m_cols) (arg109_m_rows) (0) (0)
    in
    let rec fun132_dotprod arg140_b_cols arg139_b arg138_a arg137_innerDim arg133_acc arg134_p arg135_a_offset arg136_b_offset =
            if ( = ) (arg134_p) (arg137_innerDim) then
                arg133_acc
            else
                fun132_dotprod (arg140_b_cols) (arg139_b) (arg138_a) (arg137_innerDim) (( + ) (arg133_acc) (( * ) (Array.get (arg138_a) (arg135_a_offset)) (Array.get (arg139_b) (arg136_b_offset)))) (( + ) (arg134_p) (1)) (( + ) (arg135_a_offset) (1)) (( + ) (arg136_b_offset) (arg140_b_cols))
    in
    let fun141_matrixMuliWorker arg122_innerDim arg123_a_rows arg124_b_cols arg125_a arg126_b arg127_idx =
        let var128_row  =
            ( / ) (arg127_idx) (arg124_b_cols)
        in
        let var129_col  =
            ( mod ) (arg127_idx) (arg124_b_cols)
        in
        let var130_a_start_offset  =
            ( * ) (arg122_innerDim) (var128_row)
        in
        let var131_b_start_offset  =
            var129_col
        in
        fun132_dotprod (arg124_b_cols) (arg126_b) (arg125_a) (arg122_innerDim) (0) (0) (var130_a_start_offset) (var131_b_start_offset)
    in
    let rec fun152_dotprod arg159_outerDim arg158_a arg157_innerDim arg153_acc arg154_p arg155_aT_offset arg156_a_offset =
            if ( = ) (arg154_p) (arg157_innerDim) then
                arg153_acc
            else
                fun152_dotprod (arg159_outerDim) (arg158_a) (arg157_innerDim) (( + ) (arg153_acc) (( * ) (Array.get (arg158_a) (arg155_aT_offset)) (Array.get (arg158_a) (arg156_a_offset)))) (( + ) (arg154_p) (1)) (( + ) (arg155_aT_offset) (arg159_outerDim)) (( + ) (arg156_a_offset) (arg159_outerDim))
    in
    let fun160_matrixATAiWorker arg142_rows arg143_cols arg144_a arg145_idx =
        let var146_innerDim  =
            arg142_rows
        in
        let var147_outerDim  =
            arg143_cols
        in
        let var148_row  =
            ( / ) (arg145_idx) (arg143_cols)
        in
        let var149_col  =
            ( mod ) (arg145_idx) (arg143_cols)
        in
        let var150_aT_start_offset  =
            var148_row
        in
        let var151_a_start_offset  =
            var149_col
        in
        fun152_dotprod (var147_outerDim) (arg144_a) (var146_innerDim) (0) (0) (var150_aT_start_offset) (var151_a_start_offset)
    in
    let fun167_maxi arg165_x arg166_y =
        if ( < ) (arg165_x) (arg166_y) then
            arg166_y
        else
            arg165_x
    in
    let rec fun178_vecmsum arg186_b_cols arg185_b arg184_a arg183_innerDim arg179_acc arg180_p arg181_a_offset arg182_b_offset =
            if ( = ) (arg180_p) (arg183_innerDim) then
                arg179_acc
            else
                fun178_vecmsum (arg186_b_cols) (arg185_b) (arg184_a) (arg183_innerDim) (( + ) (arg179_acc) (fun167_maxi (Array.get (arg184_a) (arg181_a_offset)) (Array.get (arg185_b) (arg182_b_offset)))) (( + ) (arg180_p) (1)) (( + ) (arg181_a_offset) (1)) (( + ) (arg182_b_offset) (arg186_b_cols))
    in
    let fun187_matrixMaxSumiWorker arg168_innerDim arg169_a_rows arg170_b_cols arg171_a arg172_b arg173_idx =
        let var174_row  =
            ( / ) (arg173_idx) (arg170_b_cols)
        in
        let var175_col  =
            ( mod ) (arg173_idx) (arg170_b_cols)
        in
        let var176_a_start_offset  =
            ( * ) (arg168_innerDim) (var174_row)
        in
        let var177_b_start_offset  =
            var175_col
        in
        fun178_vecmsum (arg170_b_cols) (arg172_b) (arg171_a) (arg168_innerDim) (0) (0) (var176_a_start_offset) (var177_b_start_offset)
    in
    let fun190_matAinitfun arg188_row arg189_col =
        ( + ) (( * ) (arg188_row) (arg188_row)) (arg189_col)
    in
    let fun193_matBinitfun arg191_row arg192_col =
        ( mod ) (( / ) (( * ) (( + ) (arg191_row) (19)) (17)) (( + ) (arg192_col) (13))) (( + ) (arg191_row) (11))
    in
    let fun196_matAinitfun_v2 arg194_row arg195_col =
        ( - ) (( mod ) (( + ) (( * ) (arg194_row) (arg194_row)) (arg195_col)) (3)) (1)
    in
    let fun199_matBinitfun_v2 arg197_row arg198_col =
        ( mod ) (( / ) (( * ) (( + ) (arg197_row) (19)) (17)) (( + ) (arg198_col) (13))) (2)
    in
    let var161_matA_rows  =
        64
    in
    let var162_matA_cols  =
        64
    in
    let var163_matB_rows  =
        64
    in
    let var164_matB_cols  =
        64
    in
    let var200_matA  =
        fun96_matrixIniti (var161_matA_rows) (var162_matA_cols) (fun196_matAinitfun_v2)
    in
    let var201_matB  =
        fun96_matrixIniti (var163_matB_rows) (var164_matB_cols) (fun199_matBinitfun_v2)
    in
    let var202_innerDim  =
        var162_matA_cols
    in
    let var203_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    let var204_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    let var205_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    let var206_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    let var207_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    let var208_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    let var209_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    let var210_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    let var211_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    let var212_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    let var213_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    let var214_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    let var215_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    let var216_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    let var217_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    let var218_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    let var219_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    let var220_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    let var221_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    let var222_matAmB  =
        gpuhost_fun187_matrixMaxSumiWorker [|1; var202_innerDim; var161_matA_rows; var164_matB_cols; ( * ) (var161_matA_rows) (var164_matB_cols)|] [||] (var200_matA) (var201_matB)
    in
    ()