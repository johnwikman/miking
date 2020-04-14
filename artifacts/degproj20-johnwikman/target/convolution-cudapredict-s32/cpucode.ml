open Printf

external gpuhost_fun186_convolute17Worker: int array -> float array -> float array -> float array -> float array = "gpuhost_fun186_convolute17Worker"

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
    let fun80_matrixMkf arg77_rows arg78_cols arg79_v =
        Array.make (( * ) (arg77_rows) (arg78_cols)) (arg79_v)
    in
    let fun86_matrixGetf arg81_row arg82_col arg83_m_rows arg84_m_cols arg85_m =
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
    let fun96_matrixInitf arg87_rows arg88_cols arg89_f =
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
                fun22_strJoin ([||]) ([|fun21_float2string (fun86_matrixGetf (arg101_row) (arg102_col) (arg103_m_rows) (arg104_m_cols) (arg107_m)); if ( = ) (var105_next_col) (0) then
                    [|'\n'|]
                else
                    [|' '|]; fun100_printrc (arg107_m) (arg104_m_cols) (arg103_m_rows) (var106_next_row) (var105_next_col)|])
    in
    let fun108_matrix2strf arg97_m_rows arg98_m_cols arg99_m =
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
                    (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun22_strJoin ([||]) ([|fun21_float2string (fun86_matrixGetf (arg113_row) (arg114_col) (arg115_m_rows) (arg116_m_cols) (arg119_m)); if ( = ) (var117_next_col) (0) then
                        [|'\n'|]
                    else
                        [|' '|]|]))
                in
                fun112_printrc (arg119_m) (arg116_m_cols) (arg115_m_rows) (var118_next_row) (var117_next_col)
    in
    let fun121_printMatrixf arg109_m_rows arg110_m_cols arg111_m =
        fun112_printrc (arg111_m) (arg110_m_cols) (arg109_m_rows) (0) (0)
    in
    let rec fun132_dotprod arg140_b_cols arg139_b arg138_a arg137_innerDim arg133_acc arg134_p arg135_a_offset arg136_b_offset =
            if ( = ) (arg134_p) (arg137_innerDim) then
                arg133_acc
            else
                fun132_dotprod (arg140_b_cols) (arg139_b) (arg138_a) (arg137_innerDim) (( +. ) (arg133_acc) (( *. ) (Array.get (arg138_a) (arg135_a_offset)) (Array.get (arg139_b) (arg136_b_offset)))) (( + ) (arg134_p) (1)) (( + ) (arg135_a_offset) (1)) (( + ) (arg136_b_offset) (arg140_b_cols))
    in
    let fun141_matrixMulfWorker arg122_innerDim arg123_a_rows arg124_b_cols arg125_a arg126_b arg127_idx =
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
        fun132_dotprod (arg124_b_cols) (arg126_b) (arg125_a) (arg122_innerDim) (0.0) (0) (var130_a_start_offset) (var131_b_start_offset)
    in
    let rec fun152_dotprod arg159_outerDim arg158_a arg157_innerDim arg153_acc arg154_p arg155_aT_offset arg156_a_offset =
            if ( = ) (arg154_p) (arg157_innerDim) then
                arg153_acc
            else
                fun152_dotprod (arg159_outerDim) (arg158_a) (arg157_innerDim) (( +. ) (arg153_acc) (( *. ) (Array.get (arg158_a) (arg155_aT_offset)) (Array.get (arg158_a) (arg156_a_offset)))) (( + ) (arg154_p) (1)) (( + ) (arg155_aT_offset) (arg159_outerDim)) (( + ) (arg156_a_offset) (arg159_outerDim))
    in
    let fun160_matrixATAfWorker arg142_rows arg143_cols arg144_a arg145_idx =
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
        fun152_dotprod (var147_outerDim) (arg144_a) (var146_innerDim) (0.0) (0) (var150_aT_start_offset) (var151_a_start_offset)
    in
    let fun165_matAinitfun_v3 arg163_row arg164_col =
        ( -. ) (( /. ) (float_of_int (( mod ) (( * ) (arg163_row) (arg163_row)) (( + ) (arg164_col) (17)))) (3.991000e+0)) (1.400000e-2)
    in
    let fun167_filter17initfun arg166_i =
        ( -. ) (1.250e+0) (( /. ) (1.0e-0) (( +. ) (float_of_int (arg166_i)) (1.0e-0)))
    in
    let rec fun180_work arg185_filter arg184_originalOffset arg183_mat arg181_acc arg182_i =
            if ( = ) (arg182_i) (17) then
                arg181_acc
            else
                fun180_work (arg185_filter) (arg184_originalOffset) (arg183_mat) (( +. ) (arg181_acc) (( *. ) (Array.get (arg183_mat) (( + ) (arg184_originalOffset) (arg182_i))) (Array.get (arg185_filter) (arg182_i)))) (( + ) (arg182_i) (1))
    in
    let fun186_convolute17Worker arg168_filter arg169_rows arg170_cols arg171_mat arg172_idx =
        let var173_row  =
            ( / ) (arg172_idx) (arg170_cols)
        in
        let var174_col  =
            ( mod ) (arg172_idx) (arg170_cols)
        in
        let var175_originalRows  =
            arg169_rows
        in
        let var176_originalCols  =
            ( + ) (arg170_cols) (16)
        in
        let var177_originalRow  =
            var173_row
        in
        let var178_originalCol  =
            ( + ) (var174_col) (8)
        in
        let var179_originalOffset  =
            ( - ) (( + ) (( * ) (var177_originalRow) (var176_originalCols)) (var178_originalCol)) (8)
        in
        fun180_work (arg168_filter) (var179_originalOffset) (arg171_mat) (0.0) (0)
    in
    let fun201_bm_runonce arg198_matA arg197_resCols arg196_resRows arg195_filter17 arg194_resCols arg193_resRows arg191__ =
        let var192_bm_t_start  =
            Unix.gettimeofday (())
        in
        let var199_matRes  =
            if (( < ) (( * ) (( * ) (arg193_resRows) (arg194_resCols)) (( + ) (50) (1513))) (( + ) (( + ) (( * ) (( + ) (( + ) (( * ) (arg193_resRows) (arg194_resCols)) (Array.length (arg195_filter17))) (Array.length (arg198_matA))) (10)) (40000000)) (( * ) (16826) (( / ) (( + ) (( * ) (arg193_resRows) (arg194_resCols)) (( - ) (1536) (1))) (1536))))) then (Array.init (( * ) (arg193_resRows) (arg194_resCols)) (fun186_convolute17Worker (arg195_filter17) (arg196_resRows) (arg197_resCols) (arg198_matA))) else (gpuhost_fun186_convolute17Worker [|1; arg196_resRows; arg197_resCols; ( * ) (arg193_resRows) (arg194_resCols)|] [||] (arg195_filter17) (arg198_matA))
        in
        let var200_bm_t_end  =
            Unix.gettimeofday (())
        in
        ( -. ) (var200_bm_t_end) (var192_bm_t_start)
    in
    let rec fun203_bm_iter arg213_resRows arg212_resCols arg211_filter17 arg210_resRows arg209_resCols arg208_matA arg206_n arg204_i arg205_acc =
            if ( >= ) (arg204_i) (arg206_n) then
                arg205_acc
            else
                let var207__  =
                    ()
                in
                let var214_res  =
                    fun201_bm_runonce (arg208_matA) (arg209_resCols) (arg210_resRows) (arg211_filter17) (arg212_resCols) (arg213_resRows) (())
                in
                let var215_newacc  =
                    Array.append (arg205_acc) ([|var214_res|])
                in
                fun203_bm_iter (arg213_resRows) (arg212_resCols) (arg211_filter17) (arg210_resRows) (arg209_resCols) (arg208_matA) (arg206_n) (( + ) (arg204_i) (1)) (var215_newacc)
    in
    let fun222_bm_runmultiple arg221_matA arg220_resCols arg219_resRows arg218_filter17 arg217_resCols arg216_resRows arg202_n =
        fun203_bm_iter (arg216_resRows) (arg217_resCols) (arg218_filter17) (arg219_resRows) (arg220_resCols) (arg221_matA) (arg202_n) (0) ([||])
    in
    let rec fun225_quicksort_rec arg227_pivot arg228_lt_pivot arg229_geq_pivot arg230_remaining =
            if ( = ) (Array.length (arg230_remaining)) (0) then
                let var231_seq_lt  =
                    fun226_quicksort (arg228_lt_pivot)
                in
                let var232_seq_pivot  =
                    [|arg227_pivot|]
                in
                let var233_seq_geq  =
                    fun226_quicksort (arg229_geq_pivot)
                in
                Array.append (Array.append (var231_seq_lt) (var232_seq_pivot)) (var233_seq_geq)
            else
                let var234_e  =
                    fun1_head (arg230_remaining)
                in
                let var235_t  =
                    fun3_tail (arg230_remaining)
                in
                if ( < ) (var234_e) (arg227_pivot) then
                    fun225_quicksort_rec (arg227_pivot) ((fun x xs -> Array.append [|x|] xs) (var234_e) (arg228_lt_pivot)) (arg229_geq_pivot) (var235_t)
                else
                    fun225_quicksort_rec (arg227_pivot) (arg228_lt_pivot) ((fun x xs -> Array.append [|x|] xs) (var234_e) (arg229_geq_pivot)) (var235_t)
        and fun226_quicksort arg236_arr =
            if ( <= ) (Array.length (arg236_arr)) (1) then
                arg236_arr
            else
                fun225_quicksort_rec (fun1_head (arg236_arr)) ([||]) ([||]) (fun3_tail (arg236_arr))
    in
    let fun237_bm_sort arg223_arr =
        let var224_n  =
            Array.length (arg223_arr)
        in
        fun226_quicksort (arg223_arr)
    in
    let fun241_bm_median arg238_arr =
        let var239_n  =
            Array.length (arg238_arr)
        in
        let var240_sorted  =
            fun237_bm_sort (arg238_arr)
        in
        if ( = ) (( mod ) (var239_n) (2)) (0) then
            ( /. ) (( +. ) (Array.get (arg238_arr) (( - ) (( / ) (var239_n) (2)) (1))) (Array.get (arg238_arr) (( / ) (var239_n) (2)))) (2.0e+0)
        else
            Array.get (arg238_arr) (( / ) (var239_n) (2))
    in
    let rec fun244_work arg248_arr arg247_n arg245_i arg246_acc =
            if ( = ) (arg245_i) (arg247_n) then
                arg246_acc
            else
                fun244_work (arg248_arr) (arg247_n) (( + ) (arg245_i) (1)) (( +. ) (arg246_acc) (Array.get (arg248_arr) (arg245_i)))
    in
    let fun249_bm_sum arg242_arr =
        let var243_n  =
            Array.length (arg242_arr)
        in
        fun244_work (arg242_arr) (var243_n) (0) (0.0)
    in
    let rec fun252_work arg256_arr arg255_n arg253_i arg254_acc =
            if ( = ) (arg253_i) (arg255_n) then
                arg254_acc
            else
                let var257_e  =
                    Array.get (arg256_arr) (arg253_i)
                in
                fun252_work (arg256_arr) (arg255_n) (( + ) (arg253_i) (1)) (if ( > ) (var257_e) (arg254_acc) then
                    var257_e
                else
                    arg254_acc)
    in
    let fun258_bm_max arg250_arr =
        let var251_n  =
            Array.length (arg250_arr)
        in
        fun252_work (arg250_arr) (var251_n) (1) (Array.get (arg250_arr) (0))
    in
    let rec fun261_work arg265_arr arg264_n arg262_i arg263_acc =
            if ( = ) (arg262_i) (arg264_n) then
                arg263_acc
            else
                let var266_e  =
                    Array.get (arg265_arr) (arg262_i)
                in
                fun261_work (arg265_arr) (arg264_n) (( + ) (arg262_i) (1)) (if ( < ) (var266_e) (arg263_acc) then
                    var266_e
                else
                    arg263_acc)
    in
    let fun267_bm_min arg259_arr =
        let var260_n  =
            Array.length (arg259_arr)
        in
        fun261_work (arg259_arr) (var260_n) (1) (Array.get (arg259_arr) (0))
    in
    let fun270_bm_dist arg268_a arg269_b =
        if ( > ) (arg268_a) (arg269_b) then
            ( -. ) (arg268_a) (arg269_b)
        else
            ( -. ) (arg269_b) (arg268_a)
    in
    let var161_matA_rows  =
        32
    in
    let var162_matA_cols  =
        32
    in
    let var187_matA  =
        fun96_matrixInitf (var161_matA_rows) (var162_matA_cols) (fun165_matAinitfun_v3)
    in
    let var188_filter17  =
        fun14_seqInit (17) (fun167_filter17initfun)
    in
    let var189_resRows  =
        var161_matA_rows
    in
    let var190_resCols  =
        ( - ) (var162_matA_cols) (16)
    in
    let var271__  =
        ()
    in
    let var272_bmres_warmup  =
        fun222_bm_runmultiple (var187_matA) (var190_resCols) (var189_resRows) (var188_filter17) (var190_resCols) (var189_resRows) (4)
    in
    let var273__  =
        ()
    in
    let var274_bmres_iters  =
        fun222_bm_runmultiple (var187_matA) (var190_resCols) (var189_resRows) (var188_filter17) (var190_resCols) (var189_resRows) (15)
    in
    let var337__  =
        let var275_median  =
            fun241_bm_median (var274_bmres_iters)
        in
        let var276_sum  =
            fun249_bm_sum (var274_bmres_iters)
        in
        let var277_avg  =
            ( /. ) (var276_sum) (1.50e+1)
        in
        let var278_max  =
            fun258_bm_max (var274_bmres_iters)
        in
        let var279_min  =
            fun267_bm_min (var274_bmres_iters)
        in
        let var280_variance  =
            fun258_bm_max ([|fun270_bm_dist (var277_avg) (var278_max); fun270_bm_dist (var277_avg) (var279_min)|])
        in
        let var281_median  =
            ( *. ) (var275_median) (1.0e+3)
        in
        let var282_sum  =
            ( *. ) (var276_sum) (1.0e+3)
        in
        let var283_avg  =
            ( *. ) (var277_avg) (1.0e+3)
        in
        let var284_max  =
            ( *. ) (var278_max) (1.0e+3)
        in
        let var285_min  =
            ( *. ) (var279_min) (1.0e+3)
        in
        let var286_variance  =
            ( *. ) (var280_variance) (1.0e+3)
        in
        let var287__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'='; '='; ' '; 'I'; 'T'; 'E'; 'R'; 'A'; 'T'; 'I'; 'O'; 'N'; ' '; 'R'; 'E'; 'S'; 'U'; 'L'; 'T'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var288__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'i'; 't'; 'e'; 'r'; 'a'; 't'; 'i'; 'o'; 'n'; 's'; ':'; ' '|])
        in
        let var289__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'1'; '5'|])
        in
        let var290__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var291__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var292__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var281_median))
        in
        let var293__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var294__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var295__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var284_max))
        in
        let var296__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var297__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var298__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var285_min))
        in
        let var299__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var300__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var301__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var283_avg))
        in
        let var302__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var303__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var304__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var286_variance))
        in
        let var305__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var306_median  =
            fun241_bm_median (var272_bmres_warmup)
        in
        let var307_sum  =
            fun249_bm_sum (var272_bmres_warmup)
        in
        let var308_avg  =
            ( /. ) (var307_sum) (4.0e+0)
        in
        let var309_max  =
            fun258_bm_max (var272_bmres_warmup)
        in
        let var310_min  =
            fun267_bm_min (var272_bmres_warmup)
        in
        let var311_variance  =
            fun258_bm_max ([|fun270_bm_dist (var308_avg) (var309_max); fun270_bm_dist (var308_avg) (var310_min)|])
        in
        let var312_median  =
            ( *. ) (var306_median) (1.0e+3)
        in
        let var313_sum  =
            ( *. ) (var307_sum) (1.0e+3)
        in
        let var314_avg  =
            ( *. ) (var308_avg) (1.0e+3)
        in
        let var315_max  =
            ( *. ) (var309_max) (1.0e+3)
        in
        let var316_min  =
            ( *. ) (var310_min) (1.0e+3)
        in
        let var317_variance  =
            ( *. ) (var311_variance) (1.0e+3)
        in
        let var318__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; '\n'; '='; '='; ' '; 'W'; 'A'; 'R'; 'M'; 'U'; 'P'; ' '; 'S'; 'T'; 'A'; 'T'; 'I'; 'S'; 'T'; 'I'; 'C'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var319__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'w'; 'a'; 'r'; 'm'; 'u'; 'p'; ' '; 'r'; 'u'; 'n'; 's'; ':'; ' '|])
        in
        let var320__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'4'|])
        in
        let var321__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var322__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var323__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var312_median))
        in
        let var324__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var325__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var326__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var315_max))
        in
        let var327__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var328__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var329__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var316_min))
        in
        let var330__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var331__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var332__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var314_avg))
        in
        let var333__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var334__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var335__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var317_variance))
        in
        let var336__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        ()
    in
    ()