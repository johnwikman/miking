open Printf

external gpuhost_fun141_matrixMulfWorker: int array -> float array -> float array -> float array -> float array = "gpuhost_fun141_matrixMulfWorker"

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
    let fun167_matAinitfun arg165_row arg166_col =
        float_of_int (( + ) (( * ) (arg165_row) (arg165_row)) (arg166_col))
    in
    let fun170_matBinitfun arg168_row arg169_col =
        ( /. ) (float_of_int (( / ) (( * ) (( + ) (arg168_row) (19)) (17)) (( + ) (arg169_col) (13)))) (float_of_int (( + ) (arg168_row) (11)))
    in
    let fun173_matAinitfun_v2 arg171_row arg172_col =
        ( -. ) (( /. ) (float_of_int (( + ) (( * ) (arg171_row) (arg171_row)) (arg172_col))) (3.0e+0)) (1.400000e-2)
    in
    let fun176_matBinitfun_v2 arg174_row arg175_col =
        float_of_int (( mod ) (( / ) (( * ) (( + ) (arg174_row) (19)) (17)) (( + ) (arg175_col) (13))) (2))
    in
    let fun191_bm_runonce arg188_matB arg187_matA arg186_matB_cols arg185_matA_rows arg184_innerDim arg183_matB_cols arg182_matA_rows arg180__ =
        let var181_bm_t_start  =
            Unix.gettimeofday (())
        in
        let var189_matAxB  =
            if (( < ) (( * ) (( * ) (arg182_matA_rows) (arg183_matB_cols)) (( + ) (50) (( + ) (12) (( + ) (0) (( + ) (7) (( + ) (7) (( + ) (5) (( + ) (1) (( + ) (16) (( + ) (0) (( * ) (94) (if ( < ) (1) (arg184_innerDim) then
                arg184_innerDim
            else
                1)))))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (( * ) (arg182_matA_rows) (arg183_matB_cols)) (Array.length (arg187_matA))) (Array.length (arg188_matB))) (10)) (40000000)) (( * ) (( + ) (12) (( + ) (0) (( + ) (64) (( + ) (64) (( + ) (30) (( + ) (1) (( + ) (16) (( + ) (0) (( * ) (995) (if ( < ) (1) (arg184_innerDim) then
                arg184_innerDim
            else
                1)))))))))) (( / ) (( + ) (( * ) (arg182_matA_rows) (arg183_matB_cols)) (( - ) (1536) (1))) (1536))))) then (Array.init (( * ) (arg182_matA_rows) (arg183_matB_cols)) (fun141_matrixMulfWorker (arg184_innerDim) (arg185_matA_rows) (arg186_matB_cols) (arg187_matA) (arg188_matB))) else (gpuhost_fun141_matrixMulfWorker [|1; arg184_innerDim; arg185_matA_rows; arg186_matB_cols; ( * ) (arg182_matA_rows) (arg183_matB_cols)|] [||] (arg187_matA) (arg188_matB))
        in
        let var190_bm_t_end  =
            Unix.gettimeofday (())
        in
        ( -. ) (var190_bm_t_end) (var181_bm_t_start)
    in
    let rec fun193_bm_iter arg204_matA_rows arg203_matB_cols arg202_innerDim arg201_matA_rows arg200_matB_cols arg199_matA arg198_matB arg196_n arg194_i arg195_acc =
            if ( >= ) (arg194_i) (arg196_n) then
                arg195_acc
            else
                let var197__  =
                    ()
                in
                let var205_res  =
                    fun191_bm_runonce (arg198_matB) (arg199_matA) (arg200_matB_cols) (arg201_matA_rows) (arg202_innerDim) (arg203_matB_cols) (arg204_matA_rows) (())
                in
                let var206_newacc  =
                    Array.append (arg195_acc) ([|var205_res|])
                in
                fun193_bm_iter (arg204_matA_rows) (arg203_matB_cols) (arg202_innerDim) (arg201_matA_rows) (arg200_matB_cols) (arg199_matA) (arg198_matB) (arg196_n) (( + ) (arg194_i) (1)) (var206_newacc)
    in
    let fun214_bm_runmultiple arg213_matB arg212_matA arg211_matB_cols arg210_matA_rows arg209_innerDim arg208_matB_cols arg207_matA_rows arg192_n =
        fun193_bm_iter (arg207_matA_rows) (arg208_matB_cols) (arg209_innerDim) (arg210_matA_rows) (arg211_matB_cols) (arg212_matA) (arg213_matB) (arg192_n) (0) ([||])
    in
    let rec fun217_quicksort_rec arg219_pivot arg220_lt_pivot arg221_geq_pivot arg222_remaining =
            if ( = ) (Array.length (arg222_remaining)) (0) then
                let var223_seq_lt  =
                    fun218_quicksort (arg220_lt_pivot)
                in
                let var224_seq_pivot  =
                    [|arg219_pivot|]
                in
                let var225_seq_geq  =
                    fun218_quicksort (arg221_geq_pivot)
                in
                Array.append (Array.append (var223_seq_lt) (var224_seq_pivot)) (var225_seq_geq)
            else
                let var226_e  =
                    fun1_head (arg222_remaining)
                in
                let var227_t  =
                    fun3_tail (arg222_remaining)
                in
                if ( < ) (var226_e) (arg219_pivot) then
                    fun217_quicksort_rec (arg219_pivot) ((fun x xs -> Array.append [|x|] xs) (var226_e) (arg220_lt_pivot)) (arg221_geq_pivot) (var227_t)
                else
                    fun217_quicksort_rec (arg219_pivot) (arg220_lt_pivot) ((fun x xs -> Array.append [|x|] xs) (var226_e) (arg221_geq_pivot)) (var227_t)
        and fun218_quicksort arg228_arr =
            if ( <= ) (Array.length (arg228_arr)) (1) then
                arg228_arr
            else
                fun217_quicksort_rec (fun1_head (arg228_arr)) ([||]) ([||]) (fun3_tail (arg228_arr))
    in
    let fun229_bm_sort arg215_arr =
        let var216_n  =
            Array.length (arg215_arr)
        in
        fun218_quicksort (arg215_arr)
    in
    let fun233_bm_median arg230_arr =
        let var231_n  =
            Array.length (arg230_arr)
        in
        let var232_sorted  =
            fun229_bm_sort (arg230_arr)
        in
        if ( = ) (( mod ) (var231_n) (2)) (0) then
            ( /. ) (( +. ) (Array.get (arg230_arr) (( - ) (( / ) (var231_n) (2)) (1))) (Array.get (arg230_arr) (( / ) (var231_n) (2)))) (2.0e+0)
        else
            Array.get (arg230_arr) (( / ) (var231_n) (2))
    in
    let rec fun236_work arg240_arr arg239_n arg237_i arg238_acc =
            if ( = ) (arg237_i) (arg239_n) then
                arg238_acc
            else
                fun236_work (arg240_arr) (arg239_n) (( + ) (arg237_i) (1)) (( +. ) (arg238_acc) (Array.get (arg240_arr) (arg237_i)))
    in
    let fun241_bm_sum arg234_arr =
        let var235_n  =
            Array.length (arg234_arr)
        in
        fun236_work (arg234_arr) (var235_n) (0) (0.0)
    in
    let rec fun244_work arg248_arr arg247_n arg245_i arg246_acc =
            if ( = ) (arg245_i) (arg247_n) then
                arg246_acc
            else
                let var249_e  =
                    Array.get (arg248_arr) (arg245_i)
                in
                fun244_work (arg248_arr) (arg247_n) (( + ) (arg245_i) (1)) (if ( > ) (var249_e) (arg246_acc) then
                    var249_e
                else
                    arg246_acc)
    in
    let fun250_bm_max arg242_arr =
        let var243_n  =
            Array.length (arg242_arr)
        in
        fun244_work (arg242_arr) (var243_n) (1) (Array.get (arg242_arr) (0))
    in
    let rec fun253_work arg257_arr arg256_n arg254_i arg255_acc =
            if ( = ) (arg254_i) (arg256_n) then
                arg255_acc
            else
                let var258_e  =
                    Array.get (arg257_arr) (arg254_i)
                in
                fun253_work (arg257_arr) (arg256_n) (( + ) (arg254_i) (1)) (if ( < ) (var258_e) (arg255_acc) then
                    var258_e
                else
                    arg255_acc)
    in
    let fun259_bm_min arg251_arr =
        let var252_n  =
            Array.length (arg251_arr)
        in
        fun253_work (arg251_arr) (var252_n) (1) (Array.get (arg251_arr) (0))
    in
    let fun262_bm_dist arg260_a arg261_b =
        if ( > ) (arg260_a) (arg261_b) then
            ( -. ) (arg260_a) (arg261_b)
        else
            ( -. ) (arg261_b) (arg260_a)
    in
    let var161_matA_rows  =
        1024
    in
    let var162_matA_cols  =
        1024
    in
    let var163_matB_rows  =
        1024
    in
    let var164_matB_cols  =
        1024
    in
    let var177_matA  =
        fun96_matrixInitf (var161_matA_rows) (var162_matA_cols) (fun173_matAinitfun_v2)
    in
    let var178_matB  =
        fun96_matrixInitf (var163_matB_rows) (var164_matB_cols) (fun176_matBinitfun_v2)
    in
    let var179_innerDim  =
        var162_matA_cols
    in
    let var263__  =
        ()
    in
    let var264_bmres_warmup  =
        fun214_bm_runmultiple (var178_matB) (var177_matA) (var164_matB_cols) (var161_matA_rows) (var179_innerDim) (var164_matB_cols) (var161_matA_rows) (4)
    in
    let var265__  =
        ()
    in
    let var266_bmres_iters  =
        fun214_bm_runmultiple (var178_matB) (var177_matA) (var164_matB_cols) (var161_matA_rows) (var179_innerDim) (var164_matB_cols) (var161_matA_rows) (15)
    in
    let var329__  =
        let var267_median  =
            fun233_bm_median (var266_bmres_iters)
        in
        let var268_sum  =
            fun241_bm_sum (var266_bmres_iters)
        in
        let var269_avg  =
            ( /. ) (var268_sum) (1.50e+1)
        in
        let var270_max  =
            fun250_bm_max (var266_bmres_iters)
        in
        let var271_min  =
            fun259_bm_min (var266_bmres_iters)
        in
        let var272_variance  =
            fun250_bm_max ([|fun262_bm_dist (var269_avg) (var270_max); fun262_bm_dist (var269_avg) (var271_min)|])
        in
        let var273_median  =
            ( *. ) (var267_median) (1.0e+3)
        in
        let var274_sum  =
            ( *. ) (var268_sum) (1.0e+3)
        in
        let var275_avg  =
            ( *. ) (var269_avg) (1.0e+3)
        in
        let var276_max  =
            ( *. ) (var270_max) (1.0e+3)
        in
        let var277_min  =
            ( *. ) (var271_min) (1.0e+3)
        in
        let var278_variance  =
            ( *. ) (var272_variance) (1.0e+3)
        in
        let var279__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'='; '='; ' '; 'I'; 'T'; 'E'; 'R'; 'A'; 'T'; 'I'; 'O'; 'N'; ' '; 'R'; 'E'; 'S'; 'U'; 'L'; 'T'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var280__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'i'; 't'; 'e'; 'r'; 'a'; 't'; 'i'; 'o'; 'n'; 's'; ':'; ' '|])
        in
        let var281__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'1'; '5'|])
        in
        let var282__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var283__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var284__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var273_median))
        in
        let var285__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var286__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var287__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var276_max))
        in
        let var288__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var289__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var290__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var277_min))
        in
        let var291__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var292__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var293__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var275_avg))
        in
        let var294__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var295__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var296__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var278_variance))
        in
        let var297__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var298_median  =
            fun233_bm_median (var264_bmres_warmup)
        in
        let var299_sum  =
            fun241_bm_sum (var264_bmres_warmup)
        in
        let var300_avg  =
            ( /. ) (var299_sum) (4.0e+0)
        in
        let var301_max  =
            fun250_bm_max (var264_bmres_warmup)
        in
        let var302_min  =
            fun259_bm_min (var264_bmres_warmup)
        in
        let var303_variance  =
            fun250_bm_max ([|fun262_bm_dist (var300_avg) (var301_max); fun262_bm_dist (var300_avg) (var302_min)|])
        in
        let var304_median  =
            ( *. ) (var298_median) (1.0e+3)
        in
        let var305_sum  =
            ( *. ) (var299_sum) (1.0e+3)
        in
        let var306_avg  =
            ( *. ) (var300_avg) (1.0e+3)
        in
        let var307_max  =
            ( *. ) (var301_max) (1.0e+3)
        in
        let var308_min  =
            ( *. ) (var302_min) (1.0e+3)
        in
        let var309_variance  =
            ( *. ) (var303_variance) (1.0e+3)
        in
        let var310__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; '\n'; '='; '='; ' '; 'W'; 'A'; 'R'; 'M'; 'U'; 'P'; ' '; 'S'; 'T'; 'A'; 'T'; 'I'; 'S'; 'T'; 'I'; 'C'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var311__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'w'; 'a'; 'r'; 'm'; 'u'; 'p'; ' '; 'r'; 'u'; 'n'; 's'; ':'; ' '|])
        in
        let var312__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'4'|])
        in
        let var313__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var314__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var315__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var304_median))
        in
        let var316__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var317__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var318__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var307_max))
        in
        let var319__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var320__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var321__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var308_min))
        in
        let var322__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var323__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var324__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var306_avg))
        in
        let var325__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var326__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var327__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var309_variance))
        in
        let var328__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        ()
    in
    ()