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
    let fun165_matAinitfun_v2 arg163_row arg164_col =
        ( -. ) (( /. ) (float_of_int (( + ) (( * ) (arg163_row) (arg163_row)) (arg164_col))) (3.0e+0)) (1.400000e-2)
    in
    let fun168_vecXinitfun arg166_row arg167_col =
        float_of_int (( mod ) (( * ) (arg166_row) (10657)) (41081))
    in
    let fun184_bm_runonce arg181_vecX arg180_matA arg179_vecX_cols arg178_matA_rows arg177_matA_cols arg176_vecX_cols arg175_matA_cols arg173__ =
        let var174_bm_t_start  =
            Unix.gettimeofday (())
        in
        let var182_vecAx  =
            if (( < ) (( * ) (( * ) (arg175_matA_cols) (arg176_vecX_cols)) (( + ) (50) (( + ) (12) (( + ) (0) (( + ) (7) (( + ) (7) (( + ) (5) (( + ) (1) (( + ) (16) (( + ) (0) (( * ) (94) (if ( < ) (1) (arg177_matA_cols) then
                arg177_matA_cols
            else
                1)))))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (( * ) (arg175_matA_cols) (arg176_vecX_cols)) (Array.length (arg180_matA))) (Array.length (arg181_vecX))) (10)) (40000000)) (( * ) (( + ) (12) (( + ) (0) (( + ) (64) (( + ) (64) (( + ) (30) (( + ) (1) (( + ) (16) (( + ) (0) (( * ) (995) (if ( < ) (1) (arg177_matA_cols) then
                arg177_matA_cols
            else
                1)))))))))) (( / ) (( + ) (( * ) (arg175_matA_cols) (arg176_vecX_cols)) (( - ) (1536) (1))) (1536))))) then (Array.init (( * ) (arg175_matA_cols) (arg176_vecX_cols)) (fun141_matrixMulfWorker (arg177_matA_cols) (arg178_matA_rows) (arg179_vecX_cols) (arg180_matA) (arg181_vecX))) else (gpuhost_fun141_matrixMulfWorker [|1; arg177_matA_cols; arg178_matA_rows; arg179_vecX_cols; ( * ) (arg175_matA_cols) (arg176_vecX_cols)|] [||] (arg180_matA) (arg181_vecX))
        in
        let var183_bm_t_end  =
            Unix.gettimeofday (())
        in
        ( -. ) (var183_bm_t_end) (var174_bm_t_start)
    in
    let rec fun186_bm_iter arg197_matA_cols arg196_vecX_cols arg195_matA_cols arg194_matA_rows arg193_vecX_cols arg192_matA arg191_vecX arg189_n arg187_i arg188_acc =
            if ( >= ) (arg187_i) (arg189_n) then
                arg188_acc
            else
                let var190__  =
                    ()
                in
                let var198_res  =
                    fun184_bm_runonce (arg191_vecX) (arg192_matA) (arg193_vecX_cols) (arg194_matA_rows) (arg195_matA_cols) (arg196_vecX_cols) (arg197_matA_cols) (())
                in
                let var199_newacc  =
                    Array.append (arg188_acc) ([|var198_res|])
                in
                fun186_bm_iter (arg197_matA_cols) (arg196_vecX_cols) (arg195_matA_cols) (arg194_matA_rows) (arg193_vecX_cols) (arg192_matA) (arg191_vecX) (arg189_n) (( + ) (arg187_i) (1)) (var199_newacc)
    in
    let fun207_bm_runmultiple arg206_vecX arg205_matA arg204_vecX_cols arg203_matA_rows arg202_matA_cols arg201_vecX_cols arg200_matA_cols arg185_n =
        fun186_bm_iter (arg200_matA_cols) (arg201_vecX_cols) (arg202_matA_cols) (arg203_matA_rows) (arg204_vecX_cols) (arg205_matA) (arg206_vecX) (arg185_n) (0) ([||])
    in
    let rec fun210_quicksort_rec arg212_pivot arg213_lt_pivot arg214_geq_pivot arg215_remaining =
            if ( = ) (Array.length (arg215_remaining)) (0) then
                let var216_seq_lt  =
                    fun211_quicksort (arg213_lt_pivot)
                in
                let var217_seq_pivot  =
                    [|arg212_pivot|]
                in
                let var218_seq_geq  =
                    fun211_quicksort (arg214_geq_pivot)
                in
                Array.append (Array.append (var216_seq_lt) (var217_seq_pivot)) (var218_seq_geq)
            else
                let var219_e  =
                    fun1_head (arg215_remaining)
                in
                let var220_t  =
                    fun3_tail (arg215_remaining)
                in
                if ( < ) (var219_e) (arg212_pivot) then
                    fun210_quicksort_rec (arg212_pivot) ((fun x xs -> Array.append [|x|] xs) (var219_e) (arg213_lt_pivot)) (arg214_geq_pivot) (var220_t)
                else
                    fun210_quicksort_rec (arg212_pivot) (arg213_lt_pivot) ((fun x xs -> Array.append [|x|] xs) (var219_e) (arg214_geq_pivot)) (var220_t)
        and fun211_quicksort arg221_arr =
            if ( <= ) (Array.length (arg221_arr)) (1) then
                arg221_arr
            else
                fun210_quicksort_rec (fun1_head (arg221_arr)) ([||]) ([||]) (fun3_tail (arg221_arr))
    in
    let fun222_bm_sort arg208_arr =
        let var209_n  =
            Array.length (arg208_arr)
        in
        fun211_quicksort (arg208_arr)
    in
    let fun226_bm_median arg223_arr =
        let var224_n  =
            Array.length (arg223_arr)
        in
        let var225_sorted  =
            fun222_bm_sort (arg223_arr)
        in
        if ( = ) (( mod ) (var224_n) (2)) (0) then
            ( /. ) (( +. ) (Array.get (arg223_arr) (( - ) (( / ) (var224_n) (2)) (1))) (Array.get (arg223_arr) (( / ) (var224_n) (2)))) (2.0e+0)
        else
            Array.get (arg223_arr) (( / ) (var224_n) (2))
    in
    let rec fun229_work arg233_arr arg232_n arg230_i arg231_acc =
            if ( = ) (arg230_i) (arg232_n) then
                arg231_acc
            else
                fun229_work (arg233_arr) (arg232_n) (( + ) (arg230_i) (1)) (( +. ) (arg231_acc) (Array.get (arg233_arr) (arg230_i)))
    in
    let fun234_bm_sum arg227_arr =
        let var228_n  =
            Array.length (arg227_arr)
        in
        fun229_work (arg227_arr) (var228_n) (0) (0.0)
    in
    let rec fun237_work arg241_arr arg240_n arg238_i arg239_acc =
            if ( = ) (arg238_i) (arg240_n) then
                arg239_acc
            else
                let var242_e  =
                    Array.get (arg241_arr) (arg238_i)
                in
                fun237_work (arg241_arr) (arg240_n) (( + ) (arg238_i) (1)) (if ( > ) (var242_e) (arg239_acc) then
                    var242_e
                else
                    arg239_acc)
    in
    let fun243_bm_max arg235_arr =
        let var236_n  =
            Array.length (arg235_arr)
        in
        fun237_work (arg235_arr) (var236_n) (1) (Array.get (arg235_arr) (0))
    in
    let rec fun246_work arg250_arr arg249_n arg247_i arg248_acc =
            if ( = ) (arg247_i) (arg249_n) then
                arg248_acc
            else
                let var251_e  =
                    Array.get (arg250_arr) (arg247_i)
                in
                fun246_work (arg250_arr) (arg249_n) (( + ) (arg247_i) (1)) (if ( < ) (var251_e) (arg248_acc) then
                    var251_e
                else
                    arg248_acc)
    in
    let fun252_bm_min arg244_arr =
        let var245_n  =
            Array.length (arg244_arr)
        in
        fun246_work (arg244_arr) (var245_n) (1) (Array.get (arg244_arr) (0))
    in
    let fun255_bm_dist arg253_a arg254_b =
        if ( > ) (arg253_a) (arg254_b) then
            ( -. ) (arg253_a) (arg254_b)
        else
            ( -. ) (arg254_b) (arg253_a)
    in
    let var161_matA_rows  =
        4096
    in
    let var162_matA_cols  =
        4096
    in
    let var169_matA  =
        fun96_matrixInitf (var161_matA_rows) (var162_matA_cols) (fun165_matAinitfun_v2)
    in
    let var170_vecX_rows  =
        var162_matA_cols
    in
    let var171_vecX_cols  =
        1
    in
    let var172_vecX  =
        fun96_matrixInitf (var170_vecX_rows) (var171_vecX_cols) (fun168_vecXinitfun)
    in
    let var256__  =
        ()
    in
    let var257_bmres_warmup  =
        fun207_bm_runmultiple (var172_vecX) (var169_matA) (var171_vecX_cols) (var161_matA_rows) (var162_matA_cols) (var171_vecX_cols) (var162_matA_cols) (4)
    in
    let var258__  =
        ()
    in
    let var259_bmres_iters  =
        fun207_bm_runmultiple (var172_vecX) (var169_matA) (var171_vecX_cols) (var161_matA_rows) (var162_matA_cols) (var171_vecX_cols) (var162_matA_cols) (15)
    in
    let var322__  =
        let var260_median  =
            fun226_bm_median (var259_bmres_iters)
        in
        let var261_sum  =
            fun234_bm_sum (var259_bmres_iters)
        in
        let var262_avg  =
            ( /. ) (var261_sum) (1.50e+1)
        in
        let var263_max  =
            fun243_bm_max (var259_bmres_iters)
        in
        let var264_min  =
            fun252_bm_min (var259_bmres_iters)
        in
        let var265_variance  =
            fun243_bm_max ([|fun255_bm_dist (var262_avg) (var263_max); fun255_bm_dist (var262_avg) (var264_min)|])
        in
        let var266_median  =
            ( *. ) (var260_median) (1.0e+3)
        in
        let var267_sum  =
            ( *. ) (var261_sum) (1.0e+3)
        in
        let var268_avg  =
            ( *. ) (var262_avg) (1.0e+3)
        in
        let var269_max  =
            ( *. ) (var263_max) (1.0e+3)
        in
        let var270_min  =
            ( *. ) (var264_min) (1.0e+3)
        in
        let var271_variance  =
            ( *. ) (var265_variance) (1.0e+3)
        in
        let var272__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'='; '='; ' '; 'I'; 'T'; 'E'; 'R'; 'A'; 'T'; 'I'; 'O'; 'N'; ' '; 'R'; 'E'; 'S'; 'U'; 'L'; 'T'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var273__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'i'; 't'; 'e'; 'r'; 'a'; 't'; 'i'; 'o'; 'n'; 's'; ':'; ' '|])
        in
        let var274__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'1'; '5'|])
        in
        let var275__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var276__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var277__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var266_median))
        in
        let var278__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var279__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var280__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var269_max))
        in
        let var281__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var282__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var283__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var270_min))
        in
        let var284__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var285__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var286__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var268_avg))
        in
        let var287__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var288__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var289__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var271_variance))
        in
        let var290__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var291_median  =
            fun226_bm_median (var257_bmres_warmup)
        in
        let var292_sum  =
            fun234_bm_sum (var257_bmres_warmup)
        in
        let var293_avg  =
            ( /. ) (var292_sum) (4.0e+0)
        in
        let var294_max  =
            fun243_bm_max (var257_bmres_warmup)
        in
        let var295_min  =
            fun252_bm_min (var257_bmres_warmup)
        in
        let var296_variance  =
            fun243_bm_max ([|fun255_bm_dist (var293_avg) (var294_max); fun255_bm_dist (var293_avg) (var295_min)|])
        in
        let var297_median  =
            ( *. ) (var291_median) (1.0e+3)
        in
        let var298_sum  =
            ( *. ) (var292_sum) (1.0e+3)
        in
        let var299_avg  =
            ( *. ) (var293_avg) (1.0e+3)
        in
        let var300_max  =
            ( *. ) (var294_max) (1.0e+3)
        in
        let var301_min  =
            ( *. ) (var295_min) (1.0e+3)
        in
        let var302_variance  =
            ( *. ) (var296_variance) (1.0e+3)
        in
        let var303__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; '\n'; '='; '='; ' '; 'W'; 'A'; 'R'; 'M'; 'U'; 'P'; ' '; 'S'; 'T'; 'A'; 'T'; 'I'; 'S'; 'T'; 'I'; 'C'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var304__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'w'; 'a'; 'r'; 'm'; 'u'; 'p'; ' '; 'r'; 'u'; 'n'; 's'; ':'; ' '|])
        in
        let var305__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'4'|])
        in
        let var306__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var307__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var308__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var297_median))
        in
        let var309__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var310__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var311__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var300_max))
        in
        let var312__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var313__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var314__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var301_min))
        in
        let var315__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var316__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var317__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var299_avg))
        in
        let var318__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var319__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var320__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var302_variance))
        in
        let var321__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        ()
    in
    ()