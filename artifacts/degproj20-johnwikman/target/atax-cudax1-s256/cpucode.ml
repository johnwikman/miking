open Printf

external gpuhost_fun141_matrixMulfWorker: int array -> float array -> float array -> float array -> float array = "gpuhost_fun141_matrixMulfWorker"
external gpuhost_fun160_matrixATAfWorker: int array -> float array -> float array -> float array = "gpuhost_fun160_matrixATAfWorker"

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
    let fun167_matAinitfun_v2 arg165_row arg166_col =
        ( -. ) (( /. ) (float_of_int (( + ) (( * ) (arg165_row) (arg165_row)) (arg166_col))) (3.0e+0)) (1.400000e-2)
    in
    let fun170_vecXinitfun arg168_row arg169_col =
        float_of_int (( mod ) (( * ) (arg168_row) (10657)) (41081))
    in
    let fun187_bm_runonce arg184_vecX arg183_vecX_cols arg182_vecX_cols arg180_matA arg179_matA_cols arg178_matA_rows arg177_matA_cols arg175__ =
        let var176_bm_t_start  =
            Unix.gettimeofday (())
        in
        let var181_matATA  =
            gpuhost_fun160_matrixATAfWorker [|1; arg178_matA_rows; arg179_matA_cols; ( * ) (arg177_matA_cols) (arg177_matA_cols)|] [||] (arg180_matA)
        in
        let var185_vecATAx  =
            gpuhost_fun141_matrixMulfWorker [|1; arg177_matA_cols; arg177_matA_cols; arg183_vecX_cols; ( * ) (arg177_matA_cols) (arg182_vecX_cols)|] [||] (var181_matATA) (arg184_vecX)
        in
        let var186_bm_t_end  =
            Unix.gettimeofday (())
        in
        ( -. ) (var186_bm_t_end) (var176_bm_t_start)
    in
    let rec fun189_bm_iter arg200_matA_cols arg199_matA_rows arg198_matA_cols arg197_matA arg196_vecX_cols arg195_vecX_cols arg194_vecX arg192_n arg190_i arg191_acc =
            if ( >= ) (arg190_i) (arg192_n) then
                arg191_acc
            else
                let var193__  =
                    ()
                in
                let var201_res  =
                    fun187_bm_runonce (arg194_vecX) (arg195_vecX_cols) (arg196_vecX_cols) (arg197_matA) (arg198_matA_cols) (arg199_matA_rows) (arg200_matA_cols) (())
                in
                let var202_newacc  =
                    Array.append (arg191_acc) ([|var201_res|])
                in
                fun189_bm_iter (arg200_matA_cols) (arg199_matA_rows) (arg198_matA_cols) (arg197_matA) (arg196_vecX_cols) (arg195_vecX_cols) (arg194_vecX) (arg192_n) (( + ) (arg190_i) (1)) (var202_newacc)
    in
    let fun210_bm_runmultiple arg209_vecX arg208_vecX_cols arg207_vecX_cols arg206_matA arg205_matA_cols arg204_matA_rows arg203_matA_cols arg188_n =
        fun189_bm_iter (arg203_matA_cols) (arg204_matA_rows) (arg205_matA_cols) (arg206_matA) (arg207_vecX_cols) (arg208_vecX_cols) (arg209_vecX) (arg188_n) (0) ([||])
    in
    let rec fun213_quicksort_rec arg215_pivot arg216_lt_pivot arg217_geq_pivot arg218_remaining =
            if ( = ) (Array.length (arg218_remaining)) (0) then
                let var219_seq_lt  =
                    fun214_quicksort (arg216_lt_pivot)
                in
                let var220_seq_pivot  =
                    [|arg215_pivot|]
                in
                let var221_seq_geq  =
                    fun214_quicksort (arg217_geq_pivot)
                in
                Array.append (Array.append (var219_seq_lt) (var220_seq_pivot)) (var221_seq_geq)
            else
                let var222_e  =
                    fun1_head (arg218_remaining)
                in
                let var223_t  =
                    fun3_tail (arg218_remaining)
                in
                if ( < ) (var222_e) (arg215_pivot) then
                    fun213_quicksort_rec (arg215_pivot) ((fun x xs -> Array.append [|x|] xs) (var222_e) (arg216_lt_pivot)) (arg217_geq_pivot) (var223_t)
                else
                    fun213_quicksort_rec (arg215_pivot) (arg216_lt_pivot) ((fun x xs -> Array.append [|x|] xs) (var222_e) (arg217_geq_pivot)) (var223_t)
        and fun214_quicksort arg224_arr =
            if ( <= ) (Array.length (arg224_arr)) (1) then
                arg224_arr
            else
                fun213_quicksort_rec (fun1_head (arg224_arr)) ([||]) ([||]) (fun3_tail (arg224_arr))
    in
    let fun225_bm_sort arg211_arr =
        let var212_n  =
            Array.length (arg211_arr)
        in
        fun214_quicksort (arg211_arr)
    in
    let fun229_bm_median arg226_arr =
        let var227_n  =
            Array.length (arg226_arr)
        in
        let var228_sorted  =
            fun225_bm_sort (arg226_arr)
        in
        if ( = ) (( mod ) (var227_n) (2)) (0) then
            ( /. ) (( +. ) (Array.get (arg226_arr) (( - ) (( / ) (var227_n) (2)) (1))) (Array.get (arg226_arr) (( / ) (var227_n) (2)))) (2.0e+0)
        else
            Array.get (arg226_arr) (( / ) (var227_n) (2))
    in
    let rec fun232_work arg236_arr arg235_n arg233_i arg234_acc =
            if ( = ) (arg233_i) (arg235_n) then
                arg234_acc
            else
                fun232_work (arg236_arr) (arg235_n) (( + ) (arg233_i) (1)) (( +. ) (arg234_acc) (Array.get (arg236_arr) (arg233_i)))
    in
    let fun237_bm_sum arg230_arr =
        let var231_n  =
            Array.length (arg230_arr)
        in
        fun232_work (arg230_arr) (var231_n) (0) (0.0)
    in
    let rec fun240_work arg244_arr arg243_n arg241_i arg242_acc =
            if ( = ) (arg241_i) (arg243_n) then
                arg242_acc
            else
                let var245_e  =
                    Array.get (arg244_arr) (arg241_i)
                in
                fun240_work (arg244_arr) (arg243_n) (( + ) (arg241_i) (1)) (if ( > ) (var245_e) (arg242_acc) then
                    var245_e
                else
                    arg242_acc)
    in
    let fun246_bm_max arg238_arr =
        let var239_n  =
            Array.length (arg238_arr)
        in
        fun240_work (arg238_arr) (var239_n) (1) (Array.get (arg238_arr) (0))
    in
    let rec fun249_work arg253_arr arg252_n arg250_i arg251_acc =
            if ( = ) (arg250_i) (arg252_n) then
                arg251_acc
            else
                let var254_e  =
                    Array.get (arg253_arr) (arg250_i)
                in
                fun249_work (arg253_arr) (arg252_n) (( + ) (arg250_i) (1)) (if ( < ) (var254_e) (arg251_acc) then
                    var254_e
                else
                    arg251_acc)
    in
    let fun255_bm_min arg247_arr =
        let var248_n  =
            Array.length (arg247_arr)
        in
        fun249_work (arg247_arr) (var248_n) (1) (Array.get (arg247_arr) (0))
    in
    let fun258_bm_dist arg256_a arg257_b =
        if ( > ) (arg256_a) (arg257_b) then
            ( -. ) (arg256_a) (arg257_b)
        else
            ( -. ) (arg257_b) (arg256_a)
    in
    let var161_matA_rows  =
        256
    in
    let var162_matA_cols  =
        256
    in
    let var163_matB_rows  =
        256
    in
    let var164_matB_cols  =
        256
    in
    let var171_matA  =
        fun96_matrixInitf (var161_matA_rows) (var162_matA_cols) (fun167_matAinitfun_v2)
    in
    let var172_vecX_rows  =
        var162_matA_cols
    in
    let var173_vecX_cols  =
        1
    in
    let var174_vecX  =
        fun96_matrixInitf (var172_vecX_rows) (var173_vecX_cols) (fun170_vecXinitfun)
    in
    let var259__  =
        ()
    in
    let var260_bmres_warmup  =
        fun210_bm_runmultiple (var174_vecX) (var173_vecX_cols) (var173_vecX_cols) (var171_matA) (var162_matA_cols) (var161_matA_rows) (var162_matA_cols) (4)
    in
    let var261__  =
        ()
    in
    let var262_bmres_iters  =
        fun210_bm_runmultiple (var174_vecX) (var173_vecX_cols) (var173_vecX_cols) (var171_matA) (var162_matA_cols) (var161_matA_rows) (var162_matA_cols) (15)
    in
    let var325__  =
        let var263_median  =
            fun229_bm_median (var262_bmres_iters)
        in
        let var264_sum  =
            fun237_bm_sum (var262_bmres_iters)
        in
        let var265_avg  =
            ( /. ) (var264_sum) (1.50e+1)
        in
        let var266_max  =
            fun246_bm_max (var262_bmres_iters)
        in
        let var267_min  =
            fun255_bm_min (var262_bmres_iters)
        in
        let var268_variance  =
            fun246_bm_max ([|fun258_bm_dist (var265_avg) (var266_max); fun258_bm_dist (var265_avg) (var267_min)|])
        in
        let var269_median  =
            ( *. ) (var263_median) (1.0e+3)
        in
        let var270_sum  =
            ( *. ) (var264_sum) (1.0e+3)
        in
        let var271_avg  =
            ( *. ) (var265_avg) (1.0e+3)
        in
        let var272_max  =
            ( *. ) (var266_max) (1.0e+3)
        in
        let var273_min  =
            ( *. ) (var267_min) (1.0e+3)
        in
        let var274_variance  =
            ( *. ) (var268_variance) (1.0e+3)
        in
        let var275__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'='; '='; ' '; 'I'; 'T'; 'E'; 'R'; 'A'; 'T'; 'I'; 'O'; 'N'; ' '; 'R'; 'E'; 'S'; 'U'; 'L'; 'T'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var276__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'i'; 't'; 'e'; 'r'; 'a'; 't'; 'i'; 'o'; 'n'; 's'; ':'; ' '|])
        in
        let var277__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'1'; '5'|])
        in
        let var278__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var279__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var280__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var269_median))
        in
        let var281__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var282__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var283__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var272_max))
        in
        let var284__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var285__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var286__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var273_min))
        in
        let var287__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var288__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var289__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var271_avg))
        in
        let var290__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var291__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var292__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var274_variance))
        in
        let var293__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var294_median  =
            fun229_bm_median (var260_bmres_warmup)
        in
        let var295_sum  =
            fun237_bm_sum (var260_bmres_warmup)
        in
        let var296_avg  =
            ( /. ) (var295_sum) (4.0e+0)
        in
        let var297_max  =
            fun246_bm_max (var260_bmres_warmup)
        in
        let var298_min  =
            fun255_bm_min (var260_bmres_warmup)
        in
        let var299_variance  =
            fun246_bm_max ([|fun258_bm_dist (var296_avg) (var297_max); fun258_bm_dist (var296_avg) (var298_min)|])
        in
        let var300_median  =
            ( *. ) (var294_median) (1.0e+3)
        in
        let var301_sum  =
            ( *. ) (var295_sum) (1.0e+3)
        in
        let var302_avg  =
            ( *. ) (var296_avg) (1.0e+3)
        in
        let var303_max  =
            ( *. ) (var297_max) (1.0e+3)
        in
        let var304_min  =
            ( *. ) (var298_min) (1.0e+3)
        in
        let var305_variance  =
            ( *. ) (var299_variance) (1.0e+3)
        in
        let var306__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; '\n'; '='; '='; ' '; 'W'; 'A'; 'R'; 'M'; 'U'; 'P'; ' '; 'S'; 'T'; 'A'; 'T'; 'I'; 'S'; 'T'; 'I'; 'C'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var307__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'w'; 'a'; 'r'; 'm'; 'u'; 'p'; ' '; 'r'; 'u'; 'n'; 's'; ':'; ' '|])
        in
        let var308__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'4'|])
        in
        let var309__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var310__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var311__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var300_median))
        in
        let var312__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var313__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var314__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var303_max))
        in
        let var315__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var316__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var317__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var304_min))
        in
        let var318__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var319__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var320__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var302_avg))
        in
        let var321__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var322__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var323__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var305_variance))
        in
        let var324__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        ()
    in
    ()