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
    let fun182_bm_runonce arg179_vecX arg178_matA arg177_matA_rows arg176_vecX_cols arg175_matA_cols arg173__ =
        let var174_bm_t_start  =
            Unix.gettimeofday (())
        in
        let var180_vecAx  =
            fun14_seqInit (( * ) (arg175_matA_cols) (arg176_vecX_cols)) (fun141_matrixMulfWorker (arg175_matA_cols) (arg177_matA_rows) (arg176_vecX_cols) (arg178_matA) (arg179_vecX))
        in
        let var181_bm_t_end  =
            Unix.gettimeofday (())
        in
        ( -. ) (var181_bm_t_end) (var174_bm_t_start)
    in
    let rec fun184_bm_iter arg193_matA_cols arg192_vecX_cols arg191_matA_rows arg190_matA arg189_vecX arg187_n arg185_i arg186_acc =
            if ( >= ) (arg185_i) (arg187_n) then
                arg186_acc
            else
                let var188__  =
                    ()
                in
                let var194_res  =
                    fun182_bm_runonce (arg189_vecX) (arg190_matA) (arg191_matA_rows) (arg192_vecX_cols) (arg193_matA_cols) (())
                in
                let var195_newacc  =
                    Array.append (arg186_acc) ([|var194_res|])
                in
                fun184_bm_iter (arg193_matA_cols) (arg192_vecX_cols) (arg191_matA_rows) (arg190_matA) (arg189_vecX) (arg187_n) (( + ) (arg185_i) (1)) (var195_newacc)
    in
    let fun201_bm_runmultiple arg200_vecX arg199_matA arg198_matA_rows arg197_vecX_cols arg196_matA_cols arg183_n =
        fun184_bm_iter (arg196_matA_cols) (arg197_vecX_cols) (arg198_matA_rows) (arg199_matA) (arg200_vecX) (arg183_n) (0) ([||])
    in
    let rec fun204_quicksort_rec arg206_pivot arg207_lt_pivot arg208_geq_pivot arg209_remaining =
            if ( = ) (Array.length (arg209_remaining)) (0) then
                let var210_seq_lt  =
                    fun205_quicksort (arg207_lt_pivot)
                in
                let var211_seq_pivot  =
                    [|arg206_pivot|]
                in
                let var212_seq_geq  =
                    fun205_quicksort (arg208_geq_pivot)
                in
                Array.append (Array.append (var210_seq_lt) (var211_seq_pivot)) (var212_seq_geq)
            else
                let var213_e  =
                    fun1_head (arg209_remaining)
                in
                let var214_t  =
                    fun3_tail (arg209_remaining)
                in
                if ( < ) (var213_e) (arg206_pivot) then
                    fun204_quicksort_rec (arg206_pivot) ((fun x xs -> Array.append [|x|] xs) (var213_e) (arg207_lt_pivot)) (arg208_geq_pivot) (var214_t)
                else
                    fun204_quicksort_rec (arg206_pivot) (arg207_lt_pivot) ((fun x xs -> Array.append [|x|] xs) (var213_e) (arg208_geq_pivot)) (var214_t)
        and fun205_quicksort arg215_arr =
            if ( <= ) (Array.length (arg215_arr)) (1) then
                arg215_arr
            else
                fun204_quicksort_rec (fun1_head (arg215_arr)) ([||]) ([||]) (fun3_tail (arg215_arr))
    in
    let fun216_bm_sort arg202_arr =
        let var203_n  =
            Array.length (arg202_arr)
        in
        fun205_quicksort (arg202_arr)
    in
    let fun220_bm_median arg217_arr =
        let var218_n  =
            Array.length (arg217_arr)
        in
        let var219_sorted  =
            fun216_bm_sort (arg217_arr)
        in
        if ( = ) (( mod ) (var218_n) (2)) (0) then
            ( /. ) (( +. ) (Array.get (arg217_arr) (( - ) (( / ) (var218_n) (2)) (1))) (Array.get (arg217_arr) (( / ) (var218_n) (2)))) (2.0e+0)
        else
            Array.get (arg217_arr) (( / ) (var218_n) (2))
    in
    let rec fun223_work arg227_arr arg226_n arg224_i arg225_acc =
            if ( = ) (arg224_i) (arg226_n) then
                arg225_acc
            else
                fun223_work (arg227_arr) (arg226_n) (( + ) (arg224_i) (1)) (( +. ) (arg225_acc) (Array.get (arg227_arr) (arg224_i)))
    in
    let fun228_bm_sum arg221_arr =
        let var222_n  =
            Array.length (arg221_arr)
        in
        fun223_work (arg221_arr) (var222_n) (0) (0.0)
    in
    let rec fun231_work arg235_arr arg234_n arg232_i arg233_acc =
            if ( = ) (arg232_i) (arg234_n) then
                arg233_acc
            else
                let var236_e  =
                    Array.get (arg235_arr) (arg232_i)
                in
                fun231_work (arg235_arr) (arg234_n) (( + ) (arg232_i) (1)) (if ( > ) (var236_e) (arg233_acc) then
                    var236_e
                else
                    arg233_acc)
    in
    let fun237_bm_max arg229_arr =
        let var230_n  =
            Array.length (arg229_arr)
        in
        fun231_work (arg229_arr) (var230_n) (1) (Array.get (arg229_arr) (0))
    in
    let rec fun240_work arg244_arr arg243_n arg241_i arg242_acc =
            if ( = ) (arg241_i) (arg243_n) then
                arg242_acc
            else
                let var245_e  =
                    Array.get (arg244_arr) (arg241_i)
                in
                fun240_work (arg244_arr) (arg243_n) (( + ) (arg241_i) (1)) (if ( < ) (var245_e) (arg242_acc) then
                    var245_e
                else
                    arg242_acc)
    in
    let fun246_bm_min arg238_arr =
        let var239_n  =
            Array.length (arg238_arr)
        in
        fun240_work (arg238_arr) (var239_n) (1) (Array.get (arg238_arr) (0))
    in
    let fun249_bm_dist arg247_a arg248_b =
        if ( > ) (arg247_a) (arg248_b) then
            ( -. ) (arg247_a) (arg248_b)
        else
            ( -. ) (arg248_b) (arg247_a)
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
    let var250__  =
        ()
    in
    let var251_bmres_warmup  =
        fun201_bm_runmultiple (var172_vecX) (var169_matA) (var161_matA_rows) (var171_vecX_cols) (var162_matA_cols) (4)
    in
    let var252__  =
        ()
    in
    let var253_bmres_iters  =
        fun201_bm_runmultiple (var172_vecX) (var169_matA) (var161_matA_rows) (var171_vecX_cols) (var162_matA_cols) (15)
    in
    let var316__  =
        let var254_median  =
            fun220_bm_median (var253_bmres_iters)
        in
        let var255_sum  =
            fun228_bm_sum (var253_bmres_iters)
        in
        let var256_avg  =
            ( /. ) (var255_sum) (1.50e+1)
        in
        let var257_max  =
            fun237_bm_max (var253_bmres_iters)
        in
        let var258_min  =
            fun246_bm_min (var253_bmres_iters)
        in
        let var259_variance  =
            fun237_bm_max ([|fun249_bm_dist (var256_avg) (var257_max); fun249_bm_dist (var256_avg) (var258_min)|])
        in
        let var260_median  =
            ( *. ) (var254_median) (1.0e+3)
        in
        let var261_sum  =
            ( *. ) (var255_sum) (1.0e+3)
        in
        let var262_avg  =
            ( *. ) (var256_avg) (1.0e+3)
        in
        let var263_max  =
            ( *. ) (var257_max) (1.0e+3)
        in
        let var264_min  =
            ( *. ) (var258_min) (1.0e+3)
        in
        let var265_variance  =
            ( *. ) (var259_variance) (1.0e+3)
        in
        let var266__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'='; '='; ' '; 'I'; 'T'; 'E'; 'R'; 'A'; 'T'; 'I'; 'O'; 'N'; ' '; 'R'; 'E'; 'S'; 'U'; 'L'; 'T'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var267__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'i'; 't'; 'e'; 'r'; 'a'; 't'; 'i'; 'o'; 'n'; 's'; ':'; ' '|])
        in
        let var268__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'1'; '5'|])
        in
        let var269__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var270__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var271__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var260_median))
        in
        let var272__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var273__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var274__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var263_max))
        in
        let var275__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var276__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var277__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var264_min))
        in
        let var278__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var279__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var280__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var262_avg))
        in
        let var281__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var282__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var283__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var265_variance))
        in
        let var284__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var285_median  =
            fun220_bm_median (var251_bmres_warmup)
        in
        let var286_sum  =
            fun228_bm_sum (var251_bmres_warmup)
        in
        let var287_avg  =
            ( /. ) (var286_sum) (4.0e+0)
        in
        let var288_max  =
            fun237_bm_max (var251_bmres_warmup)
        in
        let var289_min  =
            fun246_bm_min (var251_bmres_warmup)
        in
        let var290_variance  =
            fun237_bm_max ([|fun249_bm_dist (var287_avg) (var288_max); fun249_bm_dist (var287_avg) (var289_min)|])
        in
        let var291_median  =
            ( *. ) (var285_median) (1.0e+3)
        in
        let var292_sum  =
            ( *. ) (var286_sum) (1.0e+3)
        in
        let var293_avg  =
            ( *. ) (var287_avg) (1.0e+3)
        in
        let var294_max  =
            ( *. ) (var288_max) (1.0e+3)
        in
        let var295_min  =
            ( *. ) (var289_min) (1.0e+3)
        in
        let var296_variance  =
            ( *. ) (var290_variance) (1.0e+3)
        in
        let var297__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; '\n'; '='; '='; ' '; 'W'; 'A'; 'R'; 'M'; 'U'; 'P'; ' '; 'S'; 'T'; 'A'; 'T'; 'I'; 'S'; 'T'; 'I'; 'C'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var298__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'w'; 'a'; 'r'; 'm'; 'u'; 'p'; ' '; 'r'; 'u'; 'n'; 's'; ':'; ' '|])
        in
        let var299__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'4'|])
        in
        let var300__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var301__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var302__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var291_median))
        in
        let var303__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var304__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var305__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var294_max))
        in
        let var306__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var307__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var308__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var295_min))
        in
        let var309__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var310__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var311__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var293_avg))
        in
        let var312__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var313__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var314__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var296_variance))
        in
        let var315__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        ()
    in
    ()