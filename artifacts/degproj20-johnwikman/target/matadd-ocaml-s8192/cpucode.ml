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
    let fun168_matBinitfun_v2 arg166_row arg167_col =
        float_of_int (( mod ) (( / ) (( * ) (( + ) (arg166_row) (19)) (17)) (( + ) (arg167_col) (13))) (2))
    in
    let fun172_matrixAddfWorker arg169_A arg170_B arg171_idx =
        ( +. ) (Array.get (arg169_A) (arg171_idx)) (Array.get (arg170_B) (arg171_idx))
    in
    let fun183_bm_runonce arg180_matB arg179_matA arg178_mat_cols arg177_mat_rows arg175__ =
        let var176_bm_t_start  =
            Unix.gettimeofday (())
        in
        let var181_matAplusB  =
            fun14_seqInit (( * ) (arg177_mat_rows) (arg178_mat_cols)) (fun172_matrixAddfWorker (arg179_matA) (arg180_matB))
        in
        let var182_bm_t_end  =
            Unix.gettimeofday (())
        in
        ( -. ) (var182_bm_t_end) (var176_bm_t_start)
    in
    let rec fun185_bm_iter arg193_mat_rows arg192_mat_cols arg191_matA arg190_matB arg188_n arg186_i arg187_acc =
            if ( >= ) (arg186_i) (arg188_n) then
                arg187_acc
            else
                let var189__  =
                    ()
                in
                let var194_res  =
                    fun183_bm_runonce (arg190_matB) (arg191_matA) (arg192_mat_cols) (arg193_mat_rows) (())
                in
                let var195_newacc  =
                    Array.append (arg187_acc) ([|var194_res|])
                in
                fun185_bm_iter (arg193_mat_rows) (arg192_mat_cols) (arg191_matA) (arg190_matB) (arg188_n) (( + ) (arg186_i) (1)) (var195_newacc)
    in
    let fun200_bm_runmultiple arg199_matB arg198_matA arg197_mat_cols arg196_mat_rows arg184_n =
        fun185_bm_iter (arg196_mat_rows) (arg197_mat_cols) (arg198_matA) (arg199_matB) (arg184_n) (0) ([||])
    in
    let rec fun203_quicksort_rec arg205_pivot arg206_lt_pivot arg207_geq_pivot arg208_remaining =
            if ( = ) (Array.length (arg208_remaining)) (0) then
                let var209_seq_lt  =
                    fun204_quicksort (arg206_lt_pivot)
                in
                let var210_seq_pivot  =
                    [|arg205_pivot|]
                in
                let var211_seq_geq  =
                    fun204_quicksort (arg207_geq_pivot)
                in
                Array.append (Array.append (var209_seq_lt) (var210_seq_pivot)) (var211_seq_geq)
            else
                let var212_e  =
                    fun1_head (arg208_remaining)
                in
                let var213_t  =
                    fun3_tail (arg208_remaining)
                in
                if ( < ) (var212_e) (arg205_pivot) then
                    fun203_quicksort_rec (arg205_pivot) ((fun x xs -> Array.append [|x|] xs) (var212_e) (arg206_lt_pivot)) (arg207_geq_pivot) (var213_t)
                else
                    fun203_quicksort_rec (arg205_pivot) (arg206_lt_pivot) ((fun x xs -> Array.append [|x|] xs) (var212_e) (arg207_geq_pivot)) (var213_t)
        and fun204_quicksort arg214_arr =
            if ( <= ) (Array.length (arg214_arr)) (1) then
                arg214_arr
            else
                fun203_quicksort_rec (fun1_head (arg214_arr)) ([||]) ([||]) (fun3_tail (arg214_arr))
    in
    let fun215_bm_sort arg201_arr =
        let var202_n  =
            Array.length (arg201_arr)
        in
        fun204_quicksort (arg201_arr)
    in
    let fun219_bm_median arg216_arr =
        let var217_n  =
            Array.length (arg216_arr)
        in
        let var218_sorted  =
            fun215_bm_sort (arg216_arr)
        in
        if ( = ) (( mod ) (var217_n) (2)) (0) then
            ( /. ) (( +. ) (Array.get (arg216_arr) (( - ) (( / ) (var217_n) (2)) (1))) (Array.get (arg216_arr) (( / ) (var217_n) (2)))) (2.0e+0)
        else
            Array.get (arg216_arr) (( / ) (var217_n) (2))
    in
    let rec fun222_work arg226_arr arg225_n arg223_i arg224_acc =
            if ( = ) (arg223_i) (arg225_n) then
                arg224_acc
            else
                fun222_work (arg226_arr) (arg225_n) (( + ) (arg223_i) (1)) (( +. ) (arg224_acc) (Array.get (arg226_arr) (arg223_i)))
    in
    let fun227_bm_sum arg220_arr =
        let var221_n  =
            Array.length (arg220_arr)
        in
        fun222_work (arg220_arr) (var221_n) (0) (0.0)
    in
    let rec fun230_work arg234_arr arg233_n arg231_i arg232_acc =
            if ( = ) (arg231_i) (arg233_n) then
                arg232_acc
            else
                let var235_e  =
                    Array.get (arg234_arr) (arg231_i)
                in
                fun230_work (arg234_arr) (arg233_n) (( + ) (arg231_i) (1)) (if ( > ) (var235_e) (arg232_acc) then
                    var235_e
                else
                    arg232_acc)
    in
    let fun236_bm_max arg228_arr =
        let var229_n  =
            Array.length (arg228_arr)
        in
        fun230_work (arg228_arr) (var229_n) (1) (Array.get (arg228_arr) (0))
    in
    let rec fun239_work arg243_arr arg242_n arg240_i arg241_acc =
            if ( = ) (arg240_i) (arg242_n) then
                arg241_acc
            else
                let var244_e  =
                    Array.get (arg243_arr) (arg240_i)
                in
                fun239_work (arg243_arr) (arg242_n) (( + ) (arg240_i) (1)) (if ( < ) (var244_e) (arg241_acc) then
                    var244_e
                else
                    arg241_acc)
    in
    let fun245_bm_min arg237_arr =
        let var238_n  =
            Array.length (arg237_arr)
        in
        fun239_work (arg237_arr) (var238_n) (1) (Array.get (arg237_arr) (0))
    in
    let fun248_bm_dist arg246_a arg247_b =
        if ( > ) (arg246_a) (arg247_b) then
            ( -. ) (arg246_a) (arg247_b)
        else
            ( -. ) (arg247_b) (arg246_a)
    in
    let var161_mat_rows  =
        8192
    in
    let var162_mat_cols  =
        8192
    in
    let var173_matA  =
        fun96_matrixInitf (var161_mat_rows) (var162_mat_cols) (fun165_matAinitfun_v2)
    in
    let var174_matB  =
        fun96_matrixInitf (var161_mat_rows) (var162_mat_cols) (fun168_matBinitfun_v2)
    in
    let var249__  =
        ()
    in
    let var250_bmres_warmup  =
        fun200_bm_runmultiple (var174_matB) (var173_matA) (var162_mat_cols) (var161_mat_rows) (4)
    in
    let var251__  =
        ()
    in
    let var252_bmres_iters  =
        fun200_bm_runmultiple (var174_matB) (var173_matA) (var162_mat_cols) (var161_mat_rows) (15)
    in
    let var315__  =
        let var253_median  =
            fun219_bm_median (var252_bmres_iters)
        in
        let var254_sum  =
            fun227_bm_sum (var252_bmres_iters)
        in
        let var255_avg  =
            ( /. ) (var254_sum) (1.50e+1)
        in
        let var256_max  =
            fun236_bm_max (var252_bmres_iters)
        in
        let var257_min  =
            fun245_bm_min (var252_bmres_iters)
        in
        let var258_variance  =
            fun236_bm_max ([|fun248_bm_dist (var255_avg) (var256_max); fun248_bm_dist (var255_avg) (var257_min)|])
        in
        let var259_median  =
            ( *. ) (var253_median) (1.0e+3)
        in
        let var260_sum  =
            ( *. ) (var254_sum) (1.0e+3)
        in
        let var261_avg  =
            ( *. ) (var255_avg) (1.0e+3)
        in
        let var262_max  =
            ( *. ) (var256_max) (1.0e+3)
        in
        let var263_min  =
            ( *. ) (var257_min) (1.0e+3)
        in
        let var264_variance  =
            ( *. ) (var258_variance) (1.0e+3)
        in
        let var265__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'='; '='; ' '; 'I'; 'T'; 'E'; 'R'; 'A'; 'T'; 'I'; 'O'; 'N'; ' '; 'R'; 'E'; 'S'; 'U'; 'L'; 'T'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var266__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'i'; 't'; 'e'; 'r'; 'a'; 't'; 'i'; 'o'; 'n'; 's'; ':'; ' '|])
        in
        let var267__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'1'; '5'|])
        in
        let var268__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var269__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var270__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var259_median))
        in
        let var271__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var272__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var273__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var262_max))
        in
        let var274__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var275__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var276__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var263_min))
        in
        let var277__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var278__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var279__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var261_avg))
        in
        let var280__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var281__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var282__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var264_variance))
        in
        let var283__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var284_median  =
            fun219_bm_median (var250_bmres_warmup)
        in
        let var285_sum  =
            fun227_bm_sum (var250_bmres_warmup)
        in
        let var286_avg  =
            ( /. ) (var285_sum) (4.0e+0)
        in
        let var287_max  =
            fun236_bm_max (var250_bmres_warmup)
        in
        let var288_min  =
            fun245_bm_min (var250_bmres_warmup)
        in
        let var289_variance  =
            fun236_bm_max ([|fun248_bm_dist (var286_avg) (var287_max); fun248_bm_dist (var286_avg) (var288_min)|])
        in
        let var290_median  =
            ( *. ) (var284_median) (1.0e+3)
        in
        let var291_sum  =
            ( *. ) (var285_sum) (1.0e+3)
        in
        let var292_avg  =
            ( *. ) (var286_avg) (1.0e+3)
        in
        let var293_max  =
            ( *. ) (var287_max) (1.0e+3)
        in
        let var294_min  =
            ( *. ) (var288_min) (1.0e+3)
        in
        let var295_variance  =
            ( *. ) (var289_variance) (1.0e+3)
        in
        let var296__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; '\n'; '='; '='; ' '; 'W'; 'A'; 'R'; 'M'; 'U'; 'P'; ' '; 'S'; 'T'; 'A'; 'T'; 'I'; 'S'; 'T'; 'I'; 'C'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var297__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'w'; 'a'; 'r'; 'm'; 'u'; 'p'; ' '; 'r'; 'u'; 'n'; 's'; ':'; ' '|])
        in
        let var298__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'4'|])
        in
        let var299__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var300__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var301__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var290_median))
        in
        let var302__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var303__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var304__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var293_max))
        in
        let var305__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var306__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var307__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var294_min))
        in
        let var308__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var309__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var310__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var292_avg))
        in
        let var311__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var312__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var313__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var295_variance))
        in
        let var314__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        ()
    in
    ()