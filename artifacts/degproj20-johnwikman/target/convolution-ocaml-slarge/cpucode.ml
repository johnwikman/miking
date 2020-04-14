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
    let fun199_bm_runonce arg196_matA arg195_filter17 arg194_resCols arg193_resRows arg191__ =
        let var192_bm_t_start  =
            Unix.gettimeofday (())
        in
        let var197_matRes  =
            fun14_seqInit (( * ) (arg193_resRows) (arg194_resCols)) (fun186_convolute17Worker (arg195_filter17) (arg193_resRows) (arg194_resCols) (arg196_matA))
        in
        let var198_bm_t_end  =
            Unix.gettimeofday (())
        in
        ( -. ) (var198_bm_t_end) (var192_bm_t_start)
    in
    let rec fun201_bm_iter arg209_resRows arg208_resCols arg207_filter17 arg206_matA arg204_n arg202_i arg203_acc =
            if ( >= ) (arg202_i) (arg204_n) then
                arg203_acc
            else
                let var205__  =
                    ()
                in
                let var210_res  =
                    fun199_bm_runonce (arg206_matA) (arg207_filter17) (arg208_resCols) (arg209_resRows) (())
                in
                let var211_newacc  =
                    Array.append (arg203_acc) ([|var210_res|])
                in
                fun201_bm_iter (arg209_resRows) (arg208_resCols) (arg207_filter17) (arg206_matA) (arg204_n) (( + ) (arg202_i) (1)) (var211_newacc)
    in
    let fun216_bm_runmultiple arg215_matA arg214_filter17 arg213_resCols arg212_resRows arg200_n =
        fun201_bm_iter (arg212_resRows) (arg213_resCols) (arg214_filter17) (arg215_matA) (arg200_n) (0) ([||])
    in
    let rec fun219_quicksort_rec arg221_pivot arg222_lt_pivot arg223_geq_pivot arg224_remaining =
            if ( = ) (Array.length (arg224_remaining)) (0) then
                let var225_seq_lt  =
                    fun220_quicksort (arg222_lt_pivot)
                in
                let var226_seq_pivot  =
                    [|arg221_pivot|]
                in
                let var227_seq_geq  =
                    fun220_quicksort (arg223_geq_pivot)
                in
                Array.append (Array.append (var225_seq_lt) (var226_seq_pivot)) (var227_seq_geq)
            else
                let var228_e  =
                    fun1_head (arg224_remaining)
                in
                let var229_t  =
                    fun3_tail (arg224_remaining)
                in
                if ( < ) (var228_e) (arg221_pivot) then
                    fun219_quicksort_rec (arg221_pivot) ((fun x xs -> Array.append [|x|] xs) (var228_e) (arg222_lt_pivot)) (arg223_geq_pivot) (var229_t)
                else
                    fun219_quicksort_rec (arg221_pivot) (arg222_lt_pivot) ((fun x xs -> Array.append [|x|] xs) (var228_e) (arg223_geq_pivot)) (var229_t)
        and fun220_quicksort arg230_arr =
            if ( <= ) (Array.length (arg230_arr)) (1) then
                arg230_arr
            else
                fun219_quicksort_rec (fun1_head (arg230_arr)) ([||]) ([||]) (fun3_tail (arg230_arr))
    in
    let fun231_bm_sort arg217_arr =
        let var218_n  =
            Array.length (arg217_arr)
        in
        fun220_quicksort (arg217_arr)
    in
    let fun235_bm_median arg232_arr =
        let var233_n  =
            Array.length (arg232_arr)
        in
        let var234_sorted  =
            fun231_bm_sort (arg232_arr)
        in
        if ( = ) (( mod ) (var233_n) (2)) (0) then
            ( /. ) (( +. ) (Array.get (arg232_arr) (( - ) (( / ) (var233_n) (2)) (1))) (Array.get (arg232_arr) (( / ) (var233_n) (2)))) (2.0e+0)
        else
            Array.get (arg232_arr) (( / ) (var233_n) (2))
    in
    let rec fun238_work arg242_arr arg241_n arg239_i arg240_acc =
            if ( = ) (arg239_i) (arg241_n) then
                arg240_acc
            else
                fun238_work (arg242_arr) (arg241_n) (( + ) (arg239_i) (1)) (( +. ) (arg240_acc) (Array.get (arg242_arr) (arg239_i)))
    in
    let fun243_bm_sum arg236_arr =
        let var237_n  =
            Array.length (arg236_arr)
        in
        fun238_work (arg236_arr) (var237_n) (0) (0.0)
    in
    let rec fun246_work arg250_arr arg249_n arg247_i arg248_acc =
            if ( = ) (arg247_i) (arg249_n) then
                arg248_acc
            else
                let var251_e  =
                    Array.get (arg250_arr) (arg247_i)
                in
                fun246_work (arg250_arr) (arg249_n) (( + ) (arg247_i) (1)) (if ( > ) (var251_e) (arg248_acc) then
                    var251_e
                else
                    arg248_acc)
    in
    let fun252_bm_max arg244_arr =
        let var245_n  =
            Array.length (arg244_arr)
        in
        fun246_work (arg244_arr) (var245_n) (1) (Array.get (arg244_arr) (0))
    in
    let rec fun255_work arg259_arr arg258_n arg256_i arg257_acc =
            if ( = ) (arg256_i) (arg258_n) then
                arg257_acc
            else
                let var260_e  =
                    Array.get (arg259_arr) (arg256_i)
                in
                fun255_work (arg259_arr) (arg258_n) (( + ) (arg256_i) (1)) (if ( < ) (var260_e) (arg257_acc) then
                    var260_e
                else
                    arg257_acc)
    in
    let fun261_bm_min arg253_arr =
        let var254_n  =
            Array.length (arg253_arr)
        in
        fun255_work (arg253_arr) (var254_n) (1) (Array.get (arg253_arr) (0))
    in
    let fun264_bm_dist arg262_a arg263_b =
        if ( > ) (arg262_a) (arg263_b) then
            ( -. ) (arg262_a) (arg263_b)
        else
            ( -. ) (arg263_b) (arg262_a)
    in
    let var161_matA_rows  =
        8192
    in
    let var162_matA_cols  =
        8192
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
    let var265__  =
        ()
    in
    let var266_bmres_warmup  =
        fun216_bm_runmultiple (var187_matA) (var188_filter17) (var190_resCols) (var189_resRows) (4)
    in
    let var267__  =
        ()
    in
    let var268_bmres_iters  =
        fun216_bm_runmultiple (var187_matA) (var188_filter17) (var190_resCols) (var189_resRows) (15)
    in
    let var331__  =
        let var269_median  =
            fun235_bm_median (var268_bmres_iters)
        in
        let var270_sum  =
            fun243_bm_sum (var268_bmres_iters)
        in
        let var271_avg  =
            ( /. ) (var270_sum) (1.50e+1)
        in
        let var272_max  =
            fun252_bm_max (var268_bmres_iters)
        in
        let var273_min  =
            fun261_bm_min (var268_bmres_iters)
        in
        let var274_variance  =
            fun252_bm_max ([|fun264_bm_dist (var271_avg) (var272_max); fun264_bm_dist (var271_avg) (var273_min)|])
        in
        let var275_median  =
            ( *. ) (var269_median) (1.0e+3)
        in
        let var276_sum  =
            ( *. ) (var270_sum) (1.0e+3)
        in
        let var277_avg  =
            ( *. ) (var271_avg) (1.0e+3)
        in
        let var278_max  =
            ( *. ) (var272_max) (1.0e+3)
        in
        let var279_min  =
            ( *. ) (var273_min) (1.0e+3)
        in
        let var280_variance  =
            ( *. ) (var274_variance) (1.0e+3)
        in
        let var281__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'='; '='; ' '; 'I'; 'T'; 'E'; 'R'; 'A'; 'T'; 'I'; 'O'; 'N'; ' '; 'R'; 'E'; 'S'; 'U'; 'L'; 'T'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var282__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'i'; 't'; 'e'; 'r'; 'a'; 't'; 'i'; 'o'; 'n'; 's'; ':'; ' '|])
        in
        let var283__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'1'; '5'|])
        in
        let var284__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var285__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var286__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var275_median))
        in
        let var287__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var288__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var289__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var278_max))
        in
        let var290__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var291__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var292__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var279_min))
        in
        let var293__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var294__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var295__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var277_avg))
        in
        let var296__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var297__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var298__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var280_variance))
        in
        let var299__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var300_median  =
            fun235_bm_median (var266_bmres_warmup)
        in
        let var301_sum  =
            fun243_bm_sum (var266_bmres_warmup)
        in
        let var302_avg  =
            ( /. ) (var301_sum) (4.0e+0)
        in
        let var303_max  =
            fun252_bm_max (var266_bmres_warmup)
        in
        let var304_min  =
            fun261_bm_min (var266_bmres_warmup)
        in
        let var305_variance  =
            fun252_bm_max ([|fun264_bm_dist (var302_avg) (var303_max); fun264_bm_dist (var302_avg) (var304_min)|])
        in
        let var306_median  =
            ( *. ) (var300_median) (1.0e+3)
        in
        let var307_sum  =
            ( *. ) (var301_sum) (1.0e+3)
        in
        let var308_avg  =
            ( *. ) (var302_avg) (1.0e+3)
        in
        let var309_max  =
            ( *. ) (var303_max) (1.0e+3)
        in
        let var310_min  =
            ( *. ) (var304_min) (1.0e+3)
        in
        let var311_variance  =
            ( *. ) (var305_variance) (1.0e+3)
        in
        let var312__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; '\n'; '='; '='; ' '; 'W'; 'A'; 'R'; 'M'; 'U'; 'P'; ' '; 'S'; 'T'; 'A'; 'T'; 'I'; 'S'; 'T'; 'I'; 'C'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var313__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'w'; 'a'; 'r'; 'm'; 'u'; 'p'; ' '; 'r'; 'u'; 'n'; 's'; ':'; ' '|])
        in
        let var314__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'4'|])
        in
        let var315__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var316__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var317__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var306_median))
        in
        let var318__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var319__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var320__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var309_max))
        in
        let var321__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var322__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var323__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var310_min))
        in
        let var324__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var325__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var326__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var308_avg))
        in
        let var327__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var328__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var329__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var311_variance))
        in
        let var330__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        ()
    in
    ()