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
    let fun173_gerWorkerf arg163_rows arg164_cols arg165_x arg166_y arg167_i arg168_Aval =
        let var169_row  =
            ( / ) (arg167_i) (arg164_cols)
        in
        let var170_col  =
            ( mod ) (arg167_i) (arg164_cols)
        in
        let var171_xval  =
            Array.get (arg165_x) (var169_row)
        in
        let var172_yval  =
            Array.get (arg166_y) (var170_col)
        in
        ( +. ) (( *. ) (var171_xval) (var172_yval)) (arg168_Aval)
    in
    let fun176_matAinitfun_v2 arg174_row arg175_col =
        ( -. ) (( /. ) (float_of_int (( + ) (( * ) (arg174_row) (arg174_row)) (arg175_col))) (3.0e+0)) (1.400000e-2)
    in
    let fun179_vecXinitfun arg177_row arg178_col =
        float_of_int (( mod ) (( * ) (arg177_row) (10657)) (41081))
    in
    let fun182_vecYinitfun arg180_row arg181_col =
        float_of_int (( mod ) (( * ) (arg180_row) (58437)) (84391))
    in
    let fun199_bm_runonce arg196_matA arg195_vecY arg194_vecX arg193_matA_cols arg192_matA_rows arg190__ =
        let var191_bm_t_start  =
            Unix.gettimeofday (())
        in
        let var197_matxyA  =
            fun11_mapi (fun173_gerWorkerf (arg192_matA_rows) (arg193_matA_cols) (arg194_vecX) (arg195_vecY)) (arg196_matA)
        in
        let var198_bm_t_end  =
            Unix.gettimeofday (())
        in
        ( -. ) (var198_bm_t_end) (var191_bm_t_start)
    in
    let rec fun201_bm_iter arg210_matA_rows arg209_matA_cols arg208_vecX arg207_vecY arg206_matA arg204_n arg202_i arg203_acc =
            if ( >= ) (arg202_i) (arg204_n) then
                arg203_acc
            else
                let var205__  =
                    ()
                in
                let var211_res  =
                    fun199_bm_runonce (arg206_matA) (arg207_vecY) (arg208_vecX) (arg209_matA_cols) (arg210_matA_rows) (())
                in
                let var212_newacc  =
                    Array.append (arg203_acc) ([|var211_res|])
                in
                fun201_bm_iter (arg210_matA_rows) (arg209_matA_cols) (arg208_vecX) (arg207_vecY) (arg206_matA) (arg204_n) (( + ) (arg202_i) (1)) (var212_newacc)
    in
    let fun218_bm_runmultiple arg217_matA arg216_vecY arg215_vecX arg214_matA_cols arg213_matA_rows arg200_n =
        fun201_bm_iter (arg213_matA_rows) (arg214_matA_cols) (arg215_vecX) (arg216_vecY) (arg217_matA) (arg200_n) (0) ([||])
    in
    let rec fun221_quicksort_rec arg223_pivot arg224_lt_pivot arg225_geq_pivot arg226_remaining =
            if ( = ) (Array.length (arg226_remaining)) (0) then
                let var227_seq_lt  =
                    fun222_quicksort (arg224_lt_pivot)
                in
                let var228_seq_pivot  =
                    [|arg223_pivot|]
                in
                let var229_seq_geq  =
                    fun222_quicksort (arg225_geq_pivot)
                in
                Array.append (Array.append (var227_seq_lt) (var228_seq_pivot)) (var229_seq_geq)
            else
                let var230_e  =
                    fun1_head (arg226_remaining)
                in
                let var231_t  =
                    fun3_tail (arg226_remaining)
                in
                if ( < ) (var230_e) (arg223_pivot) then
                    fun221_quicksort_rec (arg223_pivot) ((fun x xs -> Array.append [|x|] xs) (var230_e) (arg224_lt_pivot)) (arg225_geq_pivot) (var231_t)
                else
                    fun221_quicksort_rec (arg223_pivot) (arg224_lt_pivot) ((fun x xs -> Array.append [|x|] xs) (var230_e) (arg225_geq_pivot)) (var231_t)
        and fun222_quicksort arg232_arr =
            if ( <= ) (Array.length (arg232_arr)) (1) then
                arg232_arr
            else
                fun221_quicksort_rec (fun1_head (arg232_arr)) ([||]) ([||]) (fun3_tail (arg232_arr))
    in
    let fun233_bm_sort arg219_arr =
        let var220_n  =
            Array.length (arg219_arr)
        in
        fun222_quicksort (arg219_arr)
    in
    let fun237_bm_median arg234_arr =
        let var235_n  =
            Array.length (arg234_arr)
        in
        let var236_sorted  =
            fun233_bm_sort (arg234_arr)
        in
        if ( = ) (( mod ) (var235_n) (2)) (0) then
            ( /. ) (( +. ) (Array.get (arg234_arr) (( - ) (( / ) (var235_n) (2)) (1))) (Array.get (arg234_arr) (( / ) (var235_n) (2)))) (2.0e+0)
        else
            Array.get (arg234_arr) (( / ) (var235_n) (2))
    in
    let rec fun240_work arg244_arr arg243_n arg241_i arg242_acc =
            if ( = ) (arg241_i) (arg243_n) then
                arg242_acc
            else
                fun240_work (arg244_arr) (arg243_n) (( + ) (arg241_i) (1)) (( +. ) (arg242_acc) (Array.get (arg244_arr) (arg241_i)))
    in
    let fun245_bm_sum arg238_arr =
        let var239_n  =
            Array.length (arg238_arr)
        in
        fun240_work (arg238_arr) (var239_n) (0) (0.0)
    in
    let rec fun248_work arg252_arr arg251_n arg249_i arg250_acc =
            if ( = ) (arg249_i) (arg251_n) then
                arg250_acc
            else
                let var253_e  =
                    Array.get (arg252_arr) (arg249_i)
                in
                fun248_work (arg252_arr) (arg251_n) (( + ) (arg249_i) (1)) (if ( > ) (var253_e) (arg250_acc) then
                    var253_e
                else
                    arg250_acc)
    in
    let fun254_bm_max arg246_arr =
        let var247_n  =
            Array.length (arg246_arr)
        in
        fun248_work (arg246_arr) (var247_n) (1) (Array.get (arg246_arr) (0))
    in
    let rec fun257_work arg261_arr arg260_n arg258_i arg259_acc =
            if ( = ) (arg258_i) (arg260_n) then
                arg259_acc
            else
                let var262_e  =
                    Array.get (arg261_arr) (arg258_i)
                in
                fun257_work (arg261_arr) (arg260_n) (( + ) (arg258_i) (1)) (if ( < ) (var262_e) (arg259_acc) then
                    var262_e
                else
                    arg259_acc)
    in
    let fun263_bm_min arg255_arr =
        let var256_n  =
            Array.length (arg255_arr)
        in
        fun257_work (arg255_arr) (var256_n) (1) (Array.get (arg255_arr) (0))
    in
    let fun266_bm_dist arg264_a arg265_b =
        if ( > ) (arg264_a) (arg265_b) then
            ( -. ) (arg264_a) (arg265_b)
        else
            ( -. ) (arg265_b) (arg264_a)
    in
    let var161_matA_rows  =
        4096
    in
    let var162_matA_cols  =
        4096
    in
    let var183_matA  =
        fun96_matrixInitf (var161_matA_rows) (var162_matA_cols) (fun176_matAinitfun_v2)
    in
    let var184_vecX_rows  =
        var162_matA_cols
    in
    let var185_vecX_cols  =
        1
    in
    let var186_vecY_rows  =
        var162_matA_cols
    in
    let var187_vecY_cols  =
        1
    in
    let var188_vecX  =
        fun96_matrixInitf (var184_vecX_rows) (var185_vecX_cols) (fun179_vecXinitfun)
    in
    let var189_vecY  =
        fun96_matrixInitf (var186_vecY_rows) (var187_vecY_cols) (fun182_vecYinitfun)
    in
    let var267__  =
        ()
    in
    let var268_bmres_warmup  =
        fun218_bm_runmultiple (var183_matA) (var189_vecY) (var188_vecX) (var162_matA_cols) (var161_matA_rows) (4)
    in
    let var269__  =
        ()
    in
    let var270_bmres_iters  =
        fun218_bm_runmultiple (var183_matA) (var189_vecY) (var188_vecX) (var162_matA_cols) (var161_matA_rows) (15)
    in
    let var333__  =
        let var271_median  =
            fun237_bm_median (var270_bmres_iters)
        in
        let var272_sum  =
            fun245_bm_sum (var270_bmres_iters)
        in
        let var273_avg  =
            ( /. ) (var272_sum) (1.50e+1)
        in
        let var274_max  =
            fun254_bm_max (var270_bmres_iters)
        in
        let var275_min  =
            fun263_bm_min (var270_bmres_iters)
        in
        let var276_variance  =
            fun254_bm_max ([|fun266_bm_dist (var273_avg) (var274_max); fun266_bm_dist (var273_avg) (var275_min)|])
        in
        let var277_median  =
            ( *. ) (var271_median) (1.0e+3)
        in
        let var278_sum  =
            ( *. ) (var272_sum) (1.0e+3)
        in
        let var279_avg  =
            ( *. ) (var273_avg) (1.0e+3)
        in
        let var280_max  =
            ( *. ) (var274_max) (1.0e+3)
        in
        let var281_min  =
            ( *. ) (var275_min) (1.0e+3)
        in
        let var282_variance  =
            ( *. ) (var276_variance) (1.0e+3)
        in
        let var283__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'='; '='; ' '; 'I'; 'T'; 'E'; 'R'; 'A'; 'T'; 'I'; 'O'; 'N'; ' '; 'R'; 'E'; 'S'; 'U'; 'L'; 'T'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var284__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'i'; 't'; 'e'; 'r'; 'a'; 't'; 'i'; 'o'; 'n'; 's'; ':'; ' '|])
        in
        let var285__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'1'; '5'|])
        in
        let var286__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var287__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var288__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var277_median))
        in
        let var289__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var290__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var291__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var280_max))
        in
        let var292__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var293__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var294__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var281_min))
        in
        let var295__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var296__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var297__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var279_avg))
        in
        let var298__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var299__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var300__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var282_variance))
        in
        let var301__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var302_median  =
            fun237_bm_median (var268_bmres_warmup)
        in
        let var303_sum  =
            fun245_bm_sum (var268_bmres_warmup)
        in
        let var304_avg  =
            ( /. ) (var303_sum) (4.0e+0)
        in
        let var305_max  =
            fun254_bm_max (var268_bmres_warmup)
        in
        let var306_min  =
            fun263_bm_min (var268_bmres_warmup)
        in
        let var307_variance  =
            fun254_bm_max ([|fun266_bm_dist (var304_avg) (var305_max); fun266_bm_dist (var304_avg) (var306_min)|])
        in
        let var308_median  =
            ( *. ) (var302_median) (1.0e+3)
        in
        let var309_sum  =
            ( *. ) (var303_sum) (1.0e+3)
        in
        let var310_avg  =
            ( *. ) (var304_avg) (1.0e+3)
        in
        let var311_max  =
            ( *. ) (var305_max) (1.0e+3)
        in
        let var312_min  =
            ( *. ) (var306_min) (1.0e+3)
        in
        let var313_variance  =
            ( *. ) (var307_variance) (1.0e+3)
        in
        let var314__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; '\n'; '='; '='; ' '; 'W'; 'A'; 'R'; 'M'; 'U'; 'P'; ' '; 'S'; 'T'; 'A'; 'T'; 'I'; 'S'; 'T'; 'I'; 'C'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var315__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'w'; 'a'; 'r'; 'm'; 'u'; 'p'; ' '; 'r'; 'u'; 'n'; 's'; ':'; ' '|])
        in
        let var316__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'4'|])
        in
        let var317__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var318__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var319__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var308_median))
        in
        let var320__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var321__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var322__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var311_max))
        in
        let var323__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var324__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var325__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var312_min))
        in
        let var326__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var327__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var328__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var310_avg))
        in
        let var329__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var330__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var331__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var313_variance))
        in
        let var332__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        ()
    in
    ()