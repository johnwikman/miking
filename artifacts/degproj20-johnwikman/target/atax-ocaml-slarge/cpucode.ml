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
    let fun167_matAinitfun_v2 arg165_row arg166_col =
        ( -. ) (( /. ) (float_of_int (( + ) (( * ) (arg165_row) (arg165_row)) (arg166_col))) (3.0e+0)) (1.400000e-2)
    in
    let fun170_vecXinitfun arg168_row arg169_col =
        float_of_int (( mod ) (( * ) (arg168_row) (10657)) (41081))
    in
    let fun185_bm_runonce arg182_vecX arg181_vecX_cols arg179_matA arg178_matA_rows arg177_matA_cols arg175__ =
        let var176_bm_t_start  =
            Unix.gettimeofday (())
        in
        let var180_matATA  =
            fun14_seqInit (( * ) (arg177_matA_cols) (arg177_matA_cols)) (fun160_matrixATAfWorker (arg178_matA_rows) (arg177_matA_cols) (arg179_matA))
        in
        let var183_vecATAx  =
            fun14_seqInit (( * ) (arg177_matA_cols) (arg181_vecX_cols)) (fun141_matrixMulfWorker (arg177_matA_cols) (arg177_matA_cols) (arg181_vecX_cols) (var180_matATA) (arg182_vecX))
        in
        let var184_bm_t_end  =
            Unix.gettimeofday (())
        in
        ( -. ) (var184_bm_t_end) (var176_bm_t_start)
    in
    let rec fun187_bm_iter arg196_matA_cols arg195_matA_rows arg194_matA arg193_vecX_cols arg192_vecX arg190_n arg188_i arg189_acc =
            if ( >= ) (arg188_i) (arg190_n) then
                arg189_acc
            else
                let var191__  =
                    ()
                in
                let var197_res  =
                    fun185_bm_runonce (arg192_vecX) (arg193_vecX_cols) (arg194_matA) (arg195_matA_rows) (arg196_matA_cols) (())
                in
                let var198_newacc  =
                    Array.append (arg189_acc) ([|var197_res|])
                in
                fun187_bm_iter (arg196_matA_cols) (arg195_matA_rows) (arg194_matA) (arg193_vecX_cols) (arg192_vecX) (arg190_n) (( + ) (arg188_i) (1)) (var198_newacc)
    in
    let fun204_bm_runmultiple arg203_vecX arg202_vecX_cols arg201_matA arg200_matA_rows arg199_matA_cols arg186_n =
        fun187_bm_iter (arg199_matA_cols) (arg200_matA_rows) (arg201_matA) (arg202_vecX_cols) (arg203_vecX) (arg186_n) (0) ([||])
    in
    let rec fun207_quicksort_rec arg209_pivot arg210_lt_pivot arg211_geq_pivot arg212_remaining =
            if ( = ) (Array.length (arg212_remaining)) (0) then
                let var213_seq_lt  =
                    fun208_quicksort (arg210_lt_pivot)
                in
                let var214_seq_pivot  =
                    [|arg209_pivot|]
                in
                let var215_seq_geq  =
                    fun208_quicksort (arg211_geq_pivot)
                in
                Array.append (Array.append (var213_seq_lt) (var214_seq_pivot)) (var215_seq_geq)
            else
                let var216_e  =
                    fun1_head (arg212_remaining)
                in
                let var217_t  =
                    fun3_tail (arg212_remaining)
                in
                if ( < ) (var216_e) (arg209_pivot) then
                    fun207_quicksort_rec (arg209_pivot) ((fun x xs -> Array.append [|x|] xs) (var216_e) (arg210_lt_pivot)) (arg211_geq_pivot) (var217_t)
                else
                    fun207_quicksort_rec (arg209_pivot) (arg210_lt_pivot) ((fun x xs -> Array.append [|x|] xs) (var216_e) (arg211_geq_pivot)) (var217_t)
        and fun208_quicksort arg218_arr =
            if ( <= ) (Array.length (arg218_arr)) (1) then
                arg218_arr
            else
                fun207_quicksort_rec (fun1_head (arg218_arr)) ([||]) ([||]) (fun3_tail (arg218_arr))
    in
    let fun219_bm_sort arg205_arr =
        let var206_n  =
            Array.length (arg205_arr)
        in
        fun208_quicksort (arg205_arr)
    in
    let fun223_bm_median arg220_arr =
        let var221_n  =
            Array.length (arg220_arr)
        in
        let var222_sorted  =
            fun219_bm_sort (arg220_arr)
        in
        if ( = ) (( mod ) (var221_n) (2)) (0) then
            ( /. ) (( +. ) (Array.get (arg220_arr) (( - ) (( / ) (var221_n) (2)) (1))) (Array.get (arg220_arr) (( / ) (var221_n) (2)))) (2.0e+0)
        else
            Array.get (arg220_arr) (( / ) (var221_n) (2))
    in
    let rec fun226_work arg230_arr arg229_n arg227_i arg228_acc =
            if ( = ) (arg227_i) (arg229_n) then
                arg228_acc
            else
                fun226_work (arg230_arr) (arg229_n) (( + ) (arg227_i) (1)) (( +. ) (arg228_acc) (Array.get (arg230_arr) (arg227_i)))
    in
    let fun231_bm_sum arg224_arr =
        let var225_n  =
            Array.length (arg224_arr)
        in
        fun226_work (arg224_arr) (var225_n) (0) (0.0)
    in
    let rec fun234_work arg238_arr arg237_n arg235_i arg236_acc =
            if ( = ) (arg235_i) (arg237_n) then
                arg236_acc
            else
                let var239_e  =
                    Array.get (arg238_arr) (arg235_i)
                in
                fun234_work (arg238_arr) (arg237_n) (( + ) (arg235_i) (1)) (if ( > ) (var239_e) (arg236_acc) then
                    var239_e
                else
                    arg236_acc)
    in
    let fun240_bm_max arg232_arr =
        let var233_n  =
            Array.length (arg232_arr)
        in
        fun234_work (arg232_arr) (var233_n) (1) (Array.get (arg232_arr) (0))
    in
    let rec fun243_work arg247_arr arg246_n arg244_i arg245_acc =
            if ( = ) (arg244_i) (arg246_n) then
                arg245_acc
            else
                let var248_e  =
                    Array.get (arg247_arr) (arg244_i)
                in
                fun243_work (arg247_arr) (arg246_n) (( + ) (arg244_i) (1)) (if ( < ) (var248_e) (arg245_acc) then
                    var248_e
                else
                    arg245_acc)
    in
    let fun249_bm_min arg241_arr =
        let var242_n  =
            Array.length (arg241_arr)
        in
        fun243_work (arg241_arr) (var242_n) (1) (Array.get (arg241_arr) (0))
    in
    let fun252_bm_dist arg250_a arg251_b =
        if ( > ) (arg250_a) (arg251_b) then
            ( -. ) (arg250_a) (arg251_b)
        else
            ( -. ) (arg251_b) (arg250_a)
    in
    let var161_matA_rows  =
        8192
    in
    let var162_matA_cols  =
        8192
    in
    let var163_matB_rows  =
        8192
    in
    let var164_matB_cols  =
        8192
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
    let var253__  =
        ()
    in
    let var254_bmres_warmup  =
        fun204_bm_runmultiple (var174_vecX) (var173_vecX_cols) (var171_matA) (var161_matA_rows) (var162_matA_cols) (4)
    in
    let var255__  =
        ()
    in
    let var256_bmres_iters  =
        fun204_bm_runmultiple (var174_vecX) (var173_vecX_cols) (var171_matA) (var161_matA_rows) (var162_matA_cols) (15)
    in
    let var319__  =
        let var257_median  =
            fun223_bm_median (var256_bmres_iters)
        in
        let var258_sum  =
            fun231_bm_sum (var256_bmres_iters)
        in
        let var259_avg  =
            ( /. ) (var258_sum) (1.50e+1)
        in
        let var260_max  =
            fun240_bm_max (var256_bmres_iters)
        in
        let var261_min  =
            fun249_bm_min (var256_bmres_iters)
        in
        let var262_variance  =
            fun240_bm_max ([|fun252_bm_dist (var259_avg) (var260_max); fun252_bm_dist (var259_avg) (var261_min)|])
        in
        let var263_median  =
            ( *. ) (var257_median) (1.0e+3)
        in
        let var264_sum  =
            ( *. ) (var258_sum) (1.0e+3)
        in
        let var265_avg  =
            ( *. ) (var259_avg) (1.0e+3)
        in
        let var266_max  =
            ( *. ) (var260_max) (1.0e+3)
        in
        let var267_min  =
            ( *. ) (var261_min) (1.0e+3)
        in
        let var268_variance  =
            ( *. ) (var262_variance) (1.0e+3)
        in
        let var269__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'='; '='; ' '; 'I'; 'T'; 'E'; 'R'; 'A'; 'T'; 'I'; 'O'; 'N'; ' '; 'R'; 'E'; 'S'; 'U'; 'L'; 'T'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var270__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'i'; 't'; 'e'; 'r'; 'a'; 't'; 'i'; 'o'; 'n'; 's'; ':'; ' '|])
        in
        let var271__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'1'; '5'|])
        in
        let var272__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var273__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var274__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var263_median))
        in
        let var275__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var276__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var277__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var266_max))
        in
        let var278__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var279__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var280__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var267_min))
        in
        let var281__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var282__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var283__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var265_avg))
        in
        let var284__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var285__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var286__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var268_variance))
        in
        let var287__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var288_median  =
            fun223_bm_median (var254_bmres_warmup)
        in
        let var289_sum  =
            fun231_bm_sum (var254_bmres_warmup)
        in
        let var290_avg  =
            ( /. ) (var289_sum) (4.0e+0)
        in
        let var291_max  =
            fun240_bm_max (var254_bmres_warmup)
        in
        let var292_min  =
            fun249_bm_min (var254_bmres_warmup)
        in
        let var293_variance  =
            fun240_bm_max ([|fun252_bm_dist (var290_avg) (var291_max); fun252_bm_dist (var290_avg) (var292_min)|])
        in
        let var294_median  =
            ( *. ) (var288_median) (1.0e+3)
        in
        let var295_sum  =
            ( *. ) (var289_sum) (1.0e+3)
        in
        let var296_avg  =
            ( *. ) (var290_avg) (1.0e+3)
        in
        let var297_max  =
            ( *. ) (var291_max) (1.0e+3)
        in
        let var298_min  =
            ( *. ) (var292_min) (1.0e+3)
        in
        let var299_variance  =
            ( *. ) (var293_variance) (1.0e+3)
        in
        let var300__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; '\n'; '='; '='; ' '; 'W'; 'A'; 'R'; 'M'; 'U'; 'P'; ' '; 'S'; 'T'; 'A'; 'T'; 'I'; 'S'; 'T'; 'I'; 'C'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var301__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'w'; 'a'; 'r'; 'm'; 'u'; 'p'; ' '; 'r'; 'u'; 'n'; 's'; ':'; ' '|])
        in
        let var302__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'4'|])
        in
        let var303__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var304__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var305__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var294_median))
        in
        let var306__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var307__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var308__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var297_max))
        in
        let var309__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var310__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var311__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var298_min))
        in
        let var312__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var313__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var314__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var296_avg))
        in
        let var315__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var316__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var317__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var299_variance))
        in
        let var318__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        ()
    in
    ()