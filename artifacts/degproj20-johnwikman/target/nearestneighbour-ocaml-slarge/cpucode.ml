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
    let fun85_nnDistance arg78_x arg79_y arg80_pointsX arg81_pointsY arg82_i =
        let var83_dxi  =
            ( -. ) (arg78_x) (Array.get (arg80_pointsX) (arg82_i))
        in
        let var84_dyi  =
            ( -. ) (arg79_y) (Array.get (arg81_pointsY) (arg82_i))
        in
        Float.sqrt (( +. ) (( *. ) (var83_dxi) (var83_dxi)) (( *. ) (var84_dyi) (var84_dyi)))
    in
    let fun87_pointsXInitFunc arg86__ =
        (fun x y -> x +. (Random.float (Float.abs (x -. y)))) (-1.0e+1) (1.0e+1)
    in
    let fun89_pointsYInitFunc arg88__ =
        (fun x y -> x +. (Random.float (Float.abs (x -. y)))) (-1.0e+1) (1.0e+1)
    in
    let fun103_bm_runonce arg100_pointsY arg99_pointsX arg98_pY arg97_pX arg96_numPoints arg94__ =
        let var95_bm_t_start  =
            Unix.gettimeofday (())
        in
        let var101_distances  =
            fun14_seqInit (arg96_numPoints) (fun85_nnDistance (arg97_pX) (arg98_pY) (arg99_pointsX) (arg100_pointsY))
        in
        let var102_bm_t_end  =
            Unix.gettimeofday (())
        in
        ( -. ) (var102_bm_t_end) (var95_bm_t_start)
    in
    let rec fun105_bm_iter arg114_numPoints arg113_pX arg112_pY arg111_pointsX arg110_pointsY arg108_n arg106_i arg107_acc =
            if ( >= ) (arg106_i) (arg108_n) then
                arg107_acc
            else
                let var109__  =
                    ()
                in
                let var115_res  =
                    fun103_bm_runonce (arg110_pointsY) (arg111_pointsX) (arg112_pY) (arg113_pX) (arg114_numPoints) (())
                in
                let var116_newacc  =
                    Array.append (arg107_acc) ([|var115_res|])
                in
                fun105_bm_iter (arg114_numPoints) (arg113_pX) (arg112_pY) (arg111_pointsX) (arg110_pointsY) (arg108_n) (( + ) (arg106_i) (1)) (var116_newacc)
    in
    let fun122_bm_runmultiple arg121_pointsY arg120_pointsX arg119_pY arg118_pX arg117_numPoints arg104_n =
        fun105_bm_iter (arg117_numPoints) (arg118_pX) (arg119_pY) (arg120_pointsX) (arg121_pointsY) (arg104_n) (0) ([||])
    in
    let rec fun125_quicksort_rec arg127_pivot arg128_lt_pivot arg129_geq_pivot arg130_remaining =
            if ( = ) (Array.length (arg130_remaining)) (0) then
                let var131_seq_lt  =
                    fun126_quicksort (arg128_lt_pivot)
                in
                let var132_seq_pivot  =
                    [|arg127_pivot|]
                in
                let var133_seq_geq  =
                    fun126_quicksort (arg129_geq_pivot)
                in
                Array.append (Array.append (var131_seq_lt) (var132_seq_pivot)) (var133_seq_geq)
            else
                let var134_e  =
                    fun1_head (arg130_remaining)
                in
                let var135_t  =
                    fun3_tail (arg130_remaining)
                in
                if ( < ) (var134_e) (arg127_pivot) then
                    fun125_quicksort_rec (arg127_pivot) ((fun x xs -> Array.append [|x|] xs) (var134_e) (arg128_lt_pivot)) (arg129_geq_pivot) (var135_t)
                else
                    fun125_quicksort_rec (arg127_pivot) (arg128_lt_pivot) ((fun x xs -> Array.append [|x|] xs) (var134_e) (arg129_geq_pivot)) (var135_t)
        and fun126_quicksort arg136_arr =
            if ( <= ) (Array.length (arg136_arr)) (1) then
                arg136_arr
            else
                fun125_quicksort_rec (fun1_head (arg136_arr)) ([||]) ([||]) (fun3_tail (arg136_arr))
    in
    let fun137_bm_sort arg123_arr =
        let var124_n  =
            Array.length (arg123_arr)
        in
        fun126_quicksort (arg123_arr)
    in
    let fun141_bm_median arg138_arr =
        let var139_n  =
            Array.length (arg138_arr)
        in
        let var140_sorted  =
            fun137_bm_sort (arg138_arr)
        in
        if ( = ) (( mod ) (var139_n) (2)) (0) then
            ( /. ) (( +. ) (Array.get (arg138_arr) (( - ) (( / ) (var139_n) (2)) (1))) (Array.get (arg138_arr) (( / ) (var139_n) (2)))) (2.0e+0)
        else
            Array.get (arg138_arr) (( / ) (var139_n) (2))
    in
    let rec fun144_work arg148_arr arg147_n arg145_i arg146_acc =
            if ( = ) (arg145_i) (arg147_n) then
                arg146_acc
            else
                fun144_work (arg148_arr) (arg147_n) (( + ) (arg145_i) (1)) (( +. ) (arg146_acc) (Array.get (arg148_arr) (arg145_i)))
    in
    let fun149_bm_sum arg142_arr =
        let var143_n  =
            Array.length (arg142_arr)
        in
        fun144_work (arg142_arr) (var143_n) (0) (0.0)
    in
    let rec fun152_work arg156_arr arg155_n arg153_i arg154_acc =
            if ( = ) (arg153_i) (arg155_n) then
                arg154_acc
            else
                let var157_e  =
                    Array.get (arg156_arr) (arg153_i)
                in
                fun152_work (arg156_arr) (arg155_n) (( + ) (arg153_i) (1)) (if ( > ) (var157_e) (arg154_acc) then
                    var157_e
                else
                    arg154_acc)
    in
    let fun158_bm_max arg150_arr =
        let var151_n  =
            Array.length (arg150_arr)
        in
        fun152_work (arg150_arr) (var151_n) (1) (Array.get (arg150_arr) (0))
    in
    let rec fun161_work arg165_arr arg164_n arg162_i arg163_acc =
            if ( = ) (arg162_i) (arg164_n) then
                arg163_acc
            else
                let var166_e  =
                    Array.get (arg165_arr) (arg162_i)
                in
                fun161_work (arg165_arr) (arg164_n) (( + ) (arg162_i) (1)) (if ( < ) (var166_e) (arg163_acc) then
                    var166_e
                else
                    arg163_acc)
    in
    let fun167_bm_min arg159_arr =
        let var160_n  =
            Array.length (arg159_arr)
        in
        fun161_work (arg159_arr) (var160_n) (1) (Array.get (arg159_arr) (0))
    in
    let fun170_bm_dist arg168_a arg169_b =
        if ( > ) (arg168_a) (arg169_b) then
            ( -. ) (arg168_a) (arg169_b)
        else
            ( -. ) (arg169_b) (arg168_a)
    in
    let var77_numPoints  =
        33554432
    in
    let var90_pointsX  =
        fun14_seqInit (var77_numPoints) (fun87_pointsXInitFunc)
    in
    let var91_pointsY  =
        fun14_seqInit (var77_numPoints) (fun89_pointsYInitFunc)
    in
    let var92_pX  =
        5.0e-1
    in
    let var93_pY  =
        1.50e+0
    in
    let var171__  =
        ()
    in
    let var172_bmres_warmup  =
        fun122_bm_runmultiple (var91_pointsY) (var90_pointsX) (var93_pY) (var92_pX) (var77_numPoints) (4)
    in
    let var173__  =
        ()
    in
    let var174_bmres_iters  =
        fun122_bm_runmultiple (var91_pointsY) (var90_pointsX) (var93_pY) (var92_pX) (var77_numPoints) (15)
    in
    let var237__  =
        let var175_median  =
            fun141_bm_median (var174_bmres_iters)
        in
        let var176_sum  =
            fun149_bm_sum (var174_bmres_iters)
        in
        let var177_avg  =
            ( /. ) (var176_sum) (1.50e+1)
        in
        let var178_max  =
            fun158_bm_max (var174_bmres_iters)
        in
        let var179_min  =
            fun167_bm_min (var174_bmres_iters)
        in
        let var180_variance  =
            fun158_bm_max ([|fun170_bm_dist (var177_avg) (var178_max); fun170_bm_dist (var177_avg) (var179_min)|])
        in
        let var181_median  =
            ( *. ) (var175_median) (1.0e+3)
        in
        let var182_sum  =
            ( *. ) (var176_sum) (1.0e+3)
        in
        let var183_avg  =
            ( *. ) (var177_avg) (1.0e+3)
        in
        let var184_max  =
            ( *. ) (var178_max) (1.0e+3)
        in
        let var185_min  =
            ( *. ) (var179_min) (1.0e+3)
        in
        let var186_variance  =
            ( *. ) (var180_variance) (1.0e+3)
        in
        let var187__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'='; '='; ' '; 'I'; 'T'; 'E'; 'R'; 'A'; 'T'; 'I'; 'O'; 'N'; ' '; 'R'; 'E'; 'S'; 'U'; 'L'; 'T'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var188__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'i'; 't'; 'e'; 'r'; 'a'; 't'; 'i'; 'o'; 'n'; 's'; ':'; ' '|])
        in
        let var189__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'1'; '5'|])
        in
        let var190__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var191__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var192__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var181_median))
        in
        let var193__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var194__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var195__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var184_max))
        in
        let var196__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var197__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var198__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var185_min))
        in
        let var199__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var200__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var201__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var183_avg))
        in
        let var202__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var203__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var204__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var186_variance))
        in
        let var205__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var206_median  =
            fun141_bm_median (var172_bmres_warmup)
        in
        let var207_sum  =
            fun149_bm_sum (var172_bmres_warmup)
        in
        let var208_avg  =
            ( /. ) (var207_sum) (4.0e+0)
        in
        let var209_max  =
            fun158_bm_max (var172_bmres_warmup)
        in
        let var210_min  =
            fun167_bm_min (var172_bmres_warmup)
        in
        let var211_variance  =
            fun158_bm_max ([|fun170_bm_dist (var208_avg) (var209_max); fun170_bm_dist (var208_avg) (var210_min)|])
        in
        let var212_median  =
            ( *. ) (var206_median) (1.0e+3)
        in
        let var213_sum  =
            ( *. ) (var207_sum) (1.0e+3)
        in
        let var214_avg  =
            ( *. ) (var208_avg) (1.0e+3)
        in
        let var215_max  =
            ( *. ) (var209_max) (1.0e+3)
        in
        let var216_min  =
            ( *. ) (var210_min) (1.0e+3)
        in
        let var217_variance  =
            ( *. ) (var211_variance) (1.0e+3)
        in
        let var218__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; '\n'; '='; '='; ' '; 'W'; 'A'; 'R'; 'M'; 'U'; 'P'; ' '; 'S'; 'T'; 'A'; 'T'; 'I'; 'S'; 'T'; 'I'; 'C'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var219__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'w'; 'a'; 'r'; 'm'; 'u'; 'p'; ' '; 'r'; 'u'; 'n'; 's'; ':'; ' '|])
        in
        let var220__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'4'|])
        in
        let var221__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var222__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var223__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var212_median))
        in
        let var224__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var225__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var226__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var215_max))
        in
        let var227__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var228__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var229__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var216_min))
        in
        let var230__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var231__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var232__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var214_avg))
        in
        let var233__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var234__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var235__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var217_variance))
        in
        let var236__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        ()
    in
    ()