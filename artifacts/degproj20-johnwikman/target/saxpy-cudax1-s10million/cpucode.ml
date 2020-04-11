open Printf

external gpuhost_fun82_saxpy: int array -> float array -> float array -> float array -> float array = "gpuhost_fun82_saxpy"

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
    let fun82_saxpy arg78_a arg79_y arg80_i arg81_xelem =
        ( +. ) (( *. ) (arg78_a) (arg81_xelem)) (Array.get (arg79_y) (arg80_i))
    in
    let fun84_vecXinitfun arg83_i =
        ( /. ) (float_of_int (( mod ) (( * ) (arg83_i) (arg83_i)) (30269))) (3.0e+0)
    in
    let fun86_vecYinitfun arg85_i =
        ( /. ) (float_of_int (( mod ) (( * ) (( * ) (arg85_i) (arg85_i)) (arg85_i)) (30271))) (7.0e+0)
    in
    let fun97_bm_runonce arg94_vecX arg93_vecY arg92_scalarA arg90__ =
        let var91_bm_t_start  =
            Unix.gettimeofday (())
        in
        let var95_vecS  =
            gpuhost_fun82_saxpy [|1|] [|arg92_scalarA|] (arg93_vecY) (arg94_vecX)
        in
        let var96_bm_t_end  =
            Unix.gettimeofday (())
        in
        ( -. ) (var96_bm_t_end) (var91_bm_t_start)
    in
    let rec fun99_bm_iter arg106_scalarA arg105_vecY arg104_vecX arg102_n arg100_i arg101_acc =
            if ( >= ) (arg100_i) (arg102_n) then
                arg101_acc
            else
                let var103__  =
                    ()
                in
                let var107_res  =
                    fun97_bm_runonce (arg104_vecX) (arg105_vecY) (arg106_scalarA) (())
                in
                let var108_newacc  =
                    Array.append (arg101_acc) ([|var107_res|])
                in
                fun99_bm_iter (arg106_scalarA) (arg105_vecY) (arg104_vecX) (arg102_n) (( + ) (arg100_i) (1)) (var108_newacc)
    in
    let fun112_bm_runmultiple arg111_vecX arg110_vecY arg109_scalarA arg98_n =
        fun99_bm_iter (arg109_scalarA) (arg110_vecY) (arg111_vecX) (arg98_n) (0) ([||])
    in
    let rec fun115_quicksort_rec arg117_pivot arg118_lt_pivot arg119_geq_pivot arg120_remaining =
            if ( = ) (Array.length (arg120_remaining)) (0) then
                let var121_seq_lt  =
                    fun116_quicksort (arg118_lt_pivot)
                in
                let var122_seq_pivot  =
                    [|arg117_pivot|]
                in
                let var123_seq_geq  =
                    fun116_quicksort (arg119_geq_pivot)
                in
                Array.append (Array.append (var121_seq_lt) (var122_seq_pivot)) (var123_seq_geq)
            else
                let var124_e  =
                    fun1_head (arg120_remaining)
                in
                let var125_t  =
                    fun3_tail (arg120_remaining)
                in
                if ( < ) (var124_e) (arg117_pivot) then
                    fun115_quicksort_rec (arg117_pivot) ((fun x xs -> Array.append [|x|] xs) (var124_e) (arg118_lt_pivot)) (arg119_geq_pivot) (var125_t)
                else
                    fun115_quicksort_rec (arg117_pivot) (arg118_lt_pivot) ((fun x xs -> Array.append [|x|] xs) (var124_e) (arg119_geq_pivot)) (var125_t)
        and fun116_quicksort arg126_arr =
            if ( <= ) (Array.length (arg126_arr)) (1) then
                arg126_arr
            else
                fun115_quicksort_rec (fun1_head (arg126_arr)) ([||]) ([||]) (fun3_tail (arg126_arr))
    in
    let fun127_bm_sort arg113_arr =
        let var114_n  =
            Array.length (arg113_arr)
        in
        fun116_quicksort (arg113_arr)
    in
    let fun131_bm_median arg128_arr =
        let var129_n  =
            Array.length (arg128_arr)
        in
        let var130_sorted  =
            fun127_bm_sort (arg128_arr)
        in
        if ( = ) (( mod ) (var129_n) (2)) (0) then
            ( /. ) (( +. ) (Array.get (arg128_arr) (( - ) (( / ) (var129_n) (2)) (1))) (Array.get (arg128_arr) (( / ) (var129_n) (2)))) (2.0e+0)
        else
            Array.get (arg128_arr) (( / ) (var129_n) (2))
    in
    let rec fun134_work arg138_arr arg137_n arg135_i arg136_acc =
            if ( = ) (arg135_i) (arg137_n) then
                arg136_acc
            else
                fun134_work (arg138_arr) (arg137_n) (( + ) (arg135_i) (1)) (( +. ) (arg136_acc) (Array.get (arg138_arr) (arg135_i)))
    in
    let fun139_bm_sum arg132_arr =
        let var133_n  =
            Array.length (arg132_arr)
        in
        fun134_work (arg132_arr) (var133_n) (0) (0.0)
    in
    let rec fun142_work arg146_arr arg145_n arg143_i arg144_acc =
            if ( = ) (arg143_i) (arg145_n) then
                arg144_acc
            else
                let var147_e  =
                    Array.get (arg146_arr) (arg143_i)
                in
                fun142_work (arg146_arr) (arg145_n) (( + ) (arg143_i) (1)) (if ( > ) (var147_e) (arg144_acc) then
                    var147_e
                else
                    arg144_acc)
    in
    let fun148_bm_max arg140_arr =
        let var141_n  =
            Array.length (arg140_arr)
        in
        fun142_work (arg140_arr) (var141_n) (1) (Array.get (arg140_arr) (0))
    in
    let rec fun151_work arg155_arr arg154_n arg152_i arg153_acc =
            if ( = ) (arg152_i) (arg154_n) then
                arg153_acc
            else
                let var156_e  =
                    Array.get (arg155_arr) (arg152_i)
                in
                fun151_work (arg155_arr) (arg154_n) (( + ) (arg152_i) (1)) (if ( < ) (var156_e) (arg153_acc) then
                    var156_e
                else
                    arg153_acc)
    in
    let fun157_bm_min arg149_arr =
        let var150_n  =
            Array.length (arg149_arr)
        in
        fun151_work (arg149_arr) (var150_n) (1) (Array.get (arg149_arr) (0))
    in
    let fun160_bm_dist arg158_a arg159_b =
        if ( > ) (arg158_a) (arg159_b) then
            ( -. ) (arg158_a) (arg159_b)
        else
            ( -. ) (arg159_b) (arg158_a)
    in
    let var77_vecsize  =
        10000000
    in
    let var87_scalarA  =
        2.389999e+2
    in
    let var88_vecX  =
        fun14_seqInit (var77_vecsize) (fun84_vecXinitfun)
    in
    let var89_vecY  =
        fun14_seqInit (var77_vecsize) (fun86_vecYinitfun)
    in
    let var161__  =
        ()
    in
    let var162_bmres_warmup  =
        fun112_bm_runmultiple (var88_vecX) (var89_vecY) (var87_scalarA) (4)
    in
    let var163__  =
        ()
    in
    let var164_bmres_iters  =
        fun112_bm_runmultiple (var88_vecX) (var89_vecY) (var87_scalarA) (15)
    in
    let var227__  =
        let var165_median  =
            fun131_bm_median (var164_bmres_iters)
        in
        let var166_sum  =
            fun139_bm_sum (var164_bmres_iters)
        in
        let var167_avg  =
            ( /. ) (var166_sum) (1.50e+1)
        in
        let var168_max  =
            fun148_bm_max (var164_bmres_iters)
        in
        let var169_min  =
            fun157_bm_min (var164_bmres_iters)
        in
        let var170_variance  =
            fun148_bm_max ([|fun160_bm_dist (var167_avg) (var168_max); fun160_bm_dist (var167_avg) (var169_min)|])
        in
        let var171_median  =
            ( *. ) (var165_median) (1.0e+3)
        in
        let var172_sum  =
            ( *. ) (var166_sum) (1.0e+3)
        in
        let var173_avg  =
            ( *. ) (var167_avg) (1.0e+3)
        in
        let var174_max  =
            ( *. ) (var168_max) (1.0e+3)
        in
        let var175_min  =
            ( *. ) (var169_min) (1.0e+3)
        in
        let var176_variance  =
            ( *. ) (var170_variance) (1.0e+3)
        in
        let var177__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'='; '='; ' '; 'I'; 'T'; 'E'; 'R'; 'A'; 'T'; 'I'; 'O'; 'N'; ' '; 'R'; 'E'; 'S'; 'U'; 'L'; 'T'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var178__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'i'; 't'; 'e'; 'r'; 'a'; 't'; 'i'; 'o'; 'n'; 's'; ':'; ' '|])
        in
        let var179__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'1'; '5'|])
        in
        let var180__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var181__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var182__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var171_median))
        in
        let var183__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var184__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var185__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var174_max))
        in
        let var186__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var187__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var188__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var175_min))
        in
        let var189__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var190__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var191__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var173_avg))
        in
        let var192__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var193__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var194__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var176_variance))
        in
        let var195__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var196_median  =
            fun131_bm_median (var162_bmres_warmup)
        in
        let var197_sum  =
            fun139_bm_sum (var162_bmres_warmup)
        in
        let var198_avg  =
            ( /. ) (var197_sum) (4.0e+0)
        in
        let var199_max  =
            fun148_bm_max (var162_bmres_warmup)
        in
        let var200_min  =
            fun157_bm_min (var162_bmres_warmup)
        in
        let var201_variance  =
            fun148_bm_max ([|fun160_bm_dist (var198_avg) (var199_max); fun160_bm_dist (var198_avg) (var200_min)|])
        in
        let var202_median  =
            ( *. ) (var196_median) (1.0e+3)
        in
        let var203_sum  =
            ( *. ) (var197_sum) (1.0e+3)
        in
        let var204_avg  =
            ( *. ) (var198_avg) (1.0e+3)
        in
        let var205_max  =
            ( *. ) (var199_max) (1.0e+3)
        in
        let var206_min  =
            ( *. ) (var200_min) (1.0e+3)
        in
        let var207_variance  =
            ( *. ) (var201_variance) (1.0e+3)
        in
        let var208__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; '\n'; '='; '='; ' '; 'W'; 'A'; 'R'; 'M'; 'U'; 'P'; ' '; 'S'; 'T'; 'A'; 'T'; 'I'; 'S'; 'T'; 'I'; 'C'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var209__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'w'; 'a'; 'r'; 'm'; 'u'; 'p'; ' '; 'r'; 'u'; 'n'; 's'; ':'; ' '|])
        in
        let var210__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'4'|])
        in
        let var211__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var212__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var213__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var202_median))
        in
        let var214__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var215__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var216__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var205_max))
        in
        let var217__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var218__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var219__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var206_min))
        in
        let var220__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var221__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var222__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var204_avg))
        in
        let var223__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var224__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var225__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var207_variance))
        in
        let var226__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        ()
    in
    ()