open Printf

external gpuhost_fun81_saxpy: int array -> float array -> float array -> float array = "gpuhost_fun81_saxpy"

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
    let fun81_saxpy arg78_a arg79_y arg80_xelem =
        ( +. ) (( *. ) (arg78_a) (arg80_xelem)) (arg79_y)
    in
    let fun83_vecXinitfun arg82_i =
        float_of_int (( mod ) (( * ) (( * ) (arg82_i) (arg82_i)) (( - ) (500000000) (arg82_i))) (5073))
    in
    let fun94_bm_runonce arg91_vecX arg90_scalarY arg89_scalarA arg87__ =
        let var88_bm_t_start  =
            Unix.gettimeofday (())
        in
        let var92_vecS  =
            gpuhost_fun81_saxpy [|1|] [|arg89_scalarA; arg90_scalarY|] (arg91_vecX)
        in
        let var93_bm_t_end  =
            Unix.gettimeofday (())
        in
        ( -. ) (var93_bm_t_end) (var88_bm_t_start)
    in
    let rec fun96_bm_iter arg103_scalarA arg102_scalarY arg101_vecX arg99_n arg97_i arg98_acc =
            if ( >= ) (arg97_i) (arg99_n) then
                arg98_acc
            else
                let var100__  =
                    ()
                in
                let var104_res  =
                    fun94_bm_runonce (arg101_vecX) (arg102_scalarY) (arg103_scalarA) (())
                in
                let var105_newacc  =
                    Array.append (arg98_acc) ([|var104_res|])
                in
                fun96_bm_iter (arg103_scalarA) (arg102_scalarY) (arg101_vecX) (arg99_n) (( + ) (arg97_i) (1)) (var105_newacc)
    in
    let fun109_bm_runmultiple arg108_vecX arg107_scalarY arg106_scalarA arg95_n =
        fun96_bm_iter (arg106_scalarA) (arg107_scalarY) (arg108_vecX) (arg95_n) (0) ([||])
    in
    let rec fun112_quicksort_rec arg114_pivot arg115_lt_pivot arg116_geq_pivot arg117_remaining =
            if ( = ) (Array.length (arg117_remaining)) (0) then
                let var118_seq_lt  =
                    fun113_quicksort (arg115_lt_pivot)
                in
                let var119_seq_pivot  =
                    [|arg114_pivot|]
                in
                let var120_seq_geq  =
                    fun113_quicksort (arg116_geq_pivot)
                in
                Array.append (Array.append (var118_seq_lt) (var119_seq_pivot)) (var120_seq_geq)
            else
                let var121_e  =
                    fun1_head (arg117_remaining)
                in
                let var122_t  =
                    fun3_tail (arg117_remaining)
                in
                if ( < ) (var121_e) (arg114_pivot) then
                    fun112_quicksort_rec (arg114_pivot) ((fun x xs -> Array.append [|x|] xs) (var121_e) (arg115_lt_pivot)) (arg116_geq_pivot) (var122_t)
                else
                    fun112_quicksort_rec (arg114_pivot) (arg115_lt_pivot) ((fun x xs -> Array.append [|x|] xs) (var121_e) (arg116_geq_pivot)) (var122_t)
        and fun113_quicksort arg123_arr =
            if ( <= ) (Array.length (arg123_arr)) (1) then
                arg123_arr
            else
                fun112_quicksort_rec (fun1_head (arg123_arr)) ([||]) ([||]) (fun3_tail (arg123_arr))
    in
    let fun124_bm_sort arg110_arr =
        let var111_n  =
            Array.length (arg110_arr)
        in
        fun113_quicksort (arg110_arr)
    in
    let fun128_bm_median arg125_arr =
        let var126_n  =
            Array.length (arg125_arr)
        in
        let var127_sorted  =
            fun124_bm_sort (arg125_arr)
        in
        if ( = ) (( mod ) (var126_n) (2)) (0) then
            ( /. ) (( +. ) (Array.get (arg125_arr) (( - ) (( / ) (var126_n) (2)) (1))) (Array.get (arg125_arr) (( / ) (var126_n) (2)))) (2.0e+0)
        else
            Array.get (arg125_arr) (( / ) (var126_n) (2))
    in
    let rec fun131_work arg135_arr arg134_n arg132_i arg133_acc =
            if ( = ) (arg132_i) (arg134_n) then
                arg133_acc
            else
                fun131_work (arg135_arr) (arg134_n) (( + ) (arg132_i) (1)) (( +. ) (arg133_acc) (Array.get (arg135_arr) (arg132_i)))
    in
    let fun136_bm_sum arg129_arr =
        let var130_n  =
            Array.length (arg129_arr)
        in
        fun131_work (arg129_arr) (var130_n) (0) (0.0)
    in
    let rec fun139_work arg143_arr arg142_n arg140_i arg141_acc =
            if ( = ) (arg140_i) (arg142_n) then
                arg141_acc
            else
                let var144_e  =
                    Array.get (arg143_arr) (arg140_i)
                in
                fun139_work (arg143_arr) (arg142_n) (( + ) (arg140_i) (1)) (if ( > ) (var144_e) (arg141_acc) then
                    var144_e
                else
                    arg141_acc)
    in
    let fun145_bm_max arg137_arr =
        let var138_n  =
            Array.length (arg137_arr)
        in
        fun139_work (arg137_arr) (var138_n) (1) (Array.get (arg137_arr) (0))
    in
    let rec fun148_work arg152_arr arg151_n arg149_i arg150_acc =
            if ( = ) (arg149_i) (arg151_n) then
                arg150_acc
            else
                let var153_e  =
                    Array.get (arg152_arr) (arg149_i)
                in
                fun148_work (arg152_arr) (arg151_n) (( + ) (arg149_i) (1)) (if ( < ) (var153_e) (arg150_acc) then
                    var153_e
                else
                    arg150_acc)
    in
    let fun154_bm_min arg146_arr =
        let var147_n  =
            Array.length (arg146_arr)
        in
        fun148_work (arg146_arr) (var147_n) (1) (Array.get (arg146_arr) (0))
    in
    let fun157_bm_dist arg155_a arg156_b =
        if ( > ) (arg155_a) (arg156_b) then
            ( -. ) (arg155_a) (arg156_b)
        else
            ( -. ) (arg156_b) (arg155_a)
    in
    let var77_vecsize  =
        1000000
    in
    let var84_scalarA  =
        2.389999e+2
    in
    let var85_vecX  =
        fun14_seqInit (var77_vecsize) (fun83_vecXinitfun)
    in
    let var86_scalarY  =
        1.90e+1
    in
    let var158__  =
        ()
    in
    let var159_bmres_warmup  =
        fun109_bm_runmultiple (var85_vecX) (var86_scalarY) (var84_scalarA) (4)
    in
    let var160__  =
        ()
    in
    let var161_bmres_iters  =
        fun109_bm_runmultiple (var85_vecX) (var86_scalarY) (var84_scalarA) (15)
    in
    let var187__  =
        let var162_median  =
            fun128_bm_median (var161_bmres_iters)
        in
        let var163_sum  =
            fun136_bm_sum (var161_bmres_iters)
        in
        let var164_avg  =
            ( /. ) (var163_sum) (1.50e+1)
        in
        let var165_max  =
            fun145_bm_max (var161_bmres_iters)
        in
        let var166_min  =
            fun154_bm_min (var161_bmres_iters)
        in
        let var167_variance  =
            fun145_bm_max ([|fun157_bm_dist (var164_avg) (var165_max); fun157_bm_dist (var164_avg) (var166_min)|])
        in
        let var168__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'='; '='; ' '; 'I'; 'T'; 'E'; 'R'; 'A'; 'T'; 'I'; 'O'; 'N'; ' '; 'R'; 'E'; 'S'; 'U'; 'L'; 'T'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var169__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'i'; 't'; 'e'; 'r'; 'a'; 't'; 'i'; 'o'; 'n'; 's'; ':'; ' '|])
        in
        let var170__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'1'; '5'|])
        in
        let var171__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var172__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var173__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var162_median))
        in
        let var174__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 's'; 'e'; 'c'; 'o'; 'n'; 'd'; 's'; '\n'|])
        in
        let var175__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var176__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var165_max))
        in
        let var177__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 's'; 'e'; 'c'; 'o'; 'n'; 'd'; 's'; '\n'|])
        in
        let var178__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var179__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var166_min))
        in
        let var180__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 's'; 'e'; 'c'; 'o'; 'n'; 'd'; 's'; '\n'|])
        in
        let var181__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var182__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var164_avg))
        in
        let var183__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 's'; 'e'; 'c'; 'o'; 'n'; 'd'; 's'; '\n'|])
        in
        let var184__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var185__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var167_variance))
        in
        let var186__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 's'; 'e'; 'c'; 'o'; 'n'; 'd'; 's'; '\n'|])
        in
        ()
    in
    ()