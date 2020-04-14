open Printf

external gpuhost_fun80_vecscalef: int array -> float array -> float array -> float array = "gpuhost_fun80_vecscalef"

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
    let fun80_vecscalef arg78_a arg79_xelem =
        ( *. ) (arg78_a) (arg79_xelem)
    in
    let fun82_vecXinitfun arg81_i =
        ( /. ) (float_of_int (( mod ) (( * ) (arg81_i) (arg81_i)) (30269))) (3.0e+0)
    in
    let fun91_bm_runonce arg88_vecX arg87_scalarA arg85__ =
        let var86_bm_t_start  =
            Unix.gettimeofday (())
        in
        let var89_vecS  =
            if (( < ) (( * ) (Array.length (arg88_vecX)) (( + ) (50) (11))) (( + ) (( + ) (( * ) (( + ) (Array.length (arg88_vecX)) (Array.length (arg88_vecX))) (10)) (40000000)) (( * ) (34) (( / ) (( + ) (Array.length (arg88_vecX)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun80_vecscalef (arg87_scalarA)) (arg88_vecX)) else (gpuhost_fun80_vecscalef [|1|] [|arg87_scalarA|] (arg88_vecX))
        in
        let var90_bm_t_end  =
            Unix.gettimeofday (())
        in
        ( -. ) (var90_bm_t_end) (var86_bm_t_start)
    in
    let rec fun93_bm_iter arg99_scalarA arg98_vecX arg96_n arg94_i arg95_acc =
            if ( >= ) (arg94_i) (arg96_n) then
                arg95_acc
            else
                let var97__  =
                    ()
                in
                let var100_res  =
                    fun91_bm_runonce (arg98_vecX) (arg99_scalarA) (())
                in
                let var101_newacc  =
                    Array.append (arg95_acc) ([|var100_res|])
                in
                fun93_bm_iter (arg99_scalarA) (arg98_vecX) (arg96_n) (( + ) (arg94_i) (1)) (var101_newacc)
    in
    let fun104_bm_runmultiple arg103_vecX arg102_scalarA arg92_n =
        fun93_bm_iter (arg102_scalarA) (arg103_vecX) (arg92_n) (0) ([||])
    in
    let rec fun107_quicksort_rec arg109_pivot arg110_lt_pivot arg111_geq_pivot arg112_remaining =
            if ( = ) (Array.length (arg112_remaining)) (0) then
                let var113_seq_lt  =
                    fun108_quicksort (arg110_lt_pivot)
                in
                let var114_seq_pivot  =
                    [|arg109_pivot|]
                in
                let var115_seq_geq  =
                    fun108_quicksort (arg111_geq_pivot)
                in
                Array.append (Array.append (var113_seq_lt) (var114_seq_pivot)) (var115_seq_geq)
            else
                let var116_e  =
                    fun1_head (arg112_remaining)
                in
                let var117_t  =
                    fun3_tail (arg112_remaining)
                in
                if ( < ) (var116_e) (arg109_pivot) then
                    fun107_quicksort_rec (arg109_pivot) ((fun x xs -> Array.append [|x|] xs) (var116_e) (arg110_lt_pivot)) (arg111_geq_pivot) (var117_t)
                else
                    fun107_quicksort_rec (arg109_pivot) (arg110_lt_pivot) ((fun x xs -> Array.append [|x|] xs) (var116_e) (arg111_geq_pivot)) (var117_t)
        and fun108_quicksort arg118_arr =
            if ( <= ) (Array.length (arg118_arr)) (1) then
                arg118_arr
            else
                fun107_quicksort_rec (fun1_head (arg118_arr)) ([||]) ([||]) (fun3_tail (arg118_arr))
    in
    let fun119_bm_sort arg105_arr =
        let var106_n  =
            Array.length (arg105_arr)
        in
        fun108_quicksort (arg105_arr)
    in
    let fun123_bm_median arg120_arr =
        let var121_n  =
            Array.length (arg120_arr)
        in
        let var122_sorted  =
            fun119_bm_sort (arg120_arr)
        in
        if ( = ) (( mod ) (var121_n) (2)) (0) then
            ( /. ) (( +. ) (Array.get (arg120_arr) (( - ) (( / ) (var121_n) (2)) (1))) (Array.get (arg120_arr) (( / ) (var121_n) (2)))) (2.0e+0)
        else
            Array.get (arg120_arr) (( / ) (var121_n) (2))
    in
    let rec fun126_work arg130_arr arg129_n arg127_i arg128_acc =
            if ( = ) (arg127_i) (arg129_n) then
                arg128_acc
            else
                fun126_work (arg130_arr) (arg129_n) (( + ) (arg127_i) (1)) (( +. ) (arg128_acc) (Array.get (arg130_arr) (arg127_i)))
    in
    let fun131_bm_sum arg124_arr =
        let var125_n  =
            Array.length (arg124_arr)
        in
        fun126_work (arg124_arr) (var125_n) (0) (0.0)
    in
    let rec fun134_work arg138_arr arg137_n arg135_i arg136_acc =
            if ( = ) (arg135_i) (arg137_n) then
                arg136_acc
            else
                let var139_e  =
                    Array.get (arg138_arr) (arg135_i)
                in
                fun134_work (arg138_arr) (arg137_n) (( + ) (arg135_i) (1)) (if ( > ) (var139_e) (arg136_acc) then
                    var139_e
                else
                    arg136_acc)
    in
    let fun140_bm_max arg132_arr =
        let var133_n  =
            Array.length (arg132_arr)
        in
        fun134_work (arg132_arr) (var133_n) (1) (Array.get (arg132_arr) (0))
    in
    let rec fun143_work arg147_arr arg146_n arg144_i arg145_acc =
            if ( = ) (arg144_i) (arg146_n) then
                arg145_acc
            else
                let var148_e  =
                    Array.get (arg147_arr) (arg144_i)
                in
                fun143_work (arg147_arr) (arg146_n) (( + ) (arg144_i) (1)) (if ( < ) (var148_e) (arg145_acc) then
                    var148_e
                else
                    arg145_acc)
    in
    let fun149_bm_min arg141_arr =
        let var142_n  =
            Array.length (arg141_arr)
        in
        fun143_work (arg141_arr) (var142_n) (1) (Array.get (arg141_arr) (0))
    in
    let fun152_bm_dist arg150_a arg151_b =
        if ( > ) (arg150_a) (arg151_b) then
            ( -. ) (arg150_a) (arg151_b)
        else
            ( -. ) (arg151_b) (arg150_a)
    in
    let var77_vecsize  =
        8192
    in
    let var83_vecX  =
        fun14_seqInit (var77_vecsize) (fun82_vecXinitfun)
    in
    let var84_scalarA  =
        3.485199e+1
    in
    let var153__  =
        ()
    in
    let var154_bmres_warmup  =
        fun104_bm_runmultiple (var83_vecX) (var84_scalarA) (4)
    in
    let var155__  =
        ()
    in
    let var156_bmres_iters  =
        fun104_bm_runmultiple (var83_vecX) (var84_scalarA) (15)
    in
    let var219__  =
        let var157_median  =
            fun123_bm_median (var156_bmres_iters)
        in
        let var158_sum  =
            fun131_bm_sum (var156_bmres_iters)
        in
        let var159_avg  =
            ( /. ) (var158_sum) (1.50e+1)
        in
        let var160_max  =
            fun140_bm_max (var156_bmres_iters)
        in
        let var161_min  =
            fun149_bm_min (var156_bmres_iters)
        in
        let var162_variance  =
            fun140_bm_max ([|fun152_bm_dist (var159_avg) (var160_max); fun152_bm_dist (var159_avg) (var161_min)|])
        in
        let var163_median  =
            ( *. ) (var157_median) (1.0e+3)
        in
        let var164_sum  =
            ( *. ) (var158_sum) (1.0e+3)
        in
        let var165_avg  =
            ( *. ) (var159_avg) (1.0e+3)
        in
        let var166_max  =
            ( *. ) (var160_max) (1.0e+3)
        in
        let var167_min  =
            ( *. ) (var161_min) (1.0e+3)
        in
        let var168_variance  =
            ( *. ) (var162_variance) (1.0e+3)
        in
        let var169__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'='; '='; ' '; 'I'; 'T'; 'E'; 'R'; 'A'; 'T'; 'I'; 'O'; 'N'; ' '; 'R'; 'E'; 'S'; 'U'; 'L'; 'T'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var170__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'i'; 't'; 'e'; 'r'; 'a'; 't'; 'i'; 'o'; 'n'; 's'; ':'; ' '|])
        in
        let var171__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'1'; '5'|])
        in
        let var172__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var173__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var174__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var163_median))
        in
        let var175__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var176__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var177__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var166_max))
        in
        let var178__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var179__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var180__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var167_min))
        in
        let var181__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var182__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var183__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var165_avg))
        in
        let var184__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var185__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var186__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var168_variance))
        in
        let var187__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var188_median  =
            fun123_bm_median (var154_bmres_warmup)
        in
        let var189_sum  =
            fun131_bm_sum (var154_bmres_warmup)
        in
        let var190_avg  =
            ( /. ) (var189_sum) (4.0e+0)
        in
        let var191_max  =
            fun140_bm_max (var154_bmres_warmup)
        in
        let var192_min  =
            fun149_bm_min (var154_bmres_warmup)
        in
        let var193_variance  =
            fun140_bm_max ([|fun152_bm_dist (var190_avg) (var191_max); fun152_bm_dist (var190_avg) (var192_min)|])
        in
        let var194_median  =
            ( *. ) (var188_median) (1.0e+3)
        in
        let var195_sum  =
            ( *. ) (var189_sum) (1.0e+3)
        in
        let var196_avg  =
            ( *. ) (var190_avg) (1.0e+3)
        in
        let var197_max  =
            ( *. ) (var191_max) (1.0e+3)
        in
        let var198_min  =
            ( *. ) (var192_min) (1.0e+3)
        in
        let var199_variance  =
            ( *. ) (var193_variance) (1.0e+3)
        in
        let var200__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; '\n'; '='; '='; ' '; 'W'; 'A'; 'R'; 'M'; 'U'; 'P'; ' '; 'S'; 'T'; 'A'; 'T'; 'I'; 'S'; 'T'; 'I'; 'C'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var201__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'w'; 'a'; 'r'; 'm'; 'u'; 'p'; ' '; 'r'; 'u'; 'n'; 's'; ':'; ' '|])
        in
        let var202__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'4'|])
        in
        let var203__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var204__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var205__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var194_median))
        in
        let var206__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var207__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var208__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var197_max))
        in
        let var209__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var210__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var211__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var198_min))
        in
        let var212__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var213__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var214__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var196_avg))
        in
        let var215__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var216__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var217__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var199_variance))
        in
        let var218__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        ()
    in
    ()