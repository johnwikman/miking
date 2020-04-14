open Printf

external gpuhost_fun81_vecaddf: int array -> float array -> float array -> float array -> float array = "gpuhost_fun81_vecaddf"

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
    let fun81_vecaddf arg78_x arg79_y arg80_i =
        ( +. ) (Array.get (arg78_x) (arg80_i)) (Array.get (arg79_y) (arg80_i))
    in
    let fun83_vecXinitfun arg82_i =
        ( /. ) (float_of_int (( mod ) (( * ) (arg82_i) (arg82_i)) (30269))) (3.0e+0)
    in
    let fun85_vecYinitfun arg84_i =
        ( /. ) (float_of_int (( mod ) (( * ) (( * ) (arg84_i) (arg84_i)) (arg84_i)) (30271))) (7.0e+0)
    in
    let fun95_bm_runonce arg92_vecY arg91_vecX arg90_vecsize arg88__ =
        let var89_bm_t_start  =
            Unix.gettimeofday (())
        in
        let var93_vecS  =
            gpuhost_fun81_vecaddf [|1; arg90_vecsize|] [||] (arg91_vecX) (arg92_vecY)
        in
        let var94_bm_t_end  =
            Unix.gettimeofday (())
        in
        ( -. ) (var94_bm_t_end) (var89_bm_t_start)
    in
    let rec fun97_bm_iter arg104_vecsize arg103_vecX arg102_vecY arg100_n arg98_i arg99_acc =
            if ( >= ) (arg98_i) (arg100_n) then
                arg99_acc
            else
                let var101__  =
                    ()
                in
                let var105_res  =
                    fun95_bm_runonce (arg102_vecY) (arg103_vecX) (arg104_vecsize) (())
                in
                let var106_newacc  =
                    Array.append (arg99_acc) ([|var105_res|])
                in
                fun97_bm_iter (arg104_vecsize) (arg103_vecX) (arg102_vecY) (arg100_n) (( + ) (arg98_i) (1)) (var106_newacc)
    in
    let fun110_bm_runmultiple arg109_vecY arg108_vecX arg107_vecsize arg96_n =
        fun97_bm_iter (arg107_vecsize) (arg108_vecX) (arg109_vecY) (arg96_n) (0) ([||])
    in
    let rec fun113_quicksort_rec arg115_pivot arg116_lt_pivot arg117_geq_pivot arg118_remaining =
            if ( = ) (Array.length (arg118_remaining)) (0) then
                let var119_seq_lt  =
                    fun114_quicksort (arg116_lt_pivot)
                in
                let var120_seq_pivot  =
                    [|arg115_pivot|]
                in
                let var121_seq_geq  =
                    fun114_quicksort (arg117_geq_pivot)
                in
                Array.append (Array.append (var119_seq_lt) (var120_seq_pivot)) (var121_seq_geq)
            else
                let var122_e  =
                    fun1_head (arg118_remaining)
                in
                let var123_t  =
                    fun3_tail (arg118_remaining)
                in
                if ( < ) (var122_e) (arg115_pivot) then
                    fun113_quicksort_rec (arg115_pivot) ((fun x xs -> Array.append [|x|] xs) (var122_e) (arg116_lt_pivot)) (arg117_geq_pivot) (var123_t)
                else
                    fun113_quicksort_rec (arg115_pivot) (arg116_lt_pivot) ((fun x xs -> Array.append [|x|] xs) (var122_e) (arg117_geq_pivot)) (var123_t)
        and fun114_quicksort arg124_arr =
            if ( <= ) (Array.length (arg124_arr)) (1) then
                arg124_arr
            else
                fun113_quicksort_rec (fun1_head (arg124_arr)) ([||]) ([||]) (fun3_tail (arg124_arr))
    in
    let fun125_bm_sort arg111_arr =
        let var112_n  =
            Array.length (arg111_arr)
        in
        fun114_quicksort (arg111_arr)
    in
    let fun129_bm_median arg126_arr =
        let var127_n  =
            Array.length (arg126_arr)
        in
        let var128_sorted  =
            fun125_bm_sort (arg126_arr)
        in
        if ( = ) (( mod ) (var127_n) (2)) (0) then
            ( /. ) (( +. ) (Array.get (arg126_arr) (( - ) (( / ) (var127_n) (2)) (1))) (Array.get (arg126_arr) (( / ) (var127_n) (2)))) (2.0e+0)
        else
            Array.get (arg126_arr) (( / ) (var127_n) (2))
    in
    let rec fun132_work arg136_arr arg135_n arg133_i arg134_acc =
            if ( = ) (arg133_i) (arg135_n) then
                arg134_acc
            else
                fun132_work (arg136_arr) (arg135_n) (( + ) (arg133_i) (1)) (( +. ) (arg134_acc) (Array.get (arg136_arr) (arg133_i)))
    in
    let fun137_bm_sum arg130_arr =
        let var131_n  =
            Array.length (arg130_arr)
        in
        fun132_work (arg130_arr) (var131_n) (0) (0.0)
    in
    let rec fun140_work arg144_arr arg143_n arg141_i arg142_acc =
            if ( = ) (arg141_i) (arg143_n) then
                arg142_acc
            else
                let var145_e  =
                    Array.get (arg144_arr) (arg141_i)
                in
                fun140_work (arg144_arr) (arg143_n) (( + ) (arg141_i) (1)) (if ( > ) (var145_e) (arg142_acc) then
                    var145_e
                else
                    arg142_acc)
    in
    let fun146_bm_max arg138_arr =
        let var139_n  =
            Array.length (arg138_arr)
        in
        fun140_work (arg138_arr) (var139_n) (1) (Array.get (arg138_arr) (0))
    in
    let rec fun149_work arg153_arr arg152_n arg150_i arg151_acc =
            if ( = ) (arg150_i) (arg152_n) then
                arg151_acc
            else
                let var154_e  =
                    Array.get (arg153_arr) (arg150_i)
                in
                fun149_work (arg153_arr) (arg152_n) (( + ) (arg150_i) (1)) (if ( < ) (var154_e) (arg151_acc) then
                    var154_e
                else
                    arg151_acc)
    in
    let fun155_bm_min arg147_arr =
        let var148_n  =
            Array.length (arg147_arr)
        in
        fun149_work (arg147_arr) (var148_n) (1) (Array.get (arg147_arr) (0))
    in
    let fun158_bm_dist arg156_a arg157_b =
        if ( > ) (arg156_a) (arg157_b) then
            ( -. ) (arg156_a) (arg157_b)
        else
            ( -. ) (arg157_b) (arg156_a)
    in
    let var77_vecsize  =
        8192
    in
    let var86_vecX  =
        fun14_seqInit (var77_vecsize) (fun83_vecXinitfun)
    in
    let var87_vecY  =
        fun14_seqInit (var77_vecsize) (fun85_vecYinitfun)
    in
    let var159__  =
        ()
    in
    let var160_bmres_warmup  =
        fun110_bm_runmultiple (var87_vecY) (var86_vecX) (var77_vecsize) (4)
    in
    let var161__  =
        ()
    in
    let var162_bmres_iters  =
        fun110_bm_runmultiple (var87_vecY) (var86_vecX) (var77_vecsize) (15)
    in
    let var225__  =
        let var163_median  =
            fun129_bm_median (var162_bmres_iters)
        in
        let var164_sum  =
            fun137_bm_sum (var162_bmres_iters)
        in
        let var165_avg  =
            ( /. ) (var164_sum) (1.50e+1)
        in
        let var166_max  =
            fun146_bm_max (var162_bmres_iters)
        in
        let var167_min  =
            fun155_bm_min (var162_bmres_iters)
        in
        let var168_variance  =
            fun146_bm_max ([|fun158_bm_dist (var165_avg) (var166_max); fun158_bm_dist (var165_avg) (var167_min)|])
        in
        let var169_median  =
            ( *. ) (var163_median) (1.0e+3)
        in
        let var170_sum  =
            ( *. ) (var164_sum) (1.0e+3)
        in
        let var171_avg  =
            ( *. ) (var165_avg) (1.0e+3)
        in
        let var172_max  =
            ( *. ) (var166_max) (1.0e+3)
        in
        let var173_min  =
            ( *. ) (var167_min) (1.0e+3)
        in
        let var174_variance  =
            ( *. ) (var168_variance) (1.0e+3)
        in
        let var175__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'='; '='; ' '; 'I'; 'T'; 'E'; 'R'; 'A'; 'T'; 'I'; 'O'; 'N'; ' '; 'R'; 'E'; 'S'; 'U'; 'L'; 'T'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var176__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'i'; 't'; 'e'; 'r'; 'a'; 't'; 'i'; 'o'; 'n'; 's'; ':'; ' '|])
        in
        let var177__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'1'; '5'|])
        in
        let var178__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var179__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var180__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var169_median))
        in
        let var181__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var182__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var183__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var172_max))
        in
        let var184__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var185__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var186__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var173_min))
        in
        let var187__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var188__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var189__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var171_avg))
        in
        let var190__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var191__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var192__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var174_variance))
        in
        let var193__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var194_median  =
            fun129_bm_median (var160_bmres_warmup)
        in
        let var195_sum  =
            fun137_bm_sum (var160_bmres_warmup)
        in
        let var196_avg  =
            ( /. ) (var195_sum) (4.0e+0)
        in
        let var197_max  =
            fun146_bm_max (var160_bmres_warmup)
        in
        let var198_min  =
            fun155_bm_min (var160_bmres_warmup)
        in
        let var199_variance  =
            fun146_bm_max ([|fun158_bm_dist (var196_avg) (var197_max); fun158_bm_dist (var196_avg) (var198_min)|])
        in
        let var200_median  =
            ( *. ) (var194_median) (1.0e+3)
        in
        let var201_sum  =
            ( *. ) (var195_sum) (1.0e+3)
        in
        let var202_avg  =
            ( *. ) (var196_avg) (1.0e+3)
        in
        let var203_max  =
            ( *. ) (var197_max) (1.0e+3)
        in
        let var204_min  =
            ( *. ) (var198_min) (1.0e+3)
        in
        let var205_variance  =
            ( *. ) (var199_variance) (1.0e+3)
        in
        let var206__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; '\n'; '='; '='; ' '; 'W'; 'A'; 'R'; 'M'; 'U'; 'P'; ' '; 'S'; 'T'; 'A'; 'T'; 'I'; 'S'; 'T'; 'I'; 'C'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var207__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'w'; 'a'; 'r'; 'm'; 'u'; 'p'; ' '; 'r'; 'u'; 'n'; 's'; ':'; ' '|])
        in
        let var208__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'4'|])
        in
        let var209__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var210__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var211__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var200_median))
        in
        let var212__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var213__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var214__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var203_max))
        in
        let var215__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var216__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var217__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var204_min))
        in
        let var218__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var219__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var220__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var202_avg))
        in
        let var221__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var222__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var223__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var205_variance))
        in
        let var224__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        ()
    in
    ()