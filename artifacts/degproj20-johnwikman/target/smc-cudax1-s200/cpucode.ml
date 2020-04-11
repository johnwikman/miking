open Printf

external gpuhost_fun114_initfun: int array -> float array -> float array -> float array = "gpuhost_fun114_initfun"
external gpuhost_fun131_initfun: int array -> float array -> float array -> float array = "gpuhost_fun131_initfun"
external gpuhost_fun258_wmapf: int array -> float array -> float array -> float array = "gpuhost_fun258_wmapf"
external gpuhost_fun253_wmapf: int array -> float array -> float array -> float array = "gpuhost_fun253_wmapf"
external gpuhost_fun248_wmapf: int array -> float array -> float array -> float array -> float array = "gpuhost_fun248_wmapf"
external gpuhost_fun236_propagate_x: int array -> float array -> float array -> float array -> float array = "gpuhost_fun236_propagate_x"
external gpuhost_fun207_wmapf: int array -> float array -> float array -> float array = "gpuhost_fun207_wmapf"
external gpuhost_fun202_wmapf: int array -> float array -> float array -> float array = "gpuhost_fun202_wmapf"
external gpuhost_fun197_wmapf: int array -> float array -> float array -> float array -> float array = "gpuhost_fun197_wmapf"
external gpuhost_fun187_rndinitf: int array -> float array -> float array = "gpuhost_fun187_rndinitf"

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
    let fun95_g_map arg89_altitude arg90_hgtmap arg91_mapsize arg92_x =
        let var93_p  =
            (fun x -> int_of_float (Float.floor x)) (arg92_x)
        in
        let var94_n  =
            (fun x -> int_of_float (Float.ceil x)) (arg92_x)
        in
        if ( || ) (( < ) (var93_p) (0)) (( > ) (var94_n) (( - ) (arg91_mapsize) (1))) then
            1.0e+5
        else
            ( -. ) (arg89_altitude) (( +. ) (Array.get (arg90_hgtmap) (var93_p)) (( *. ) (( -. ) (Array.get (arg90_hgtmap) (var94_n)) (Array.get (arg90_hgtmap) (var93_p))) (( -. ) (arg92_x) (float_of_int (var93_p)))))
    in
    let rec fun99_maxwork arg100_s arg101_i arg102_end arg103_candidate =
            if ( = ) (arg101_i) (arg102_end) then
                arg103_candidate
            else
                fun99_maxwork (arg100_s) (( + ) (arg101_i) (1)) (arg102_end) (if ( > ) (Array.get (arg100_s) (arg101_i)) (arg103_candidate) then
                    Array.get (arg100_s) (arg101_i)
                else
                    arg103_candidate)
    in
    let fun107_maxwrap arg104_s arg105_i arg106_end =
        if ( = ) (arg105_i) (arg106_end) then
            ( ~-. ) (1.000000e+300)
        else
            fun99_maxwork (arg104_s) (( + ) (arg105_i) (1)) (arg106_end) (Array.get (arg104_s) (0))
    in
    let fun114_initfun arg113_s arg111_s_len arg108_i =
        let var109_start  =
            ( * ) (arg108_i) (4096)
        in
        let var110_tmp  =
            ( + ) (var109_start) (4096)
        in
        let var112_end  =
            if ( > ) (var110_tmp) (arg111_s_len) then
                arg111_s_len
            else
                var110_tmp
        in
        fun107_maxwrap (arg113_s) (var109_start) (var112_end)
    in
    let fun116_seqMaxf arg96_s =
        let var97_s_len  =
            Array.length (arg96_s)
        in
        let var98_s_initsize  =
            ( / ) (( + ) (var97_s_len) (4095)) (4096)
        in
        let var115_partial  =
            gpuhost_fun114_initfun [|1; var97_s_len; var98_s_initsize|] [||] (arg96_s)
        in
        fun107_maxwrap (var115_partial) (0) (var98_s_initsize)
    in
    let rec fun120_sumwork arg121_s arg122_i arg123_end arg124_acc =
            if ( = ) (arg122_i) (arg123_end) then
                arg124_acc
            else
                fun120_sumwork (arg121_s) (( + ) (arg122_i) (1)) (arg123_end) (( +. ) (arg124_acc) (Array.get (arg121_s) (arg122_i)))
    in
    let fun131_initfun arg130_s arg128_s_len arg125_i =
        let var126_start  =
            ( * ) (arg125_i) (4096)
        in
        let var127_tmp  =
            ( + ) (var126_start) (4096)
        in
        let var129_end  =
            if ( > ) (var127_tmp) (arg128_s_len) then
                arg128_s_len
            else
                var127_tmp
        in
        fun120_sumwork (arg130_s) (var126_start) (var129_end) (0.0)
    in
    let fun133_seqSumf arg117_s =
        let var118_s_len  =
            Array.length (arg117_s)
        in
        let var119_s_initsize  =
            ( / ) (( + ) (var118_s_len) (4095)) (4096)
        in
        let var132_partial  =
            gpuhost_fun131_initfun [|1; var118_s_len; var119_s_initsize|] [||] (arg117_s)
        in
        fun120_sumwork (var132_partial) (0) (var119_s_initsize) (0.0)
    in
    let fun135_seqAccsumf arg134_s =
        (fun f acc xs -> let a = Array.copy xs in let accref = ref acc in Array.iteri (fun i e -> accref := f !accref e; a.(i) <- !accref) xs; a) (( +. )) (0.0) (arg134_s)
    in
    let rec fun136_binsearch arg137_vec arg138_p arg139_low arg140_up =
            let var141_mid  =
                ( / ) (( - ) (( + ) (arg139_low) (arg140_up)) (1)) (2)
            in
            if ( = ) (arg139_low) (arg140_up) then
                arg139_low
            else
                if ( < ) (arg138_p) (Array.get (arg137_vec) (var141_mid)) then
                    fun136_binsearch (arg137_vec) (arg138_p) (arg139_low) (var141_mid)
                else
                    fun136_binsearch (arg137_vec) (arg138_p) (( + ) (var141_mid) (1)) (arg140_up)
    in
    let fun153_pl arg151_w arg148_x arg145_j2 =
        let var146__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun19_int2string (arg145_j2))
        in
        let var147__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'	'|])
        in
        let var149__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (Array.get (arg148_x) (arg145_j2)))
        in
        let var150__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'	'; '8'; '0'; '.'; '0'; '	'|])
        in
        let var152__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (Array.get (arg151_w) (arg145_j2)))
        in
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
    in
    let rec fun158_printloop arg162_x arg161_w arg160_x arg159_j =
            if ( >= ) (arg159_j) (Array.length (arg160_x)) then
                ()
            else
                let var163__  =
                    fun153_pl (arg161_w) (arg162_x) (arg159_j)
                in
                fun158_printloop (arg162_x) (arg161_w) (arg160_x) (( + ) (arg159_j) (1))
    in
    let fun165_pgf_print arg142_i arg143_x arg144_w =
        let var154__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; 'x'; '['|])
        in
        let var155__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun19_int2string (arg142_i))
        in
        let var156__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|']'; ':'; '\n'|])
        in
        let var157__  =
            if ( = ) (Array.length (arg143_x)) (0) then
                ()
            else
                fun153_pl (arg144_w) (arg143_x) (0)
        in
        let var164__  =
            fun158_printloop (arg143_x) (arg144_w) (arg143_x) (1)
        in
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
    in
    let fun187_rndinitf arg186_xUpperBound arg185_xLowerBound arg184__ =
        (fun x y -> x +. (Random.float (Float.abs (x -. y)))) (arg185_xLowerBound) (arg186_xUpperBound)
    in
    let fun197_wmapf arg196_sigma arg195_heightMapSize arg194_heightMap arg193_altitude arg192_fstobs arg191_xelem =
        (fun x mu sigma -> -0.5 *. (x -. mu) *. (x -. mu) /. (sigma *. sigma) -. (Float.log (sigma *. Float.sqrt (2.0 *. 3.14159265359)))) (arg192_fstobs) (fun95_g_map (arg193_altitude) (arg194_heightMap) (arg195_heightMapSize) (arg191_xelem)) (arg196_sigma)
    in
    let fun202_wmapf arg201_wmax arg200_welem =
        Float.exp (( -. ) (arg200_welem) (arg201_wmax))
    in
    let fun207_wmapf arg206_wsum arg205_welem =
        ( /. ) (arg205_welem) (arg206_wsum)
    in
    let fun236_propagate_x arg235_sigma arg234_velocity arg232_x arg230_nPoints arg229_wacc arg227__ =
        let var228_p  =
            (fun x y -> x +. (Random.float (Float.abs (x -. y)))) (0.0) (1.0e-0)
        in
        let var231_i  =
            fun136_binsearch (arg229_wacc) (var228_p) (0) (( - ) (arg230_nPoints) (1))
        in
        let var233_x_new  =
            Array.get (arg232_x) (var231_i)
        in
        (fun mu sigma -> mu +. (sigma *. (Float.sqrt (-2.0 *. Float.log (Random.float 1.0))) *. (Float.cos (2.0 *. 3.14159265359 *. (Random.float 1.0))))) (( +. ) (var233_x_new) (arg234_velocity)) (arg235_sigma)
    in
    let fun248_wmapf arg247_log_1onN arg246_sigma arg245_heightMapSize arg244_heightMap arg243_altitude arg242_ithobs arg241_xelem =
        ( +. ) ((fun x mu sigma -> -0.5 *. (x -. mu) *. (x -. mu) /. (sigma *. sigma) -. (Float.log (sigma *. Float.sqrt (2.0 *. 3.14159265359)))) (arg242_ithobs) (fun95_g_map (arg243_altitude) (arg244_heightMap) (arg245_heightMapSize) (arg241_xelem)) (arg246_sigma)) (arg247_log_1onN)
    in
    let fun253_wmapf arg252_wmax arg251_welem =
        Float.exp (( -. ) (arg251_welem) (arg252_wmax))
    in
    let fun258_wmapf arg257_wsum arg256_welem =
        ( /. ) (arg256_welem) (arg257_wsum)
    in
    let rec fun209_iterate_smc arg239_obs arg224_heightMap arg222_heightMapSize arg220_nPoints arg218_altitude arg216_velocity arg214_sigma arg210_i arg211_steps arg212_x arg213_w =
            if ( = ) (arg210_i) (arg211_steps) then
                arg212_x
            else
                let var215_sigma  =
                    arg214_sigma
                in
                let var217_velocity  =
                    arg216_velocity
                in
                let var219_altitude  =
                    arg218_altitude
                in
                let var221_nPoints  =
                    arg220_nPoints
                in
                let var223_heightMapSize  =
                    arg222_heightMapSize
                in
                let var225_heightMap  =
                    arg224_heightMap
                in
                let var226_wacc  =
                    fun135_seqAccsumf (arg213_w)
                in
                let var237_x_propagated  =
                    gpuhost_fun236_propagate_x [|1; var221_nPoints; var221_nPoints|] [|var215_sigma; var217_velocity|] (arg212_x) (var226_wacc)
                in
                let var238_log_1onN  =
                    Float.log (( /. ) (1.0e-0) (float_of_int (var221_nPoints)))
                in
                let var240_ithobs  =
                    Array.get (arg239_obs) (arg210_i)
                in
                let var249_w  =
                    gpuhost_fun248_wmapf [|1; var223_heightMapSize|] [|var238_log_1onN; var215_sigma; var219_altitude; var240_ithobs|] (var225_heightMap) (var237_x_propagated)
                in
                let var250_wmax  =
                    fun116_seqMaxf (var249_w)
                in
                let var254_w  =
                    gpuhost_fun253_wmapf [|1|] [|var250_wmax|] (var249_w)
                in
                let var255_wsum  =
                    fun133_seqSumf (var254_w)
                in
                let var259_w_updated  =
                    gpuhost_fun258_wmapf [|1|] [|var255_wsum|] (var254_w)
                in
                fun209_iterate_smc (arg239_obs) (arg224_heightMap) (arg222_heightMapSize) (arg220_nPoints) (arg218_altitude) (arg216_velocity) (arg214_sigma) (( + ) (arg210_i) (1)) (arg211_steps) (var237_x_propagated) (var259_w_updated)
    in
    let fun263_bm_runonce arg260_flight_range arg189_obs arg182_heightMap arg180_heightMapSize arg178_nPoints arg176_altitude arg174_velocity arg172_sigma arg170_xUpperBound arg168_xLowerBound arg166__ =
        let var167_bm_t_start  =
            Unix.gettimeofday (())
        in
        let var169_xLowerBound  =
            arg168_xLowerBound
        in
        let var171_xUpperBound  =
            arg170_xUpperBound
        in
        let var173_sigma  =
            arg172_sigma
        in
        let var175_velocity  =
            arg174_velocity
        in
        let var177_altitude  =
            arg176_altitude
        in
        let var179_nPoints  =
            arg178_nPoints
        in
        let var181_heightMapSize  =
            arg180_heightMapSize
        in
        let var183_heightMap  =
            arg182_heightMap
        in
        let var188_x  =
            gpuhost_fun187_rndinitf [|1; var179_nPoints|] [|var171_xUpperBound; var169_xLowerBound|] 
        in
        let var190_fstobs  =
            Array.get (arg189_obs) (0)
        in
        let var198_w  =
            gpuhost_fun197_wmapf [|1; var181_heightMapSize|] [|var173_sigma; var177_altitude; var190_fstobs|] (var183_heightMap) (var188_x)
        in
        let var199_wmax  =
            fun116_seqMaxf (var198_w)
        in
        let var203_w  =
            gpuhost_fun202_wmapf [|1|] [|var199_wmax|] (var198_w)
        in
        let var204_wsum  =
            fun133_seqSumf (var203_w)
        in
        let var208_w  =
            gpuhost_fun207_wmapf [|1|] [|var204_wsum|] (var203_w)
        in
        let var261_x_res  =
            fun209_iterate_smc (arg189_obs) (var183_heightMap) (var181_heightMapSize) (var179_nPoints) (var177_altitude) (var175_velocity) (var173_sigma) (1) (arg260_flight_range) (var188_x) (var208_w)
        in
        let var262_bm_t_end  =
            Unix.gettimeofday (())
        in
        ( -. ) (var262_bm_t_end) (var167_bm_t_start)
    in
    let rec fun265_bm_iter arg279_xLowerBound arg278_xUpperBound arg277_sigma arg276_velocity arg275_altitude arg274_nPoints arg273_heightMapSize arg272_heightMap arg271_obs arg270_flight_range arg268_n arg266_i arg267_acc =
            if ( >= ) (arg266_i) (arg268_n) then
                arg267_acc
            else
                let var269__  =
                    ()
                in
                let var280_res  =
                    fun263_bm_runonce (arg270_flight_range) (arg271_obs) (arg272_heightMap) (arg273_heightMapSize) (arg274_nPoints) (arg275_altitude) (arg276_velocity) (arg277_sigma) (arg278_xUpperBound) (arg279_xLowerBound) (())
                in
                let var281_newacc  =
                    Array.append (arg267_acc) ([|var280_res|])
                in
                fun265_bm_iter (arg279_xLowerBound) (arg278_xUpperBound) (arg277_sigma) (arg276_velocity) (arg275_altitude) (arg274_nPoints) (arg273_heightMapSize) (arg272_heightMap) (arg271_obs) (arg270_flight_range) (arg268_n) (( + ) (arg266_i) (1)) (var281_newacc)
    in
    let fun292_bm_runmultiple arg291_flight_range arg290_obs arg289_heightMap arg288_heightMapSize arg287_nPoints arg286_altitude arg285_velocity arg284_sigma arg283_xUpperBound arg282_xLowerBound arg264_n =
        fun265_bm_iter (arg282_xLowerBound) (arg283_xUpperBound) (arg284_sigma) (arg285_velocity) (arg286_altitude) (arg287_nPoints) (arg288_heightMapSize) (arg289_heightMap) (arg290_obs) (arg291_flight_range) (arg264_n) (0) ([||])
    in
    let rec fun295_quicksort_rec arg297_pivot arg298_lt_pivot arg299_geq_pivot arg300_remaining =
            if ( = ) (Array.length (arg300_remaining)) (0) then
                let var301_seq_lt  =
                    fun296_quicksort (arg298_lt_pivot)
                in
                let var302_seq_pivot  =
                    [|arg297_pivot|]
                in
                let var303_seq_geq  =
                    fun296_quicksort (arg299_geq_pivot)
                in
                Array.append (Array.append (var301_seq_lt) (var302_seq_pivot)) (var303_seq_geq)
            else
                let var304_e  =
                    fun1_head (arg300_remaining)
                in
                let var305_t  =
                    fun3_tail (arg300_remaining)
                in
                if ( < ) (var304_e) (arg297_pivot) then
                    fun295_quicksort_rec (arg297_pivot) ((fun x xs -> Array.append [|x|] xs) (var304_e) (arg298_lt_pivot)) (arg299_geq_pivot) (var305_t)
                else
                    fun295_quicksort_rec (arg297_pivot) (arg298_lt_pivot) ((fun x xs -> Array.append [|x|] xs) (var304_e) (arg299_geq_pivot)) (var305_t)
        and fun296_quicksort arg306_arr =
            if ( <= ) (Array.length (arg306_arr)) (1) then
                arg306_arr
            else
                fun295_quicksort_rec (fun1_head (arg306_arr)) ([||]) ([||]) (fun3_tail (arg306_arr))
    in
    let fun307_bm_sort arg293_arr =
        let var294_n  =
            Array.length (arg293_arr)
        in
        fun296_quicksort (arg293_arr)
    in
    let fun311_bm_median arg308_arr =
        let var309_n  =
            Array.length (arg308_arr)
        in
        let var310_sorted  =
            fun307_bm_sort (arg308_arr)
        in
        if ( = ) (( mod ) (var309_n) (2)) (0) then
            ( /. ) (( +. ) (Array.get (arg308_arr) (( - ) (( / ) (var309_n) (2)) (1))) (Array.get (arg308_arr) (( / ) (var309_n) (2)))) (2.0e+0)
        else
            Array.get (arg308_arr) (( / ) (var309_n) (2))
    in
    let rec fun314_work arg318_arr arg317_n arg315_i arg316_acc =
            if ( = ) (arg315_i) (arg317_n) then
                arg316_acc
            else
                fun314_work (arg318_arr) (arg317_n) (( + ) (arg315_i) (1)) (( +. ) (arg316_acc) (Array.get (arg318_arr) (arg315_i)))
    in
    let fun319_bm_sum arg312_arr =
        let var313_n  =
            Array.length (arg312_arr)
        in
        fun314_work (arg312_arr) (var313_n) (0) (0.0)
    in
    let rec fun322_work arg326_arr arg325_n arg323_i arg324_acc =
            if ( = ) (arg323_i) (arg325_n) then
                arg324_acc
            else
                let var327_e  =
                    Array.get (arg326_arr) (arg323_i)
                in
                fun322_work (arg326_arr) (arg325_n) (( + ) (arg323_i) (1)) (if ( > ) (var327_e) (arg324_acc) then
                    var327_e
                else
                    arg324_acc)
    in
    let fun328_bm_max arg320_arr =
        let var321_n  =
            Array.length (arg320_arr)
        in
        fun322_work (arg320_arr) (var321_n) (1) (Array.get (arg320_arr) (0))
    in
    let rec fun331_work arg335_arr arg334_n arg332_i arg333_acc =
            if ( = ) (arg332_i) (arg334_n) then
                arg333_acc
            else
                let var336_e  =
                    Array.get (arg335_arr) (arg332_i)
                in
                fun331_work (arg335_arr) (arg334_n) (( + ) (arg332_i) (1)) (if ( < ) (var336_e) (arg333_acc) then
                    var336_e
                else
                    arg333_acc)
    in
    let fun337_bm_min arg329_arr =
        let var330_n  =
            Array.length (arg329_arr)
        in
        fun331_work (arg329_arr) (var330_n) (1) (Array.get (arg329_arr) (0))
    in
    let fun340_bm_dist arg338_a arg339_b =
        if ( > ) (arg338_a) (arg339_b) then
            ( -. ) (arg338_a) (arg339_b)
        else
            ( -. ) (arg339_b) (arg338_a)
    in
    let var77_precision  =
        200
    in
    let var78_mu  =
        0.0
    in
    let var79_sigma  =
        1.0e-0
    in
    let var80_nPoints  =
        var77_precision
    in
    let var81_xLowerBound  =
        0.0
    in
    let var82_xUpperBound  =
        2.0e+2
    in
    let var83_flight_range  =
        100
    in
    let var84_velocity  =
        2.0e+0
    in
    let var85_altitude  =
        7.0e+1
    in
    let var86_heightMapSize  =
        201
    in
    let var87_heightMap  =
        [|3.0e+1; 3.224347e+1; 3.348510e+1; 3.546133e+1; 3.861152e+1; 4.107755e+1; 4.024140e+1; 4.084070e+1; 3.993254e+1; 3.885179e+1; 3.830347e+1; 3.774197e+1; 3.891683e+1; 3.989299e+1; 4.067165e+1; 4.205351e+1; 4.337654e+1; 4.566911e+1; 4.587607e+1; 4.611399e+1; 4.471587e+1; 4.128318e+1; 4.093681e+1; 4.092580e+1; 3.964706e+1; 4.185049e+1; 4.081629e+1; 4.166584e+1; 4.246926e+1; 4.493693e+1; 4.705658e+1; 4.741800e+1; 4.750825e+1; 4.590868e+1; 4.296649e+1; 4.164295e+1; 4.104831e+1; 4.193612e+1; 4.328838e+1; 4.350659e+1; 4.411724e+1; 4.406551e+1; 4.348215e+1; 4.224752e+1; 4.418944e+1; 4.319262e+1; 4.190832e+1; 3.965744e+1; 3.952936e+1; 3.732410e+1; 3.697901e+1; 3.644260e+1; 3.759206e+1; 3.806061e+1; 3.783636e+1; 3.850742e+1; 3.920665e+1; 3.904942e+1; 3.868825e+1; 3.711290e+1; 3.576213e+1; 3.429654e+1; 3.353295e+1; 3.280378e+1; 3.162892e+1; 3.269022e+1; 3.328835e+1; 3.252929e+1; 3.352115e+1; 3.267134e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 2.0e+1; 5.193766e+1; 5.162370e+1; 5.213640e+1; 5.095951e+1; 5.013582e+1; 4.850096e+1; 4.827946e+1; 4.827906e+1; 4.795598e+1; 4.872751e+1; 4.840834e+1; 4.789986e+1; 4.929031e+1; 5.014346e+1; 5.103231e+1; 5.317448e+1; 5.344219e+1; 5.158250e+1; 5.171437e+1; 4.961624e+1; 4.876687e+1; 4.880023e+1; 5.107968e+1; 5.122694e+1; 5.140002e+1; 5.216388e+1; 5.201438e+1; 5.318082e+1; 5.165678e+1; 4.969001e+1; 4.825830e+1; 4.683513e+1; 4.813477e+1; 4.890378e+1; 4.930456e+1; 4.880930e+1; 5.062203e+1; 5.059127e+1; 4.977969e+1; 5.128514e+1; 5.142600e+1; 5.172041e+1; 5.111567e+1; 5.097814e+1; 4.950530e+1; 4.802299e+1; 4.873999e+1; 4.844677e+1; 4.861729e+1; 4.916113e+1; 5.007260e+1; 5.028835e+1; 4.901938e+1; 4.799205e+1; 4.498918e+1; 4.463004e+1; 4.207970e+1; 4.029530e+1; 4.010221e+1; 3.961220e+1; 4.183848e+1; 4.149531e+1; 4.274595e+1; 4.348334e+1; 4.269485e+1; 4.310773e+1; 4.236280e+1; 4.223861e+1; 4.206291e+1; 4.336835e+1; 4.419851e+1|]
    in
    let var88_obs  =
        [|2.491494e+1; 3.007992e+1; 3.007669e+1; 2.856822e+1; 2.941189e+1; 2.847084e+1; 2.362855e+1; 2.156412e+1; 2.526145e+1; 3.108174e+1; 2.610672e+1; 2.578896e+1; 2.490464e+1; 2.608521e+1; 2.788000e+1; 2.903804e+1; 3.228490e+1; 3.141710e+1; 3.323042e+1; 3.220916e+1; 3.040136e+1; 3.251317e+1; 3.473375e+1; 3.666994e+1; 3.750227e+1; 3.837593e+1; 3.690707e+1; 3.757577e+1; 4.927440e+1; 4.841706e+1; 4.881114e+1; 4.940368e+1; 4.806372e+1; 5.052389e+1; 4.968911e+1; 5.039904e+1; 5.195591e+1; 4.934759e+1; 5.049374e+1; 4.796931e+1; 4.988945e+1; 4.930795e+1; 5.028634e+1; 4.895474e+1; 5.068981e+1; 4.937191e+1; 5.230391e+1; 4.986405e+1; 5.009772e+1; 4.960055e+1; 4.869347e+1; 4.988183e+1; 5.066638e+1; 4.866574e+1; 5.069377e+1; 4.986629e+1; 1.719267e+1; 1.758280e+1; 2.012084e+1; 2.163585e+1; 2.125611e+1; 2.156582e+1; 2.047736e+1; 2.086848e+1; 1.590726e+1; 2.107300e+1; 2.182243e+1; 1.855395e+1; 1.698153e+1; 1.725030e+1; 2.183143e+1; 2.228202e+1; 2.188060e+1; 2.058381e+1; 2.081707e+1; 1.935463e+1; 2.092678e+1; 1.795724e+1; 1.657347e+1; 1.850760e+1; 2.241098e+1; 2.242153e+1; 2.017367e+1; 2.062946e+1; 2.317760e+1; 2.541719e+1; 2.712343e+1; 2.977505e+1; 2.969107e+1; 2.796534e+1; 2.488382e+1; 2.485925e+1; 2.771013e+1; 2.672862e+1; 2.943546e+1; 2.693998e+1; 9.999964e+4; 9.999970e+4; 1.000008e+5; 1.000014e+5|]
    in
    let var341__  =
        ()
    in
    let var342_bmres_warmup  =
        fun292_bm_runmultiple (var83_flight_range) (var88_obs) (var87_heightMap) (var86_heightMapSize) (var80_nPoints) (var85_altitude) (var84_velocity) (var79_sigma) (var82_xUpperBound) (var81_xLowerBound) (4)
    in
    let var343__  =
        ()
    in
    let var344_bmres_iters  =
        fun292_bm_runmultiple (var83_flight_range) (var88_obs) (var87_heightMap) (var86_heightMapSize) (var80_nPoints) (var85_altitude) (var84_velocity) (var79_sigma) (var82_xUpperBound) (var81_xLowerBound) (15)
    in
    let var407__  =
        let var345_median  =
            fun311_bm_median (var344_bmres_iters)
        in
        let var346_sum  =
            fun319_bm_sum (var344_bmres_iters)
        in
        let var347_avg  =
            ( /. ) (var346_sum) (1.50e+1)
        in
        let var348_max  =
            fun328_bm_max (var344_bmres_iters)
        in
        let var349_min  =
            fun337_bm_min (var344_bmres_iters)
        in
        let var350_variance  =
            fun328_bm_max ([|fun340_bm_dist (var347_avg) (var348_max); fun340_bm_dist (var347_avg) (var349_min)|])
        in
        let var351_median  =
            ( *. ) (var345_median) (1.0e+3)
        in
        let var352_sum  =
            ( *. ) (var346_sum) (1.0e+3)
        in
        let var353_avg  =
            ( *. ) (var347_avg) (1.0e+3)
        in
        let var354_max  =
            ( *. ) (var348_max) (1.0e+3)
        in
        let var355_min  =
            ( *. ) (var349_min) (1.0e+3)
        in
        let var356_variance  =
            ( *. ) (var350_variance) (1.0e+3)
        in
        let var357__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'='; '='; ' '; 'I'; 'T'; 'E'; 'R'; 'A'; 'T'; 'I'; 'O'; 'N'; ' '; 'R'; 'E'; 'S'; 'U'; 'L'; 'T'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var358__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'i'; 't'; 'e'; 'r'; 'a'; 't'; 'i'; 'o'; 'n'; 's'; ':'; ' '|])
        in
        let var359__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'1'; '5'|])
        in
        let var360__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var361__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var362__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var351_median))
        in
        let var363__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var364__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var365__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var354_max))
        in
        let var366__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var367__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var368__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var355_min))
        in
        let var369__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var370__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var371__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var353_avg))
        in
        let var372__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var373__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var374__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var356_variance))
        in
        let var375__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var376_median  =
            fun311_bm_median (var342_bmres_warmup)
        in
        let var377_sum  =
            fun319_bm_sum (var342_bmres_warmup)
        in
        let var378_avg  =
            ( /. ) (var377_sum) (4.0e+0)
        in
        let var379_max  =
            fun328_bm_max (var342_bmres_warmup)
        in
        let var380_min  =
            fun337_bm_min (var342_bmres_warmup)
        in
        let var381_variance  =
            fun328_bm_max ([|fun340_bm_dist (var378_avg) (var379_max); fun340_bm_dist (var378_avg) (var380_min)|])
        in
        let var382_median  =
            ( *. ) (var376_median) (1.0e+3)
        in
        let var383_sum  =
            ( *. ) (var377_sum) (1.0e+3)
        in
        let var384_avg  =
            ( *. ) (var378_avg) (1.0e+3)
        in
        let var385_max  =
            ( *. ) (var379_max) (1.0e+3)
        in
        let var386_min  =
            ( *. ) (var380_min) (1.0e+3)
        in
        let var387_variance  =
            ( *. ) (var381_variance) (1.0e+3)
        in
        let var388__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; '\n'; '='; '='; ' '; 'W'; 'A'; 'R'; 'M'; 'U'; 'P'; ' '; 'S'; 'T'; 'A'; 'T'; 'I'; 'S'; 'T'; 'I'; 'C'; 'S'; ' '; '='; '='; '\n'|])
        in
        let var389__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'N'; 'o'; '.'; ' '; 'o'; 'f'; ' '; 'w'; 'a'; 'r'; 'm'; 'u'; 'p'; ' '; 'r'; 'u'; 'n'; 's'; ':'; ' '|])
        in
        let var390__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'4'|])
        in
        let var391__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
        in
        let var392__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'M'; 'e'; 'd'; 'i'; 'a'; 'n'; ':'; ' '|])
        in
        let var393__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var382_median))
        in
        let var394__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var395__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'L'; 'o'; 'n'; 'g'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var396__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var385_max))
        in
        let var397__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var398__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'S'; 'h'; 'o'; 'r'; 't'; 'e'; 's'; 't'; ' '; 'r'; 'u'; 'n'; ':'; ' '|])
        in
        let var399__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var386_min))
        in
        let var400__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var401__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'A'; 'v'; 'e'; 'r'; 'a'; 'g'; 'e'; ':'; ' '|])
        in
        let var402__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var384_avg))
        in
        let var403__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        let var404__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'V'; 'a'; 'r'; 'i'; 'a'; 'n'; 'c'; 'e'; ':'; ' '; '+'; '-'|])
        in
        let var405__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (fun21_float2string (var387_variance))
        in
        let var406__  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; 'm'; 's'; '\n'|])
        in
        ()
    in
    ()