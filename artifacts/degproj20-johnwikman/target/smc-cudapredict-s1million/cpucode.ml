open Printf

external gpuhost_fun114_initfun: int array -> float array -> float array -> float array = "gpuhost_fun114_initfun"
external gpuhost_fun131_initfun: int array -> float array -> float array -> float array = "gpuhost_fun131_initfun"
external gpuhost_fun239_wmapf: int array -> float array -> float array -> float array = "gpuhost_fun239_wmapf"
external gpuhost_fun234_wmapf: int array -> float array -> float array -> float array = "gpuhost_fun234_wmapf"
external gpuhost_fun229_wmapf: int array -> float array -> float array -> float array -> float array = "gpuhost_fun229_wmapf"
external gpuhost_fun217_propagate_x: int array -> float array -> float array -> float array -> float array = "gpuhost_fun217_propagate_x"
external gpuhost_fun169_rndinitf: int array -> float array -> float array = "gpuhost_fun169_rndinitf"
external gpuhost_fun178_wmapf: int array -> float array -> float array -> float array -> float array = "gpuhost_fun178_wmapf"
external gpuhost_fun183_wmapf: int array -> float array -> float array -> float array = "gpuhost_fun183_wmapf"
external gpuhost_fun188_wmapf: int array -> float array -> float array -> float array = "gpuhost_fun188_wmapf"

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
            if (( < ) (( * ) (var98_s_initsize) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (5) (( + ) (3) (( + ) (10) (( + ) (6) (( + ) (0) (( + ) (9) (if ( < ) (2) (( + ) (21) (( + ) (0) (( * ) (82) (if ( < ) (if ( < ) (1) (( + ) (( * ) (1) (4096)) (1)) then
                ( + ) (( * ) (1) (4096)) (1)
            else
                1) (if ( > ) (( + ) (( * ) (1) (4096)) (4096)) (var97_s_len) then
                var97_s_len
            else
                ( + ) (( * ) (1) (4096)) (4096)) then
                if ( > ) (( + ) (( * ) (1) (4096)) (4096)) (var97_s_len) then
                    var97_s_len
                else
                    ( + ) (( * ) (1) (4096)) (4096)
            else
                if ( < ) (1) (( + ) (( * ) (1) (4096)) (1)) then
                    ( + ) (( * ) (1) (4096)) (1)
                else
                    1)))) then
                ( + ) (21) (( + ) (0) (( * ) (82) (if ( < ) (if ( < ) (1) (( + ) (( * ) (1) (4096)) (1)) then
                    ( + ) (( * ) (1) (4096)) (1)
                else
                    1) (if ( > ) (( + ) (( * ) (1) (4096)) (4096)) (var97_s_len) then
                    var97_s_len
                else
                    ( + ) (( * ) (1) (4096)) (4096)) then
                    if ( > ) (( + ) (( * ) (1) (4096)) (4096)) (var97_s_len) then
                        var97_s_len
                    else
                        ( + ) (( * ) (1) (4096)) (4096)
                else
                    if ( < ) (1) (( + ) (( * ) (1) (4096)) (1)) then
                        ( + ) (( * ) (1) (4096)) (1)
                    else
                        1)))
            else
                2))))))))))) (( + ) (( + ) (( * ) (( + ) (var98_s_initsize) (Array.length (arg96_s))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (30) (( + ) (14) (( + ) (87) (( + ) (6) (( + ) (0) (( + ) (86) (if ( < ) (2) (( + ) (322) (( + ) (0) (( * ) (995) (if ( < ) (if ( < ) (1) (( + ) (( * ) (1) (4096)) (1)) then
                ( + ) (( * ) (1) (4096)) (1)
            else
                1) (if ( > ) (( + ) (( * ) (1) (4096)) (4096)) (var97_s_len) then
                var97_s_len
            else
                ( + ) (( * ) (1) (4096)) (4096)) then
                if ( > ) (( + ) (( * ) (1) (4096)) (4096)) (var97_s_len) then
                    var97_s_len
                else
                    ( + ) (( * ) (1) (4096)) (4096)
            else
                if ( < ) (1) (( + ) (( * ) (1) (4096)) (1)) then
                    ( + ) (( * ) (1) (4096)) (1)
                else
                    1)))) then
                ( + ) (322) (( + ) (0) (( * ) (995) (if ( < ) (if ( < ) (1) (( + ) (( * ) (1) (4096)) (1)) then
                    ( + ) (( * ) (1) (4096)) (1)
                else
                    1) (if ( > ) (( + ) (( * ) (1) (4096)) (4096)) (var97_s_len) then
                    var97_s_len
                else
                    ( + ) (( * ) (1) (4096)) (4096)) then
                    if ( > ) (( + ) (( * ) (1) (4096)) (4096)) (var97_s_len) then
                        var97_s_len
                    else
                        ( + ) (( * ) (1) (4096)) (4096)
                else
                    if ( < ) (1) (( + ) (( * ) (1) (4096)) (1)) then
                        ( + ) (( * ) (1) (4096)) (1)
                    else
                        1)))
            else
                2))))))))) (( / ) (( + ) (var98_s_initsize) (( - ) (1536) (1))) (1536))))) then (Array.init (var98_s_initsize) (fun114_initfun (arg96_s) (var97_s_len))) else (gpuhost_fun114_initfun [|1; var97_s_len; var98_s_initsize|] [||] (arg96_s))
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
            if (( < ) (( * ) (var119_s_initsize) (( + ) (50) (( + ) (6) (( + ) (0) (( + ) (5) (( + ) (3) (( + ) (10) (( + ) (8) (( + ) (0) (( * ) (65) (if ( < ) (if ( < ) (1) (( * ) (1) (4096)) then
                ( * ) (1) (4096)
            else
                1) (if ( > ) (( + ) (( * ) (1) (4096)) (4096)) (var118_s_len) then
                var118_s_len
            else
                ( + ) (( * ) (1) (4096)) (4096)) then
                if ( > ) (( + ) (( * ) (1) (4096)) (4096)) (var118_s_len) then
                    var118_s_len
                else
                    ( + ) (( * ) (1) (4096)) (4096)
            else
                if ( < ) (1) (( * ) (1) (4096)) then
                    ( * ) (1) (4096)
                else
                    1))))))))))) (( + ) (( + ) (( * ) (( + ) (var119_s_initsize) (Array.length (arg117_s))) (10)) (40000000)) (( * ) (( + ) (6) (( + ) (0) (( + ) (30) (( + ) (14) (( + ) (87) (( + ) (8) (( + ) (0) (( * ) (631) (if ( < ) (if ( < ) (1) (( * ) (1) (4096)) then
                ( * ) (1) (4096)
            else
                1) (if ( > ) (( + ) (( * ) (1) (4096)) (4096)) (var118_s_len) then
                var118_s_len
            else
                ( + ) (( * ) (1) (4096)) (4096)) then
                if ( > ) (( + ) (( * ) (1) (4096)) (4096)) (var118_s_len) then
                    var118_s_len
                else
                    ( + ) (( * ) (1) (4096)) (4096)
            else
                if ( < ) (1) (( * ) (1) (4096)) then
                    ( * ) (1) (4096)
                else
                    1))))))))) (( / ) (( + ) (var119_s_initsize) (( - ) (1536) (1))) (1536))))) then (Array.init (var119_s_initsize) (fun131_initfun (arg117_s) (var118_s_len))) else (gpuhost_fun131_initfun [|1; var118_s_len; var119_s_initsize|] [||] (arg117_s))
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
    let fun169_rndinitf arg168_xUpperBound arg167_xLowerBound arg166__ =
        (fun x y -> x +. (Random.float (Float.abs (x -. y)))) (arg167_xLowerBound) (arg168_xUpperBound)
    in
    let fun178_wmapf arg177_sigma arg176_heightMapSize arg175_heightMap arg174_altitude arg173_fstobs arg172_xelem =
        (fun x mu sigma -> -0.5 *. (x -. mu) *. (x -. mu) /. (sigma *. sigma) -. (Float.log (sigma *. Float.sqrt (2.0 *. 3.14159265359)))) (arg173_fstobs) (fun95_g_map (arg174_altitude) (arg175_heightMap) (arg176_heightMapSize) (arg172_xelem)) (arg177_sigma)
    in
    let fun183_wmapf arg182_wmax arg181_welem =
        Float.exp (( -. ) (arg181_welem) (arg182_wmax))
    in
    let fun188_wmapf arg187_wsum arg186_welem =
        ( /. ) (arg186_welem) (arg187_wsum)
    in
    let fun217_propagate_x arg216_sigma arg215_velocity arg213_x arg211_nPoints arg210_wacc arg208__ =
        let var209_p  =
            (fun x y -> x +. (Random.float (Float.abs (x -. y)))) (0.0) (1.0e-0)
        in
        let var212_i  =
            fun136_binsearch (arg210_wacc) (var209_p) (0) (( - ) (arg211_nPoints) (1))
        in
        let var214_x_new  =
            Array.get (arg213_x) (var212_i)
        in
        (fun mu sigma -> mu +. (sigma *. (Float.sqrt (-2.0 *. Float.log (Random.float 1.0))) *. (Float.cos (2.0 *. 3.14159265359 *. (Random.float 1.0))))) (( +. ) (var214_x_new) (arg215_velocity)) (arg216_sigma)
    in
    let fun229_wmapf arg228_log_1onN arg227_sigma arg226_heightMapSize arg225_heightMap arg224_altitude arg223_ithobs arg222_xelem =
        ( +. ) ((fun x mu sigma -> -0.5 *. (x -. mu) *. (x -. mu) /. (sigma *. sigma) -. (Float.log (sigma *. Float.sqrt (2.0 *. 3.14159265359)))) (arg223_ithobs) (fun95_g_map (arg224_altitude) (arg225_heightMap) (arg226_heightMapSize) (arg222_xelem)) (arg227_sigma)) (arg228_log_1onN)
    in
    let fun234_wmapf arg233_wmax arg232_welem =
        Float.exp (( -. ) (arg232_welem) (arg233_wmax))
    in
    let fun239_wmapf arg238_wsum arg237_welem =
        ( /. ) (arg237_welem) (arg238_wsum)
    in
    let rec fun190_iterate_smc arg220_obs arg205_heightMap arg203_heightMapSize arg201_nPoints arg199_altitude arg197_velocity arg195_sigma arg191_i arg192_steps arg193_x arg194_w =
            if ( = ) (arg191_i) (arg192_steps) then
                arg193_x
            else
                let var196_sigma  =
                    arg195_sigma
                in
                let var198_velocity  =
                    arg197_velocity
                in
                let var200_altitude  =
                    arg199_altitude
                in
                let var202_nPoints  =
                    arg201_nPoints
                in
                let var204_heightMapSize  =
                    arg203_heightMapSize
                in
                let var206_heightMap  =
                    arg205_heightMap
                in
                let var207_wacc  =
                    fun135_seqAccsumf (arg194_w)
                in
                let var218_x_propagated  =
                    if (( < ) (( * ) (var202_nPoints) (( + ) (50) (( + ) (12) (( + ) (0) (( + ) (22) (( + ) (37) (( + ) (10) (( + ) (0) (( * ) (82) (if ( < ) (if ( < ) (1) (( - ) (var202_nPoints) (1)) then
                        ( - ) (var202_nPoints) (1)
                    else
                        1) ((fun x -> int_of_float (Float.floor x)) ((fun x y -> x +. (Random.float (Float.abs (x -. y)))) (0.0) (1.0e-0))) then
                        (fun x -> int_of_float (Float.floor x)) ((fun x y -> x +. (Random.float (Float.abs (x -. y)))) (0.0) (1.0e-0))
                    else
                        if ( < ) (1) (( - ) (var202_nPoints) (1)) then
                            ( - ) (var202_nPoints) (1)
                        else
                            1)))))))))) (( + ) (( + ) (( * ) (( + ) (( + ) (var202_nPoints) (Array.length (arg193_x))) (Array.length (var207_wacc))) (10)) (40000000)) (( * ) (( + ) (12) (( + ) (0) (( + ) (102) (( + ) (427) (( + ) (21) (( + ) (0) (( * ) (784) (if ( < ) (if ( < ) (1) (( - ) (var202_nPoints) (1)) then
                        ( - ) (var202_nPoints) (1)
                    else
                        1) ((fun x -> int_of_float (Float.floor x)) ((fun x y -> x +. (Random.float (Float.abs (x -. y)))) (0.0) (1.0e-0))) then
                        (fun x -> int_of_float (Float.floor x)) ((fun x y -> x +. (Random.float (Float.abs (x -. y)))) (0.0) (1.0e-0))
                    else
                        if ( < ) (1) (( - ) (var202_nPoints) (1)) then
                            ( - ) (var202_nPoints) (1)
                        else
                            1)))))))) (( / ) (( + ) (var202_nPoints) (( - ) (1536) (1))) (1536))))) then (Array.init (var202_nPoints) (fun217_propagate_x (var196_sigma) (var198_velocity) (arg193_x) (var202_nPoints) (var207_wacc))) else (gpuhost_fun217_propagate_x [|1; var202_nPoints; var202_nPoints|] [|var196_sigma; var198_velocity|] (arg193_x) (var207_wacc))
                in
                let var219_log_1onN  =
                    Float.log (( /. ) (1.0e-0) (float_of_int (var202_nPoints)))
                in
                let var221_ithobs  =
                    Array.get (arg220_obs) (arg191_i)
                in
                let var230_w  =
                    if (( < ) (( * ) (Array.length (var218_x_propagated)) (( + ) (50) (152))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var218_x_propagated)) (Array.length (var206_heightMap))) (Array.length (var218_x_propagated))) (10)) (40000000)) (( * ) (1554) (( / ) (( + ) (Array.length (var218_x_propagated)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun229_wmapf (var219_log_1onN) (var196_sigma) (var204_heightMapSize) (var206_heightMap) (var200_altitude) (var221_ithobs)) (var218_x_propagated)) else (gpuhost_fun229_wmapf [|1; var204_heightMapSize|] [|var219_log_1onN; var196_sigma; var200_altitude; var221_ithobs|] (var206_heightMap) (var218_x_propagated))
                in
                let var231_wmax  =
                    fun116_seqMaxf (var230_w)
                in
                let var235_w  =
                    if (( < ) (( * ) (Array.length (var230_w)) (( + ) (50) (28))) (( + ) (( + ) (( * ) (( + ) (Array.length (var230_w)) (Array.length (var230_w))) (10)) (40000000)) (( * ) (228) (( / ) (( + ) (Array.length (var230_w)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun234_wmapf (var231_wmax)) (var230_w)) else (gpuhost_fun234_wmapf [|1|] [|var231_wmax|] (var230_w))
                in
                let var236_wsum  =
                    fun133_seqSumf (var235_w)
                in
                let var240_w_updated  =
                    if (( < ) (( * ) (Array.length (var235_w)) (( + ) (50) (11))) (( + ) (( + ) (( * ) (( + ) (Array.length (var235_w)) (Array.length (var235_w))) (10)) (40000000)) (( * ) (98) (( / ) (( + ) (Array.length (var235_w)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun239_wmapf (var236_wsum)) (var235_w)) else (gpuhost_fun239_wmapf [|1|] [|var236_wsum|] (var235_w))
                in
                fun190_iterate_smc (arg220_obs) (arg205_heightMap) (arg203_heightMapSize) (arg201_nPoints) (arg199_altitude) (arg197_velocity) (arg195_sigma) (( + ) (arg191_i) (1)) (arg192_steps) (var218_x_propagated) (var240_w_updated)
    in
    let var77_precision  =
        1000000
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
    let var170_x  =
        if (( < ) (( * ) (var80_nPoints) (( + ) (50) (28))) (( + ) (( + ) (( * ) (var80_nPoints) (10)) (40000000)) (( * ) (108) (( / ) (( + ) (var80_nPoints) (( - ) (1536) (1))) (1536))))) then (Array.init (var80_nPoints) (fun169_rndinitf (var82_xUpperBound) (var81_xLowerBound))) else (gpuhost_fun169_rndinitf [|1; var80_nPoints|] [|var82_xUpperBound; var81_xLowerBound|] )
    in
    let var171_fstobs  =
        Array.get (var88_obs) (0)
    in
    let var179_w  =
        if (( < ) (( * ) (Array.length (var170_x)) (( + ) (50) (147))) (( + ) (( + ) (( * ) (( + ) (( + ) (Array.length (var170_x)) (Array.length (var87_heightMap))) (Array.length (var170_x))) (10)) (40000000)) (( * ) (1529) (( / ) (( + ) (Array.length (var170_x)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun178_wmapf (var79_sigma) (var86_heightMapSize) (var87_heightMap) (var85_altitude) (var171_fstobs)) (var170_x)) else (gpuhost_fun178_wmapf [|1; var86_heightMapSize|] [|var79_sigma; var85_altitude; var171_fstobs|] (var87_heightMap) (var170_x))
    in
    let var180_wmax  =
        fun116_seqMaxf (var179_w)
    in
    let var184_w  =
        if (( < ) (( * ) (Array.length (var179_w)) (( + ) (50) (28))) (( + ) (( + ) (( * ) (( + ) (Array.length (var179_w)) (Array.length (var179_w))) (10)) (40000000)) (( * ) (228) (( / ) (( + ) (Array.length (var179_w)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun183_wmapf (var180_wmax)) (var179_w)) else (gpuhost_fun183_wmapf [|1|] [|var180_wmax|] (var179_w))
    in
    let var185_wsum  =
        fun133_seqSumf (var184_w)
    in
    let var189_w  =
        if (( < ) (( * ) (Array.length (var184_w)) (( + ) (50) (11))) (( + ) (( + ) (( * ) (( + ) (Array.length (var184_w)) (Array.length (var184_w))) (10)) (40000000)) (( * ) (98) (( / ) (( + ) (Array.length (var184_w)) (( - ) (1536) (1))) (1536))))) then (Array.map (fun188_wmapf (var185_wsum)) (var184_w)) else (gpuhost_fun188_wmapf [|1|] [|var185_wsum|] (var184_w))
    in
    let var241_x_res  =
        fun190_iterate_smc (var88_obs) (var87_heightMap) (var86_heightMapSize) (var80_nPoints) (var85_altitude) (var84_velocity) (var79_sigma) (1) (var83_flight_range) (var170_x) (var189_w)
    in
    ()