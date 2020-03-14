open Printf
open Array

external gpuhost_saxpy_int: int array -> float array -> int array -> int array = "gpuhost_saxpy_int"
external gpuhost_id2f_ignore2nd: int array -> float array -> int array -> float array = "gpuhost_id2f_ignore2nd"
external gpuhost_saxpy_float: int array -> float array -> float array -> float array = "gpuhost_saxpy_float"
external gpuhost_saxpy_intseq: int array -> float array -> int array -> int array -> int array = "gpuhost_saxpy_intseq"

let main =
    let head s =
        Array.get (s) (0)
    in
    let tail s =
        (fun xs start len -> Array.sub xs (min ((Array.length xs) - 1) start) (min ((Array.length xs) - start) len)) (s) (1) (Array.length (s))
    in
    let null l =
        ( = ) (Array.length (l)) (0)
    in
    let map f seq =
        Array.map (f) (seq)
    in
    let mapi f seq =
        Array.mapi (f) (seq)
    in
    let seqInit size f =
        Array.init (size) (f)
    in
    let int2string n =
        let rec int2string_rechelper n =
                if ( < ) (n) (10) then
                    [|char_of_int (( + ) (n) (int_of_char ('0')))|]
                else
                    let d  =
                        [|char_of_int (( + ) (( mod ) (n) (10)) (int_of_char ('0')))|]
                    in
                    Array.append (int2string_rechelper (( / ) (n) (10))) (d)
        in
        if ( < ) (n) (0) then
            (fun x xs -> Array.append [|x|] xs) ('-') (int2string_rechelper (( ~- ) (n)))
        else
            int2string_rechelper (n)
    in
    let float2string f =
        Array.of_seq (String.to_seq (string_of_float (f)))
    in
    let rec strJoin delim strs =
            if ( = ) (Array.length (strs)) (0) then
                [||]
            else
                if ( = ) (Array.length (strs)) (1) then
                    head (strs)
                else
                    Array.append (Array.append (head (strs)) (delim)) (strJoin (delim) (tail (strs)))
    in
    let printint i =
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (int2string (i))
    in
    let printintln i =
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (Array.append (int2string (i)) ([|'\n'|]))
    in
    let printintarr name arr =
        let rec printloop i =
                if ( = ) (i) (Array.length (arr)) then
                    ()
                else
                    let _  =
                        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; ' '; ' '; ' '|])
                    in
                    let _  =
                        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (int2string (i))
                    in
                    let _  =
                        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|':'; ' '|])
                    in
                    let _  =
                        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (int2string (Array.get (arr) (i)))
                    in
                    let _  =
                        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
                    in
                    printloop (( + ) (i) (1))
        in
        let _  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'C'; 'o'; 'n'; 't'; 'e'; 'n'; 't'; 's'; ' '; 'o'; 'f'; ' '|])
        in
        let _  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (name)
        in
        let _  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|':'; '\n'|])
        in
        printloop (0)
    in
    let printfloatarr name arr =
        let rec printloop i =
                if ( = ) (i) (Array.length (arr)) then
                    ()
                else
                    let _  =
                        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|' '; ' '; ' '; ' '|])
                    in
                    let _  =
                        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (int2string (i))
                    in
                    let _  =
                        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|':'; ' '|])
                    in
                    let _  =
                        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (float2string (Array.get (arr) (i)))
                    in
                    let _  =
                        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'|])
                    in
                    printloop (( + ) (i) (1))
        in
        let _  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'C'; 'o'; 'n'; 't'; 'e'; 'n'; 't'; 's'; ' '; 'o'; 'f'; ' '|])
        in
        let _  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (name)
        in
        let _  =
            (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|':'; '\n'|])
        in
        printloop (0)
    in
    let saxpy_int x y a =
        ( + ) (( * ) (a) (x)) (y)
    in
    let saxpy_float x y a =
        ( +. ) (( *. ) (a) (x)) (y)
    in
    let saxpy_intseq a y i x =
        ( + ) (( * ) (a) (x)) (Array.get (y) (i))
    in
    let id_ignore2nd x y =
        x
    in
    let id2f_ignore2nd x y =
        float_of_int (x)
    in
    let res  =
        gpuhost_saxpy_int [|32; 17; 11|] [||] ([|15; 1|])
    in
    let _  =
        printintarr ([|'s'; 'a'; 'x'; 'p'; 'y'; ' '; '1'; '7'; ' '; '1'; '1'; ' '; '['; '1'; '5'; ','; ' '; '1'; ']'; ' '; 'r'; 'e'; 's'; 'u'; 'l'; 't'|]) (res)
    in
    let res  =
        gpuhost_id2f_ignore2nd [|512|] [||] (Array.make (5000) (0))
    in
    let res  =
        gpuhost_saxpy_float [|512|] [|1.70e+1; 1.100000e+1|] (res)
    in
    let res  =
        gpuhost_saxpy_intseq [|32; 17|] [||] ([|9; 8|]) ([|15; 1|])
    in
    let _  =
        printintarr ([|'s'; 'a'; 'x'; 'p'; 'y'; '_'; 'i'; 'n'; 't'; 's'; 'e'; 'q'; ' '; '('; 'a'; ':'; ' '; '1'; '7'; ')'; ' '; '('; 'x'; ':'; ' '; '['; '1'; '5'; ','; ' '; '1'; ']'; ')'; ' '; '('; 'y'; ':'; ' '; '['; '9'; ','; ' '; '8'; ']'; ')'; ' '; 'r'; 'e'; 's'; 'u'; 'l'; 't'|]) (res)
    in
    ()