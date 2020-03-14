open Printf
open Array

external gpuhost_fib: int array -> float array -> int array = "gpuhost_fib"

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
    let id x =
        x
    in
    let rec factorial n =
            if ( = ) (n) (0) then
                1
            else
                ( * ) (n) (factorial (( - ) (n) (1)))
    in
    let rec fib_helper i n prev current =
            if ( = ) (i) (n) then
                current
            else
                fib_helper (( + ) (i) (1)) (n) (current) (( + ) (prev) (current))
    in
    let fib n =
        fib_helper (0) (n) (1) (0)
    in
    let res  =
        gpuhost_fib [|16; 48|] [||] 
    in
    let _  =
        printintarr ([|'c'; 'u'; 'd'; 'a'; 'M'; 'a'; 'p'; 'i'; 'd'; 'x'; ' '; '4'; '8'; ' '; 'f'; 'i'; 'b'; ' '; 'r'; 'e'; 's'; 'u'; 'l'; 't'|]) (res)
    in
    ()