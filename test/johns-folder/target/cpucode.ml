open Printf
open Array

external gpuhost_saxpy_int_single: int -> int -> int array -> int array = "gpuhost_saxpy_int_single"
external gpuhost_id_ignore2nd: int array -> int array = "gpuhost_id_ignore2nd"
external gpuhost_factidx: int array -> int array = "gpuhost_factidx"

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
    let rec map f seq =
            if null (seq) then
                [||]
            else
                (fun x xs -> Array.append [|x|] xs) (f (head (seq))) (map (f) (tail (seq)))
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
            (fun x xs -> Array.append [|x|] xs) ('_') (int2string_rechelper (( ~- ) (n)))
        else
            int2string_rechelper (n)
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
    let saxpy_int_single x y a =
        ( + ) (( * ) (a) (x)) (y)
    in
    let saxpy_float_single x y a =
        ( +. ) (( *. ) (a) (x)) (y)
    in
    let saxpy_int_mapfull a y i x =
        ( + ) (( * ) (a) (x)) (Array.get (y) (i))
    in
    let id_ignore2nd x y =
        x
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
    let mapcuda_saxpy_int x y arr =
        gpuhost_saxpy_int_single (x) (y) (arr)
    in
    let mapcuda_id_ignore2nd arr =
        gpuhost_id_ignore2nd (arr)
    in
    let v  =
        10
    in
    let res  =
        factorial (v)
    in
    let printstr  =
        Array.append ([|'f'; 'a'; 'c'; 't'; 'o'; 'r'; 'i'; 'a'; 'l'; ' '|]) (Array.append (int2string (v)) (Array.append ([|' '; '='; ' '|]) (Array.append (int2string (res)) ([|'\n'|]))))
    in
    let _  =
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (printstr)
    in
    let res  =
        mapcuda_saxpy_int (17) (11) ([|15; 1|])
    in
    let _  =
        printintarr ([|'s'; 'a'; 'x'; 'p'; 'y'; ' '; '1'; '7'; ' '; '1'; '1'; ' '; '['; '1'; '5'; ','; ' '; '1'; ']'; ' '; 'r'; 'e'; 's'; 'u'; 'l'; 't'|]) (res)
    in
    let res  =
        mapcuda_id_ignore2nd (Array.make (70) (0))
    in
    let _  =
        printintarr ([|'m'; 'a'; 'p'; 'c'; 'u'; 'd'; 'a'; '_'; 'i'; 'd'; '_'; 'i'; 'g'; 'n'; 'o'; 'r'; 'e'; '2'; 'n'; 'd'; ' '; 'r'; 'e'; 's'; 'u'; 'l'; 't'|]) (res)
    in
    let factidx i ignored_arg =
        factorial (i)
    in
    let res  =
        gpuhost_factidx (Array.make (16) (0))
    in
    let _  =
        printintarr ([|'c'; 'u'; 'd'; 'a'; 'M'; 'a'; 'p'; 'i'; ' '; 'f'; 'a'; 'c'; 't'; 'i'; 'd'; 'x'; ' '; 'r'; 'e'; 's'; 'u'; 'l'; 't'|]) (res)
    in
    ()