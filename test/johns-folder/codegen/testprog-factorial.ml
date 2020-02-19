

open Printf

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
    let rec factorial n =
            if ( = ) (n) (0) then
                1
            else
                ( * ) (n) (factorial (( - ) (n) (1)))
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
    ()


