open Array
open Printf

external gpuhost_matrixMuliWorker: int array -> int array -> int array -> int -> int array = "gpuhost_matrixMuliWorker"

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
    let matrixMki rows cols v =
        Array.make (( * ) (rows) (cols)) (v)
    in
    let matrixGeti row col m_rows m_cols m =
        Array.get (m) (( + ) (( * ) (m_cols) (row)) (col))
    in
    let matrixIniti rows cols f =
        let seqInitFun i =
            let row  =
                ( / ) (i) (cols)
            in
            let col  =
                ( mod ) (i) (cols)
            in
            f (row) (col)
        in
        seqInit (( * ) (rows) (cols)) (seqInitFun)
    in
    let matrix2stri m_rows m_cols m =
        let rec printrc row col =
                if ( = ) (row) (m_rows) then
                    [||]
                else
                    let next_col  =
                        ( mod ) (( + ) (col) (1)) (m_cols)
                    in
                    let next_row  =
                        if ( = ) (next_col) (0) then
                            ( + ) (row) (1)
                        else
                            row
                    in
                    strJoin ([||]) ([|int2string (matrixGeti (row) (col) (m_rows) (m_cols) (m)); if ( = ) (next_col) (0) then
                        [|'\n'|]
                    else
                        [|' '|]; printrc (next_row) (next_col)|])
        in
        printrc (0) (0)
    in
    let printMatrixi m_rows m_cols m =
        let rec printrc row col =
                if ( = ) (row) (m_rows) then
                    [||]
                else
                    let next_col  =
                        ( mod ) (( + ) (col) (1)) (m_cols)
                    in
                    let next_row  =
                        if ( = ) (next_col) (0) then
                            ( + ) (row) (1)
                        else
                            row
                    in
                    let _  =
                        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) (strJoin ([||]) ([|int2string (matrixGeti (row) (col) (m_rows) (m_cols) (m)); if ( = ) (next_col) (0) then
                            [|'\n'|]
                        else
                            [|' '|]|]))
                    in
                    printrc (next_row) (next_col)
        in
        printrc (0) (0)
    in
    let rec matrixMuliWorkerReduce innerDim b_cols a b acc p a_offset b_offset =
            if ( = ) (p) (innerDim) then
                acc
            else
                matrixMuliWorkerReduce (innerDim) (b_cols) (a) (b) (( + ) (acc) (( * ) (Array.get (a) (a_offset)) (Array.get (b) (b_offset)))) (( + ) (p) (1)) (( + ) (a_offset) (1)) (( + ) (b_offset) (b_cols))
    in
    let matrixMuliWorker innerDim__a_rows__b_cols a b idx =
        matrixMuliWorkerReduce (Array.get (innerDim__a_rows__b_cols) (0)) (Array.get (innerDim__a_rows__b_cols) (2)) (a) (b) (0) (0) (( * ) (Array.get (innerDim__a_rows__b_cols) (0)) (( / ) (idx) (Array.get (innerDim__a_rows__b_cols) (2)))) (( mod ) (idx) (Array.get (innerDim__a_rows__b_cols) (2)))
    in
    let matrixMuli a_rows a_cols a b_rows b_cols b =
        if ( <> ) (a_cols) (b_rows) then
            (fun s -> printf "ERROR: %s
" (String.of_seq (Array.to_seq s)); exit 1) ([|'m'; 'a'; 't'; 'r'; 'i'; 'x'; 'M'; 'u'; 'l'; 'i'; ':'; ' '; 'I'; 'n'; 'n'; 'e'; 'r'; ' '; 'd'; 'i'; 'm'; 'e'; 'n'; 's'; 'i'; 'o'; 'n'; 's'; ' '; 'd'; 'i'; 'f'; 'f'; 'e'; 'r'; '.'|])
        else
            seqInit (( * ) (a_rows) (b_cols)) (matrixMuliWorker ([|a_cols; a_rows; b_cols|]) (a) (b))
    in
    let matrixMuliCUDA a_rows a_cols a b_rows b_cols b =
        let size  =
            ( * ) (a_rows) (b_cols)
        in
        if ( <> ) (a_cols) (b_rows) then
            (fun s -> printf "ERROR: %s
" (String.of_seq (Array.to_seq s)); exit 1) ([|'m'; 'a'; 't'; 'r'; 'i'; 'x'; 'M'; 'u'; 'l'; 'i'; 'C'; 'U'; 'D'; 'A'; ':'; ' '; 'I'; 'n'; 'n'; 'e'; 'r'; ' '; 'd'; 'i'; 'm'; 'e'; 'n'; 's'; 'i'; 'o'; 'n'; 's'; ' '; 'd'; 'i'; 'f'; 'f'; 'e'; 'r'; '.'|])
        else
            gpuhost_matrixMuliWorker ([|a_cols; a_rows; b_cols|]) (a) (b) (size)
    in
    let matAinitfun row col =
        ( + ) (( * ) (row) (row)) (col)
    in
    let matBinitfun row col =
        ( mod ) (( / ) (( * ) (( + ) (row) (19)) (17)) (( + ) (col) (13))) (( + ) (row) (11))
    in
    let matAinitfun_v2 row col =
        ( - ) (( mod ) (( + ) (( * ) (row) (row)) (col)) (3)) (1)
    in
    let matBinitfun_v2 row col =
        ( mod ) (( / ) (( * ) (( + ) (row) (19)) (17)) (( + ) (col) (13))) (2)
    in
    let matA_rows  =
        1024
    in
    let matA_cols  =
        1024
    in
    let matA  =
        matrixIniti (matA_rows) (matA_cols) (matAinitfun_v2)
    in
    let matB_rows  =
        1024
    in
    let matB_cols  =
        1024
    in
    let matB  =
        matrixIniti (matB_rows) (matB_cols) (matBinitfun_v2)
    in
    let matAxB  =
        matrixMuliCUDA (matA_rows) (matA_cols) (matA) (matB_rows) (matB_cols) (matB)
    in
    let matAxB_rows  =
        matA_rows
    in
    let matAxB_cols  =
        matB_cols
    in
    let _  =
        (fun s -> printf "%s" (String.of_seq (Array.to_seq s))) ([|'\n'; 'm'; 'a'; 't'; 'A'; 'x'; 'B'; ':'; '\n'|])
    in
    let _  =
        printMatrixi (matAxB_rows) (matAxB_cols) (matAxB)
    in
    ()