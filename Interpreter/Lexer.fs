module Lexer

open System

type Token =
    | Integer of value: int
    | Plus
    | Minus
    | Asterisk
    | Slash
    | Eof

let isEof =
    function
    | Eof -> false
    | _ -> true

let private parseInteger (input: char list) =
    Array.ofList (input)
    |> String.Concat
    |> int
    |> Integer

type Tokens(tokens: Token []) =
    do printfn "Tokens parsed: %A" tokens
    let mutable index = 0

    member this.Peak =
        if tokens.Length > index then tokens.[index] else Eof

    member this.Consume =
        if tokens.Length > index then
            this.Advance
            tokens.[index - 1]
        else
            Eof

    member this.Advance = index <- index + 1


let rec private tokenize input =
    match input with
    | [] -> [ Eof ]
    | ' ' :: tail -> tokenize tail
    | '+' :: tail -> Plus :: tokenize tail
    | '-' :: tail -> Minus :: tokenize tail
    | '*' :: tail -> Asterisk :: tokenize tail
    | '/' :: tail -> Slash :: tokenize tail
    | x :: tail when Char.IsDigit x ->
        let token =
            List.takeWhile Char.IsDigit (x :: tail)
            |> parseInteger

        let newInput = List.skipWhile Char.IsDigit tail
        token :: tokenize newInput
    | x :: _ -> failwith <| sprintf "Unmatching character %c" x

let lex (input: string) =
    Seq.toList input
    |> tokenize
    |> Array.ofList
    |> Tokens
