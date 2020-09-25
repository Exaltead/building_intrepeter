module Lexer

open System

type Token =
    | Integer of value: int
    | Plus
    | Minus
    | Asterisk
    | Slash
    | Eof
    | OpeningParenthesis
    | ClosingParenthesis
    | Begin
    | End
    | Dot
    | Id of value: string
    | Assign
    | Semicolon

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

    member this.ConsumeUntil(token: Token) =
        let slice =
            Array.skip index tokens
            |> Array.takeWhile (fun a -> token = a)

        index <- index + slice.Length
        slice

let splitAt condition target = 
    let index = List.findIndex (condition >> not) target
    List.splitAt index target

let private parseInteger (input: char list) =
    Array.ofList (input)
    |> String.Concat
    |> int
    |> Integer

let private parseIdentifier (input: char list) =
    let str = Array.ofList(input) |> String.Concat
    match str with
    | "BEGIN" -> Begin
    | "END" -> End
    | _ -> Id str 


let rec private tokenize input =
    match input with
    | [] -> [ Eof ]
    | ' ' :: tail -> tokenize tail
    | '\n' :: tail -> tokenize tail
    | '\r' :: tail -> tokenize tail
    | '\t' :: tail -> tokenize tail
    | '+' :: tail -> Plus :: tokenize tail
    | '-' :: tail -> Minus :: tokenize tail
    | '*' :: tail -> Asterisk :: tokenize tail
    | '/' :: tail -> Slash :: tokenize tail
    | '(' :: tail -> OpeningParenthesis :: tokenize tail
    | ')' :: tail -> ClosingParenthesis :: tokenize tail
    | '.' :: tail -> Dot :: tokenize tail
    | ';' :: tail -> Semicolon :: tokenize tail
    | ':' :: '=' :: tail -> Assign :: tokenize tail 
    | x :: tail when Char.IsDigit x ->
        let (head, rest) = splitAt Char.IsDigit (x :: tail)
        parseInteger head :: tokenize rest
    | x :: tail when Char.IsLetter x -> 
        let (head, rest) = splitAt Char.IsLetter (x :: tail)
        parseIdentifier head :: tokenize rest
    | x :: _ -> failwith <| sprintf "Unmatching character %c" x

let lex (input: string) =
    Seq.toList input
    |> tokenize
    |> Array.ofList
    |> Tokens
