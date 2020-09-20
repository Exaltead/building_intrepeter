module Interpreter

open Lexer

let factor (tokens: Tokens) =
    match tokens.Consume with
    | Integer x -> x
    | x -> failwith <| sprintf "Expected factor got %A" x

let term (tokens: Tokens) =
    let isOperator op =
        match op with
        | Asterisk
        | Slash -> true
        | _ -> false

    let mutable current = factor tokens

    while isOperator tokens.Peak do
        let op = tokens.Consume
        let right = factor tokens
        current <-
            match (op) with
            | Asterisk -> current * right
            | Slash -> current / right
            | x -> failwith <| sprintf "Expected mul/div got %A" x
    current

let expr tokens =
    let isOperator op =
        match op with
        | Plus
        | Minus -> true
        | _ -> false

    let mutable current = term tokens

    while isOperator tokens.Peak do
        let op = tokens.Consume
        let right = term tokens
        current <-
            match (op) with
            | Plus -> current + right
            | Minus -> current - right
            | x -> failwith <| sprintf "Expected plus/minus got %A" x
    current
