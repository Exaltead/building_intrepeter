module Interpreter

open Lexer

let factor (tokens: Tokens) exprFunc =
    match tokens.Consume with
    | Integer x -> x
    | OpeningParenthesis ->
        let res = exprFunc tokens
        if ClosingParenthesis = tokens.Consume then res else failwith "Expected closing parenthesis"
    | x -> failwith <| sprintf "Expected factor got %A" x

let term (tokens: Tokens) exprFunc =
    let isOperator op =
        match op with
        | Asterisk
        | Slash -> true
        | _ -> false

    let mutable current = factor tokens exprFunc

    while isOperator tokens.Peak do
        let op = tokens.Consume
        let right = factor tokens exprFunc
        current <-
            match (op) with
            | Asterisk -> current * right
            | Slash -> current / right
            | x -> failwith <| sprintf "Expected mul/div got %A" x
    current

let rec expr (tokens: Tokens) =
    let isOperator op =
        match op with
        | Plus
        | Minus -> true
        | _ -> false

    let mutable current = term tokens expr

    while isOperator tokens.Peak do
        let op = tokens.Consume
        let right = term tokens expr
        current <-
            match (op) with
            | Plus -> current + right
            | Minus -> current - right
            | x -> failwith <| sprintf "Expected plus/minus got %A" x
    current
