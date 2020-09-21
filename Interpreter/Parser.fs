module Parser

open Lexer

type Operation = Addition | Substraction | Multiplication | Division

let toOperation (token: Token): Operation = 
    match token with 
    | Plus -> Addition
    | Minus -> Substraction
    | Asterisk -> Multiplication
    | Slash -> Division
    | _ -> failwith <| sprintf "Unmatching operator %A" token

type ASTree = 
    | Number of value: int * token : Token
    | BinOp of left : ASTree * operation: Operation * right : ASTree * token : Token 
    // TODO: The re usage of the operation in here is bit questioable, but for now it is not an issue
    | UnaryOp of operation: Operation * value : ASTree * token: Token
    
let rec factor (tokens: Tokens) exprFunc =
    let tok = tokens.Consume
    match tok with
    | Integer x -> Number(x, tok)
    | OpeningParenthesis ->
        let res = exprFunc tokens
        if ClosingParenthesis = tokens.Consume then res else failwith "Expected closing parenthesis"
    | Plus -> UnaryOp(Addition, factor tokens exprFunc, tok)
    | Minus -> UnaryOp(Substraction, factor tokens exprFunc, tok)
    | x -> failwith <| sprintf "Expected factor got %A" x

let term (tokens: Tokens) exprFunc =
    let mutable current = factor tokens exprFunc

    while Slash = tokens.Peak || Asterisk = tokens.Peak do
        let op = tokens.Consume
        let right = factor tokens exprFunc
        current <- BinOp(current, (toOperation op), right, op) 
    current

let rec expr (tokens: Tokens) =
    let mutable current = term tokens expr

    while Plus = tokens.Peak || Minus = tokens.Peak do
        let op = tokens.Consume
        let right = term tokens expr
        current <- BinOp(current, (toOperation op), right, op) 
    current

let parseASTree = expr