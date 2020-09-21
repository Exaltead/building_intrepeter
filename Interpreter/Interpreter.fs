module Interpreter
open Parser

let evaluateBinOp left op right eval =
    let leftVal = eval left
    let rightVal = eval right
    match op with
    | Addition -> leftVal + rightVal
    | Substraction -> leftVal - rightVal
    | Multiplication -> leftVal * rightVal
    | Division -> leftVal / rightVal

let evaluateNumber = id

let evaluateUnaryOp op value eval = 
    let evaluated = eval value
    match op with
    | Addition -> +evaluated
    | Substraction -> -evaluated
    | x -> failwith <| sprintf "Unsupported unary operation %A" x

let rec evaluate node = 
    match node with
    | Number(value, _) -> evaluateNumber value
    | BinOp(left, op, right, _) -> evaluateBinOp left op right evaluate
    | UnaryOp(op, value, _) -> evaluateUnaryOp op value evaluate


let intrepet (tree: ASTree) = evaluate tree