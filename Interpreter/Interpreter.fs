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

let rec evaluate node = 
    match node with
    | Number(value, _) -> evaluateNumber value
    | BinOp(left, op, right, _) -> evaluateBinOp left op right evaluate


let intrepet (tree: ASTree) = evaluate tree