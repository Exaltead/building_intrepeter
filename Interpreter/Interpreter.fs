module Interpreter
open Parser

let mutable private symbolTable = System.Collections.Generic.Dictionary<string, int>()
//type EvaluationResult = Int of value: int | Empty

let evaluateBinOp left op right eval =
    let leftVal = eval left
    let rightVal = eval right
    match op with
    | Addition -> leftVal + rightVal
    | Substraction -> leftVal - rightVal
    | Multiplication -> leftVal * rightVal
    | Division -> leftVal / rightVal

let evaluateUnaryOp op value eval = 
    let evaluated = eval value
    match op with
    | Addition -> +evaluated
    | Substraction -> -evaluated
    | x -> failwith <| sprintf "Unsupported unary operation %A" x


let rec evaluateExpression node = 
    match node with
    | Number(value, _) -> value
    | BinOp(left, op, right, _) -> evaluateBinOp left op right evaluateExpression
    | UnaryOp(op, value, _) -> evaluateUnaryOp op value evaluateExpression
    | Variable(var, _) -> symbolTable.[var] // This is ok as not finding is alreary a failure^^... this is not really fine
    | _ -> failwith <| sprintf "Expected expression got %A" node


let evaluateAssign left right = 
    let var = 
        match left with
        | Variable(t, _) -> t
        | _ -> failwith <| sprintf "Expected variable got %A" left
    let value = evaluateExpression right
    symbolTable.[var] <- value

let rec evaluateStatement ast = 
    match ast with
    | NoOp -> ignore 0
    | Assign(left, right, _) -> evaluateAssign left right
    | Compound(nodes, _) -> Array.iter evaluateStatement nodes 
    | _ -> failwith <| sprintf "Expected statement got %A" ast



let intrepet (tree: ASTree) = 
    evaluateStatement tree
    for x in symbolTable do 
        printf "%A" x