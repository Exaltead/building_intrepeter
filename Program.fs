// Learn more about F# at http://fsharp.org

open System
open Lexer


let termValue = function
    | (Integer value) -> value
    | x -> failwith <| sprintf "Expected term got %s" (string x)

let arithmeticOp left op right =
    match op with
    | Plus -> Integer ((termValue left) + (termValue right))
    | Minus -> Integer ((termValue left) - (termValue right))
    | Asterisk -> Integer ((termValue left) * (termValue right))
    | Slash -> Integer ((termValue left) / (termValue right))
    | _ -> failwith "Expeceted artihmetic operation"


let rec evaluateArithmeticExpression  current tokens =
    match tokens with
    | [] | [Eof]-> current
    | op::right::tail ->
        let res = arithmeticOp current op right
        evaluateArithmeticExpression res tail
    | _ -> failwith "Invalid syntax"

let evaluate input =
    let tokens = Seq.toList input |> tokenize
    match tokens with
    |(left::op::right::tail) ->
        let firstResult = arithmeticOp left op right
        evaluateArithmeticExpression firstResult tail
    |_ -> failwith "Not enough operands"


[<EntryPoint>]
let main argv =
    while true do
        printf "repl >"
        let res = Console.ReadLine() |> evaluate
        printfn "%A" res
    0 // return an integer exit code