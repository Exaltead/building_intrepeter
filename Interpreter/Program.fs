open System
open Interpreter
open Lexer
open Parser

[<EntryPoint>]
let main argv =
    while true do
        printf "repl >"
        let tokens = Console.ReadLine() |> lex
        let ast =  parseASTree tokens
        let res = intrepet ast
        printfn "%A" res
    0 // return an integer exit code