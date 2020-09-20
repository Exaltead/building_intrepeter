open System
open Interpreter
open Lexer

[<EntryPoint>]
let main argv =
    while true do
        printf "repl >"
        let tokens = Console.ReadLine() |> lex
        let res =  expr tokens
        printfn "%A" res
    0 // return an integer exit code