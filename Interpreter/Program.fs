open System
open Interpreter
open Lexer
open Parser

[<EntryPoint>]
let main argv =
    let testInput = "BEGIN
    BEGIN
        number := 2;
        a := number;
        b := 10 * a + 10 * number / 4;
        c := a - - b
    END;
    x := 11;
    END."
    let testToks = lex testInput
    let testAst = parseASTree testToks
    intrepet testAst

    while true do
        printf "repl >"
        let tokens = Console.ReadLine() |> lex
        let ast =  parseASTree tokens
        let res = intrepet ast
        printfn "%A" res
    0 // return an integer exit code