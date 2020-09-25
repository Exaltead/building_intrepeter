open System
open System.IO
open Interpreter
open Lexer
open Parser


[<EntryPoint>]
let main argv =
    let testInput = File.ReadAllText("../pascal/assigment9_1.pas")
    let testToks = lex testInput
    let testAst = parseASTree testToks
    intrepet testAst
    0 // return an integer exit code