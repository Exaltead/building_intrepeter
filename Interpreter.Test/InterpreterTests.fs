module Tests

open System
open Xunit
open Interpreter
open Lexer

[<Fact>]
let ``arithmetic test`` () =
    let input = "14 + 2 * 3 - 6 / 2"
    let tokens = lex input
    let res = expr tokens
    Assert.Equal(17, res)
