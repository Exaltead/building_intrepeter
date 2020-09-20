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


[<Fact>]
let ``parenthesis test complexer`` () =
    let input = "7 + 3 * (10 / (12 / (3 + 1) - 1))"
    let tokens = lex input
    let res = expr tokens
    Assert.Equal(22, res)
