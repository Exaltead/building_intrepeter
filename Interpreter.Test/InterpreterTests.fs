module Tests

open System
open Xunit
open Interpreter
open Lexer
open Parser

[<Fact>]
let ``arithmetic test`` () =
    let input = "14 + 2 * 3 - 6 / 2"
    let tokens = lex input
    let ast = parseASTree tokens
    let res = evaluateExpression ast
    Assert.Equal(17, res)


[<Fact>]
let ``parenthesis test`` () =
    let input = "7 + 3 * (10 / (12 / (3 + 1) - 1))"
    let tokens = lex input
    let ast = parseASTree tokens
    let res = evaluateExpression ast
    Assert.Equal(22, res)

[<Fact>]
let ``unary test`` () =
    let input = "5 - - - + - (3 + 4) - +2"
    let tokens = lex input
    let ast = parseASTree tokens
    let res = evaluateExpression ast
    Assert.Equal(10, res)
