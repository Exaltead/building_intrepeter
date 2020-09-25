module Parser

open Lexer

type Operation = Addition | Substraction | Multiplication | Division

let error (expected: Token) (actual: Token) = failwith <| sprintf "Expected %A got %A" expected actual 
let validateConsume (tokens:Tokens) (expected: Token) = 
    let tok = tokens.Consume
    if tok = expected then ignore 0 else error expected tok

let toOperation (token: Token): Operation = 
    match token with 
    | Plus -> Addition
    | Minus -> Substraction
    | Asterisk -> Multiplication
    | Slash -> Division
    | _ -> failwith <| sprintf "Unmatching operator %A" token

type ASTree = 
    | Number of value: int * token : Token
    | BinOp of left : ASTree * operation: Operation * right : ASTree * token : Token 
    // TODO: The re usage of the operation in here is bit questioable, but for now it is not an issue
    | UnaryOp of operation: Operation * value : ASTree * token: Token
    | Compound of children: ASTree [] * token: Token
    | Assign of left : ASTree * right : ASTree * token : Token
    | Variable of value: string * token : Token
    | NoOp

let empty (tokens:Tokens) = NoOp

let variable (tokens: Tokens) =
    let tok = tokens.Consume
    match tok with
    | Id value -> Variable(value, tok)
    | _ -> failwith <| sprintf "Expected variable got %A" tok

    
let rec factor (tokens: Tokens) exprFunc =
    let tok = tokens.Peak
    match tok with
    | Integer x -> 
        tokens.Consume |> ignore
        Number(x, tok)
    | OpeningParenthesis ->
        tokens.Consume |> ignore
        let res = exprFunc tokens
        if ClosingParenthesis = tokens.Consume then res else failwith "Expected closing parenthesis"
    | Plus -> 
        tokens.Consume |> ignore
        UnaryOp(Addition, factor tokens exprFunc, tok)
    | Minus -> 
        tokens.Consume |> ignore
        UnaryOp(Substraction, factor tokens exprFunc, tok)
    | Id _ -> variable tokens
    | x -> failwith <| sprintf "Expected factor got %A" x

let term (tokens: Tokens) exprFunc =
    let mutable current = factor tokens exprFunc

    while Slash = tokens.Peak || Asterisk = tokens.Peak do
        let op = tokens.Consume
        let right = factor tokens exprFunc
        current <- BinOp(current, (toOperation op), right, op) 
    current

let rec expr (tokens: Tokens) =
    let mutable current = term tokens expr

    while Plus = tokens.Peak || Minus = tokens.Peak do
        let op = tokens.Consume
        let right = term tokens expr
        current <- BinOp(current, (toOperation op), right, op) 
    current

// variable Assign expression
let assignStatement (tokens: Tokens) = 
    (*
        assignment_statement : variable ASSIGN expr
    *)
    let var = variable tokens
    match tokens.Consume with
    | Token.Assign -> 
        let right = expr tokens
        Assign(var, right, Token.Assign )
    | x -> failwith <| sprintf "Expected assignment operation got %A" x 


let rec statementList (tokens: Tokens) statementFunc = 
    (*
        statement_list : statement
               | statement SEMI statement_list
    *)
    let statementVal =  statementFunc tokens
    match tokens.Peak with
    | Semicolon -> 
        tokens.Consume |> ignore
        statementVal :: statementList tokens statementFunc
    | _ -> [statementVal]


let compoundStatement (tokens: Tokens) statementFunc = 
    (*
        compound_statement : BEGIN statement_list END
    *)
    let tok = tokens.Peak
    validateConsume tokens Begin
    let statements =  statementList tokens statementFunc
    validateConsume tokens End
    Compound(List.toArray statements, tok)

let rec statement (tokens: Tokens) = 
    (*
        statement : compound_statement
          | assignment_statement
          | empty
    *)
    let tok = tokens.Peak
    match tok with
    | Begin -> compoundStatement tokens statement
    | Id _ -> assignStatement tokens
    | _ -> empty tokens

let program (tokens: Tokens) =
    let statements = compoundStatement tokens statement
    if Dot = tokens.Consume && Eof = tokens.Consume
    then statements
    else failwith "Expected dot at end of file"

let parseASTree = program