open System
open System.Globalization

let emit (t: string) e = 
    //printfn "Emit %s %A" t e
    Some e

let fail (t: string) d =
    //printfn "Fail %s %A" t d
    None

module Ast =
    type var = string
    type Expr =
    | Number   of decimal
    | BinOp    of string * Expr * Expr
    | Operator of string * Expr * Expr
    | FunApply of var * Expr list
    | VarRef    of string
    with
        static member Sum (e1, e2) = BinOp ( "+", e1, e2)
        static member Diff (e1, e2) = BinOp ( "-", e1, e2)
        static member Prod (e1, e2) = BinOp ( "*", e1, e2)
        static member Ratio (e1, e2) = BinOp ("/", e1, e2)

module Language =
    open System
    open System.Text.RegularExpressions
    let private matchToken pattern s =
        Regex.Match(s, pattern |> sprintf "\A(%s)((?s).*)", 
           RegexOptions.Multiline)
        |> fun mtch ->
            if mtch.Success then
                (mtch.Groups.[1].Value, mtch.Groups.[2].Value) |> Some
            else
                None

    let (|WS|_|) = matchToken @"[ |\t|\n|\n\r]+"
    let (|COMMENT|_|) = matchToken @"#.*[\n|\r\n]"

    let (|WHITESPACE|_|) s =
        match s with
        | WS rest ->
            rest |> Some
        | COMMENT rest ->
            rest |> Some
        | _ ->
            None

    let rec (|Star|_|) f acc s =
        match f s with
        | Some (res, rest) ->
            (|Star|_|) f (res :: acc) rest
        | None ->
            (acc |> List.rev , s) |> Some

    let (|WhiteSpace|_|) s = (|Star|_|) (|WHITESPACE|_|) [] s

    let rec MatchTokenNoWS s pattern =
        match (|WhiteSpace|_|) s with
        | Some (_, rest) ->
            rest |> matchToken pattern
        | None ->
            s |> matchToken pattern

    let MatchToken s f pattern =
        pattern |> MatchTokenNoWS s |> Option.bind f

    let MatchSymbol s pattern =
        pattern |> MatchToken s (fun (_, rest) -> rest |> Some)

    let (|NUMBER|_|) s =
        let parseDouble s = Decimal.Parse(s, CultureInfo.InvariantCulture)
        "[0-9]+\.?[0-9]*" |> MatchToken s
            (fun (n, rest) -> (n |> parseDouble, rest) |> Some)
    let (|ID|_|) s =
        @"[a-zA-Z\.]+" |> MatchToken s (fun res -> res |> Some)
    let (|PLUS|_|)   s = @"\+" |> MatchSymbol s
    let (|MINUS|_|)  s = "-"  |> MatchSymbol s
    let (|MUL|_|)    s = @"\*" |> MatchSymbol s
    let (|DIV|_|)    s = "/"  |> MatchSymbol s
    let (|LPAREN|_|) s = @"\(" |> MatchSymbol s
    let (|RPAREN|_|) s = @"\)" |> MatchSymbol s
    let (|LISTSEP|_|) s = ";" |> MatchSymbol s
    let (|LSQBRACKET|_|) s = "\[" |> MatchSymbol s
    let (|RSQBRACKET|_|) s = "\]" |> MatchSymbol s
    let (|EQ|_|)     s = "=" |> MatchSymbol s
    let (|LT|_|)     s = "<" |> MatchSymbol s
    let (|GT|_|)     s = ">" |> MatchSymbol s
    let (|BOOLAND|_|) s = "&&" |> MatchSymbol s
    let (|BOOLOR|_|) s = "||" |> MatchSymbol s

    let rec (|Factor|_|) = function
    | NUMBER (n, rest) ->
        (Ast.Expr.Number n, rest) |> Some
    // FunApply
    | ID (f, LPAREN (ExprList (args, RPAREN rest))) ->
        (Ast.Expr.FunApply (f, args), rest) |> Some
    | LPAREN (Expression (e, RPAREN (rest))) ->
        (e, rest) |> Some
    | VarRef (name, rest) ->
        (Ast.Expr.VarRef name, rest) |> Some
    | _ ->
        None
        
    and (|Term|_|) = function
    | Factor (e1, MUL (Term (e2, rest))) ->
        (Ast.Expr.Prod (e1, e2), rest) |> Some
    | Factor (e1, DIV (Term (e2, rest))) ->
        (Ast.Expr.Ratio (e1, e2), rest) |> Some
    | Factor (e, rest) ->
        (e, rest) |> Some
    | _ ->
        None
    and (|Sum|_|) = function
    | Term (e1, PLUS (Sum (e2, rest))) ->
        (Ast.Expr.Sum (e1, e2), rest) |> Some
    | Term (e1, MINUS (Sum (e2, rest))) ->
        (Ast.Expr.Diff (e1, e2), rest) |> Some
    | Term (e, rest) ->
        (e, rest) |> Some
    | _ ->
        None


    and (|BoolExp|_|) input =
        match input with
        | Sum (e1, EQ ( Sum( e2, rest))) ->
            (Ast.Expr.Operator ("=", e1, e2), rest) |> Some
        | Sum (e1, LT ( Sum( e2, rest))) ->
            (Ast.Expr.Operator ("<", e1, e2), rest) |> Some
        | Sum (e1, GT ( Sum( e2, rest))) ->
            (Ast.Expr.Operator (">", e1, e2), rest) |> Some
        | Factor (e, rest) -> (e,rest) |> Some
        | _ -> None

    and (|BoolLogic|_|) input = 
        match input with
        | BoolExp (e1, BOOLAND ( BoolExp( e2, rest))) ->
            (Ast.Expr.Operator ("&&", e1, e2), rest) |> emit "AND" 
        | BoolExp (e1, BOOLOR ( BoolExp( e2, rest))) ->
            (Ast.Expr.Operator ("||", e1, e2), rest) |> Some

        | _ -> None

    and (|Expression|_|) = function 
    | BoolLogic (e, rest) -> (e,rest) |> emit "bool"
    | Sum (e,rest) -> (e, rest) |> emit "sum"
    | _ -> None
    
    and (|ManyArgs|_|) input = 
        match input with
        | Star (|ArgumentExpr|_|) [] (args, rest) -> 
            (args, rest) |> Some
        | _ -> 
            printfn "bad input %A" input
            None
   
    and (|ExprList|_|) input =
        match input with 
        | ManyArgs (args1, Expression (arg2, rest)) ->
            (args1 @ [arg2], rest) |> Some
        | _ -> None

    and (|ArgumentExpr|_|) = function
    | Term (e, LISTSEP (rest)) -> (e, rest) |> Some
    | _ -> None

    and (|VarRef|_|) = function
    | LSQBRACKET (ID (name, RSQBRACKET rest)) ->
        (name, rest) |> Some
    | _ -> None
        
    let (|Eof|_|) s =
        if s |> String.IsNullOrEmpty then
            () |> Some
        else
            match s with
            | WhiteSpace (_, rest) when rest |> String.IsNullOrEmpty ->
                () |> Some
            | _ ->
                None

[<EntryPoint>]
let main argv =

    match "1 = 1+1" with
    | Language.BoolExp (e, Language.Eof) ->
        printfn "%A" e

    match "[foo] && [bar]" with
    | Language.BoolLogic (e, Language.Eof) ->
        printfn "%A" e

    match "2 = 1+1 && 2 > somebool(12;22)" with
    | Language.BoolLogic (e, Language.Eof) ->
        printfn "%A" e

    match "1 < 2" with
    | Language.BoolExp (e, Language.Eof) ->
        printfn "%A" e

    match "[My.Variable]" with
    | Language.VarRef (e, Language.Eof) ->
        printfn "%A" e
    match "1;" with
    | Language.ArgumentExpr (e, Language.Eof) ->
        printfn "%A" e
    match "1;2;3;" with
    | Language.ManyArgs (e, Language.Eof) ->
        printfn "%A" e
    match "1;2;3" with
    | Language.ExprList (e, Language.Eof) ->
        printfn "%A" e
    match "1+(2+3) + 4 + fun.foo((2+4); 12; 22; [My.Variable])" with 
    | Language.Expression (e, Language.Eof) ->
        printf "%A" e
    | _ -> ()
    0


