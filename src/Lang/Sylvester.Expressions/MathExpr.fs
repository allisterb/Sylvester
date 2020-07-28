namespace Sylvester

open System

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

open MathNet.Numerics
open MathNet.Symbolics


[<AutoOpen>]
module MathExpr =
    let rec toQuotation (expr: Expression) (vars: Var list) (valueType: Type) =
        let value = function
            | Value.Approximation a -> Expr.Value a.RealValue |> Some
            | Value.NegativeInfinity -> Expr.Value Double.NegativeInfinity |> Some
            | Value.PositiveInfinity -> Expr.Value System.Double.PositiveInfinity |> Some
            | Value.Number n -> n |> float |> Expr.Value |> Some
            | _ -> None
        let constant = function
            | E -> Expr.Value Constants.E  |> Some 
            | Pi -> Expr.Value Constants.Pi |> Some
            | _ -> None
        let mathType = typeof<System.Math>
        let argName = function |Symbol(n) -> n
        let getParam p =
            List.fold(
                    fun x (y : Var) ->
                        match x with
                        | None when y.Name = (argName p) -> Some y
                        | Some(v) -> Some v
                        | _ -> None
                    ) None vars
        let mathCall1 (name : string) (a : Expr) = Expr.Call(mathType.GetMethod(name, [|valueType|]), [a]) 
        let mathCall2 (name : string) (a : Expr) (b : Expr) = Expr.Call(mathType.GetMethod(name, [|valueType; valueType|]), [a; b]) 
        let add a b = <@@ (%%a:float) + (%%b:float) @@>
                
        let rec convertExpr : Expression -> Expr option = 
            function
            | Identifier(sym) -> Option.map (fun x -> Expr.Var(x)) (getParam sym)
            | Values.Value v -> value v
            | Constant c -> constant c
            | Sum(xs) ->
                let summands = List.map convertExpr xs
                List.fold (Option.map2 add) (value Value.zero) summands
            | _ -> None

        convertExpr expr
        
