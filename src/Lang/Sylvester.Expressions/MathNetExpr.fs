namespace Sylvester

open System

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

open MathNet.Numerics
open MathNet.Symbolics

module MathNetExpr =
    
    let rec fromQuotation (q:Expr) : Expression =
        match q with
        | SpecificCall <@@ ( + ) @@> (_, _, [xt; yt]) -> (fromQuotation xt) + (fromQuotation yt)
        | SpecificCall <@@ ( - ) @@> (_, _, [xt; yt]) -> (fromQuotation xt) - (fromQuotation yt)
        | SpecificCall <@@ ( ~- ) @@> (_, _, [xt]) -> -(fromQuotation xt)
        | SpecificCall <@@ ( ~+ ) @@> (_, _, [xt]) -> +(fromQuotation xt)
        | SpecificCall <@@ ( * ) @@> (_, _, [xt; yt]) -> (fromQuotation xt) * (fromQuotation yt)
        | SpecificCall <@@ ( / ) @@> (_, _, [xt; yt]) -> (fromQuotation xt) / (fromQuotation yt)
        | SpecificCall <@@ ( ** ) @@> (_, _, [xt; yt]) -> (fromQuotation xt) ** (fromQuotation yt)
        | Int16 k -> Expression.FromInt32 (int k)
        | Int32 k -> Expression.FromInt32 k
        | Int64 k -> Expression.FromInt64 k
        | UInt16 k -> Expression.FromInt32 (int k)
        | UInt32 k -> Expression.FromInt64 (int64 k)
        | UInt64 k -> Expression.FromInteger (BigInteger k)
        | DerivedPatterns.Double d -> Expression.Real d
        | DerivedPatterns.Single d -> Expression.Real (float d)
        | Var x -> Identifier (Symbol x.Name)
        | PropertyGet (_, info, _) -> Identifier (Symbol info.Name)
        | Let (_, _, t) -> fromQuotation t
        | Lambda (_, t) -> fromQuotation t
        | _ -> failwith "not supported"

    let toQuotation (expr: Expression) (vars: Var list) (valueType: Type) =
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

        let a = 4N / 5N;
        convertExpr expr
        
