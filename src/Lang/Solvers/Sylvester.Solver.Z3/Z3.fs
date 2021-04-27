namespace Sylvester

open System
open System.Reflection

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Microsoft.Z3

module Z3 =
    let create_ctx() = new Context()

    //let push (ctx:Context) = ctx.P
    let create_numeral<'t when 't : struct> (ctx:Context) (i:'t)  = 
        match box i with
        | :? uint64  as n -> n |> ctx.MkInt :> ArithExpr
        | :? int64 as n -> n |> ctx.MkInt :> ArithExpr
        | :? uint32 as n -> n |> ctx.MkInt :> ArithExpr
        | :? int32 as n -> n |> ctx.MkInt  :> ArithExpr
        | :? Rational as n -> ctx.MkReal((int) n.Numerator, (int) n.Denominator) :> ArithExpr
        | :? real as n -> ctx.MkReal(n.ToString()) :> ArithExpr
        | _ -> failwithf "Cannot create numeral from value %A of type %A." i typeof<'t>

    let create_const (ctx:Context) (s:string) (t:Type) = 
        match t.Name with
        | "UInt32 "
        | "Int32"
        | "UInt64"
        | "Int64" -> (s, s |> ctx.MkIntConst :> ArithExpr)
        | "Single"
        | "Double"
        | "Rational" -> (s, s |> ctx.MkRealConst :> ArithExpr) 
        | t -> failwithf "Cannot create constant of type %A." t

    let rec create_arith_expr (ctx:Context) (expr:FSharp.Quotations.Expr) : ArithExpr =
        let vars = expr |> get_vars |> List.map(fun v -> create_const ctx v.Name v.Type) |> Map.ofList 
        match expr with
        | Var v -> vars.[v.Name] 
        | UInt32 n -> create_numeral ctx n
        | Int32 n -> create_numeral ctx n
        | UInt64 n -> create_numeral ctx n
        | Int64 n -> create_numeral ctx n
        | Single n -> create_numeral ctx n
        | Double n -> create_numeral ctx n
        | Rational n -> create_numeral ctx n
        | Call(None, Op "op_Addition" ,l::r::[]) -> ctx.MkAdd((create_arith_expr ctx l), (create_arith_expr ctx r))
        | Call(None, Op "op_Multiply" ,l::r::[]) -> ctx.MkMul((create_arith_expr ctx l), (create_arith_expr ctx r))
        | Call(None, Op "op_Subtraction" ,l::r::[]) -> ctx.MkSub((create_arith_expr ctx l), (create_arith_expr ctx r))
        | Call(None, Op "op_Division" ,l::r::[]) -> ctx.MkDiv((create_arith_expr ctx l), (create_arith_expr ctx r))
        | e when e.Type = typeof<bool>-> failwithf "The expression %A is a boolean expression." e
        | e  -> failwithf "The expression %A of type %A is not an arithmetic expression." e (e.Type)

    let rec create_bool_expr (ctx:Context) (expr:FSharp.Quotations.Expr) : BoolExpr =
        let vars = 
            expr 
            |> get_vars 
            |> List.choose(fun v -> if v.Type = typeof<bool> then Some v else None)
            |> List.map(fun v -> v.Name, ctx.MkBoolConst(v.Name)) |> Map.ofList
        match expr.Type.Name with
            | "Bool"
            | "UInt32 "
            | "Int32"
            | "UInt64"
            | "Int64" 
            | "Single"
            | "Double"
            | "Rational" ->
                match expr with
                | Var v when v.Type = typeof<bool> -> vars.[v.Name]
                | Bool true -> ctx.MkTrue()
                | Bool false -> ctx.MkFalse()
                | Call(None, Op "op_Equality" ,l::r::[]) when expr.Type = typeof<bool> -> ctx.MkEq(create_bool_expr ctx l, create_bool_expr ctx r)
                | Call(None, Op "op_Inequality" ,l::r::[]) when expr.Type = typeof<bool> -> ctx.MkDistinct(create_bool_expr ctx l, create_bool_expr ctx r)
                | Call(None, Op "op_BarAmpBar" ,l::r::[]) -> ctx.MkAnd(create_bool_expr ctx l, create_bool_expr ctx r)
                | Call(None, Op "op_BitwiseOr" ,l::r::[]) -> ctx.MkOr(create_bool_expr ctx l, create_bool_expr ctx r)
                | Call(None, Op "Not" ,r::[]) -> ctx.MkNot(create_bool_expr ctx r)
                | Call(None, Op "op_EqualsEqualsGreater" ,l::r::[]) -> ctx.MkImplies(create_bool_expr ctx l, create_bool_expr ctx r)
                
                | Call(None, Op "op_Equality" ,l::r::[]) -> ctx.MkEq(create_arith_expr ctx l, create_arith_expr ctx r)
                | Call(None, Op "op_Inequality" ,l::r::[]) -> ctx.MkDistinct(create_arith_expr ctx l, create_arith_expr ctx r)
                | Call(None, Op "op_LessThan" ,l::r::[]) -> ctx.MkLt(create_arith_expr ctx l, create_arith_expr ctx r)
                | Call(None, Op "op_LessThanOrEqual" ,l::r::[]) -> ctx.MkLe(create_arith_expr ctx l, create_arith_expr ctx r)
                | Call(None, Op "op_GreaterThan" ,l::r::[]) -> ctx.MkGt(create_arith_expr ctx l, create_arith_expr ctx r)
                | Call(None, Op "op_GreaterThanOrEqual" ,l::r::[]) -> ctx.MkGe(create_arith_expr ctx l, create_arith_expr ctx r)
                | _ -> failwithf "Cannot create Z3 expression fron expression %A of type %A." expr (expr.Type)
            | _ -> failwithf "Cannot create Z3 expression fron expression %A of type %A." expr (expr.Type)