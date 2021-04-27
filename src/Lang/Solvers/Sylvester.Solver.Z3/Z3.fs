namespace Sylvester

open System
open System.Reflection

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns


open Microsoft.Z3

(*
type Z3Const =
| Z3Bool of string * BoolExpr
| Z3Real of string * RealExpr
| Z3Int of string * IntExpr
with 
    member x.Name =
        match x with
        | Z3Bool (n, _) -> n
        | Z3Real (n, _) -> n
        | Z3Int (n, _) -> n
    member x.Type =
        match x with
        | Z3Bool _ -> typeof<bool>
        | Z3Real _ -> typeof<real>
        | Z3Int _ -> typeof<int>
   
    member x.Bool = x |> function | Z3Bool (_,b) -> b | _ -> failwith "This is not a BoolExpr."

    member x.Real = x |> function | Z3Real (_,r) -> r | _ -> failwith "This is not a RealExpr."

    member x.Int = x |> function | Z3Int (_,i) -> i | _ -> failwith "This is not a IntExpr."
   
    member x.Match(n:string, t:Type) = x.Name = n && x.Type = t

type Z3Num =
| Z3IntNum of IntNum
| Z3RatNum of RatNum
    
type Z3Arith = Z3Arith of ArithExpr
*)

type Z3Expr =
| Z3Arith of ArithExpr
| Z3Bool of BoolExpr
| Z3Int of IntExpr

module Z3 =
    let create_numeral<'t when 't : struct> (ctx:Context) (i:'t)  = 
        match box i with
        | :? uint64  as n -> n |> ctx.MkInt :> ArithExpr
        | :? int64 as n -> n |> ctx.MkInt :> ArithExpr
        | :? uint32 as n -> n |> ctx.MkInt :> ArithExpr
        | :? int32 as n -> n |> ctx.MkInt  :> ArithExpr
        | :? Rational as n -> ctx.MkReal((int) n.Numerator, (int) n.Denominator) :> ArithExpr
        | :? real as n -> ctx.MkReal(n.ToString()) :> ArithExpr
        | _ -> failwithf "Cannot create numeral from value %A of type %A." i typeof<'t>

    let create_real_numeral(ctx:Context) (r:obj) =
        match r with
        | :? Rational as n -> ctx.MkReal((int) n.Numerator, (int) n.Denominator)
        | :? real as n -> ctx.MkReal(n.ToString())
        | _ -> failwithf "Cannot create real numeral from value %A." r

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

    let create_real_const (ctx:Context) (s:string) (t:Type) = 
        match t.Name with
        | "Single"
        | "Double"
        | "Rational" -> (s, s |> ctx.MkRealConst :> ArithExpr) 
        | t -> failwithf "Cannot create real constant of type %s." t

    let rec create_arith_expr ctx expr : ArithExpr =
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
                | Bool b -> ctx.MkBool b
                | Call(None, Op "op_Equality" ,l::r::[]) when expr.Type = typeof<bool> -> ctx.MkEq(create_bool_expr ctx l, create_bool_expr ctx r)
                | Call(None, Op "op_Inequality" ,l::r::[]) when expr.Type = typeof<bool> -> ctx.MkDistinct(create_bool_expr ctx l, create_bool_expr ctx r)
                | Call(None, Op "op_BarAmpBar" ,l::r::[]) -> ctx.MkAnd(create_bool_expr ctx l, create_bool_expr ctx r)
                | Call(None, Op "op_BitwiseOr" ,l::r::[]) -> ctx.MkOr(create_bool_expr ctx l, create_bool_expr ctx r)
                | Call(None, Op "Not" ,r::[]) -> ctx.MkNot(create_bool_expr ctx r)

                
                | Call(None, Op "op_Equality" ,l::r::[]) -> ctx.MkEq(create_arith_expr ctx l, create_arith_expr ctx r)
                | Call(None, Op "op_Inequality" ,l::r::[]) -> ctx.MkDistinct(create_arith_expr ctx l, create_arith_expr ctx r)
                | Call(None, Op "op_LessThan" ,l::r::[]) -> ctx.MkLt(create_arith_expr ctx l, create_arith_expr ctx r)
                | Call(None, Op "op_LessThanOrEqual" ,l::r::[]) -> ctx.MkLe(create_arith_expr ctx l, create_arith_expr ctx r)
                | Call(None, Op "op_GreaterThan" ,l::r::[]) -> ctx.MkGt(create_arith_expr ctx l, create_arith_expr ctx r)
                | Call(None, Op "op_GreaterThanOrEqual" ,l::r::[]) -> ctx.MkGe(create_arith_expr ctx l, create_arith_expr ctx r)
            | _ -> failwithf "Cannot create Z3 expression fron expression %A of type %A." expr (expr.Type)
    //let create_func (ctx:Context) = ctx.mkFu
//type Z3() = 
//    inherit Runtime()
//    let ctx = new Context()

//    member val Ctx = ctx


    //member x.MkInt() = ctx.MkInt()
//module Z3 =
//    let create_var (z3:Z3) (v:Var) =
//        match v.Type.Name with
//        | "Int32" -> z3.Ctx.Mk
