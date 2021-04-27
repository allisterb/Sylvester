namespace Sylvester

open System
open System.Runtime.CompilerServices

open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns

open Microsoft.Z3

type Z3ModelResult =
| ConstResult of Expr
| FuncResult of FuncInterp

type Z3Solver(?logic:string) =
    let ctx = new Context()
    let solver = logic |> function | Some l -> ctx.MkSolver(l )| None -> ctx.MkSolver()    
    member val Ctx = ctx
    member val Solver = solver
    member val Tactics = ctx.TacticNames
    member x.Check(constraints: seq<BoolExpr>) = solver.Check constraints
    member x.Model() = solver.Model

module Z3 =
    /// Multiple indexers for evaluating formulas
    type Microsoft.Z3.Model with
      member x.Item (index: Expr) =
        x.Eval(index, true)
      member x.Item (index: FuncDecl) =
        // Taking care of array declaration
        if index.DomainSize = 0u && index.Range.SortKind <> Z3_sort_kind.Z3_ARRAY_SORT then 
            x.ConstInterp(index) |> ConstResult
        else 
            x.FuncInterp(index) |> FuncResult
    
    let internal create_numeral (solver:Z3Solver) (i:obj)  = 
        match i with
        | :? uint64  as n -> n |> solver.Ctx.MkInt :> ArithExpr
        | :? int64 as n -> n |> solver.Ctx.MkInt :> ArithExpr
        | :? uint32 as n -> n |> solver.Ctx.MkInt :> ArithExpr
        | :? int32 as n -> n |> solver.Ctx.MkInt  :> ArithExpr
        | :? Rational as n -> solver.Ctx.MkReal((int) n.Numerator, (int) n.Denominator) :> ArithExpr
        | :? real as n -> solver.Ctx.MkReal(n.ToString()) :> ArithExpr
        | _ -> failwithf "Cannot create numeral from value %A of type %A." i (i.GetType())

    let internal create_const (solver:Z3Solver) (s:string) (t:Type) = 
        match t.Name with
        | "UInt32 "
        | "Int32"
        | "UInt64"
        | "Int64" -> (s, s |> solver.Ctx.MkIntConst :> ArithExpr)
        | "Single"
        | "Double"
        | "Rational" -> (s, s |> solver.Ctx.MkRealConst :> ArithExpr) 
        | t -> failwithf "Cannot create constant of type %A." t

    let rec internal create_arith_expr (solver:Z3Solver) (expr:FSharp.Quotations.Expr) : ArithExpr =
        let vars = expr |> expand |> get_vars |> List.map(fun v -> create_const solver v.Name v.Type) |> Map.ofList 
        match expr with
        | Var v -> vars.[v.Name] 
        | ValueWithName(v,_,_) -> create_numeral solver v
        | UInt32 n -> create_numeral solver n
        | Int32 n -> create_numeral solver n
        | UInt64 n -> create_numeral solver n
        | Int64 n -> create_numeral solver n
        | Single n -> create_numeral solver n
        | Double n -> create_numeral solver n
        | Rational n -> create_numeral solver n
        | Call(None, Op "FromOne" , []) as e when e.Type = typeof<Rational> -> create_numeral solver 1Q
        | Call(None, Op "FromZero" , []) as e when e.Type = typeof<Rational> -> create_numeral solver 0Q
        | Call(None, Op "op_Addition" ,l::r::[]) -> solver.Ctx.MkAdd((create_arith_expr solver l), (create_arith_expr solver r))
        | Call(None, Op "op_Multiply" ,l::r::[]) -> solver.Ctx.MkMul((create_arith_expr solver l), (create_arith_expr solver r))
        | Call(None, Op "op_Subtraction" ,l::r::[]) -> solver.Ctx.MkSub((create_arith_expr solver l), (create_arith_expr solver r))
        | Call(None, Op "op_Division" ,l::r::[]) -> solver.Ctx.MkDiv((create_arith_expr solver l), (create_arith_expr solver r))
        | e when e.Type = typeof<bool>-> failwithf "The expression %A is a boolean expression." e
        | e  -> failwithf "The expression %A of type %A is not an arithmetic expression." e (e.Type)

    let rec internal create_bool_expr (solver:Z3Solver) (expr:FSharp.Quotations.Expr) : BoolExpr =
        let vars = 
            expr
            |> expand
            |> get_vars 
            |> List.choose(fun v -> if v.Type = typeof<bool> then Some v else None)
            |> List.map(fun v -> v.Name, solver.Ctx.MkBoolConst(v.Name)) |> Map.ofList
        match expr.Type.Name with
            | "Boolean"
            | "UInt32 "
            | "Int32"
            | "UInt64"
            | "Int64" 
            | "Single"
            | "Double"
            | "Rational" ->
                match expr with
                | Var v when v.Type = typeof<bool> -> vars.[v.Name]
                | ValueWithName(v, t, _) when t = typeof<bool> -> solver.Ctx.MkBool(v :?> bool)
                | Bool true -> solver.Ctx.MkTrue()
                | Bool false -> solver.Ctx.MkFalse()
                | Call(None, Op "op_Equality" ,l::r::[]) when l.Type = typeof<bool> -> solver.Ctx.MkEq(create_bool_expr solver l, create_bool_expr solver r)
                | Call(None, Op "op_Inequality" ,l::r::[]) when l.Type = typeof<bool> -> solver.Ctx.MkDistinct(create_bool_expr solver l, create_bool_expr solver r)
                | Call(None, Op "op_BarAmpBar" ,l::r::[]) -> solver.Ctx.MkAnd(create_bool_expr solver l, create_bool_expr solver r)
                | Call(None, Op "op_BitwiseOr" ,l::r::[]) -> solver.Ctx.MkOr(create_bool_expr solver l, create_bool_expr solver r)
                | Call(None, Op "Not" ,r::[]) -> solver.Ctx.MkNot(create_bool_expr solver r)
                | Call(None, Op "op_EqualsEqualsGreater" ,l::r::[]) -> solver.Ctx.MkImplies(create_bool_expr solver l, create_bool_expr solver r)
                
                | Call(None, Op "op_Equality" ,l::r::[]) -> solver.Ctx.MkEq(create_arith_expr solver l, create_arith_expr solver r)
                | Call(None, Op "op_Inequality" ,l::r::[]) -> solver.Ctx.MkDistinct(create_arith_expr solver l, create_arith_expr solver r)
                | Call(None, Op "op_LessThan" ,l::r::[]) -> solver.Ctx.MkLt(create_arith_expr solver l, create_arith_expr solver r)
                | Call(None, Op "op_LessThanOrEqual" ,l::r::[]) -> solver.Ctx.MkLe(create_arith_expr solver l, create_arith_expr solver r)
                | Call(None, Op "op_GreaterThan" ,l::r::[]) -> solver.Ctx.MkGt(create_arith_expr solver l, create_arith_expr solver r)
                | Call(None, Op "op_GreaterThanOrEqual" ,l::r::[]) -> solver.Ctx.MkGe(create_arith_expr solver l, create_arith_expr solver r)
                
                | _ -> failwithf "Cannot create Z3 expression fron expression %A of type %A." expr (expr.Type)
            
            | _ -> failwithf "Cannot create Z3 expression fron expression %A of type %A." expr (expr.Type)

    let internal check_sat_model (s:Z3Solver) (a: Expr<bool list>) = 
        let sol = a |> expand_list |> List.map (create_bool_expr s) |> s.Check 
        match sol with
        | Status.SATISFIABLE -> Some (s.Model())
        | _ -> None

    let internal get_var_model (m:Model) = 
        m.ConstDecls 
        |> Array.toList 
        |> List.map(fun c -> c.Name, match  m.[c] with | ConstResult c -> c  | _ -> failwith "This not a constant result.")
        
    let internal get_int_const : Expr->int =
        function
        | e when e.IsIntNum -> (e :?> IntNum).Int
        | e -> failwithf "The expression %A is not an integer constant." e

    let internal get_rat_const : Expr->Rational =
        function
        | e when e.IsRatNum -> let r = (e :?> RatNum) in Rational(r.BigIntNumerator, r.BigIntDenominator)
        | e -> failwithf "The expression %A is not a rational constant." e 
        
    let internal _get_int_var_model : Model-> (string *int) list = 
        get_var_model >> List.map(fun (l, r) -> l.ToString(), get_int_const r)

    let internal _get_rat_var_model : Model-> (string * Rational) list = 
        get_var_model >> List.map(fun (l, r) -> l.ToString(), get_rat_const r)

    //let create_sort (ctx:Context) (t:Type) =
    //    match t.Name with
    //    | "UInt32" -> ctx.MkIntSort() :> Sort
    //    | "Rational" -> ctx.MkRealSort() :> Sort
    let check_sat (s:Z3Solver) (a: Expr<bool list>) = 
        let sol = a |> expand_list |> List.map (create_bool_expr s) |> s.Check
        match sol with
        | Status.SATISFIABLE -> true
        | _ -> false

    let get_int_var_model (s:Z3Solver) (a: Expr<bool list>) = check_sat_model s a |> Option.map _get_int_var_model

    let get_rat_var_model (s:Z3Solver) (a: Expr<bool list>) = check_sat_model s a |> Option.map _get_rat_var_model

    [<assembly:InternalsVisibleTo("Sylvester.Tests.Solver.Z3")>]
    do()

 