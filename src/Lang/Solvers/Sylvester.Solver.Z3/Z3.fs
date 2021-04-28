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
    let sp = ctx.MkParams();
    let op = ctx.MkParams()
    let solver = logic |> function | Some l -> ctx.MkSolver(l) | None -> ctx.MkSolver()
    do solver.Parameters <- sp
    let optimize = ctx.MkOptimize()
    do optimize.Parameters <- op 
    member val Ctx = ctx
    member val SolverParams = sp
    member val Solver = solver
    member val Optimize = optimize
    member val OptimizeParams = op

    member x.Check(constraints: seq<BoolExpr>) = solver.Check constraints
    member x.Model() = let m = solver.Model in if isNull m then failwith "No model exists." else m
    member x.OptModel() = let m = optimize.Model in if isNull m then failwith "No model exists." else m
    interface IDisposable with member x.Dispose() = solver.Dispose()
    static member val Tactics = 
        let ctx = new Context() in 
        let d = ctx.TacticNames |> Array.map(fun t -> t, ctx.TacticDescription t) |> Map.ofArray
        ctx.Dispose()
    static member val OptimizeParamsHelp = 
        let ctx = new Context()
        let opt = ctx.MkOptimize()
        let pd = opt.ParameterDescriptions.Names |> Array.map(fun d -> d.ToString(), opt.ParameterDescriptions.GetDocumentation d) |> Map.ofArray
        opt.Dispose()
        ctx.Dispose()
        pd

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
    
    let internal (|IntType|_|) (t:Type) =
        match t.Name with
        | "UInt32 "
        | "Int32"
        | "UInt64"
        | "Int64" -> Some ()
        | _ -> None

    let internal (|RatType|_|) (t:Type) =
        match t.Name with
        | "Single"
        | "Double"
        | "Rational" -> Some ()
        | _ -> None

    let internal (|BoolType|_|) (t:Type) =
        match t.Name with
        | "Boolean" -> Some()
        | _ -> None

    let internal (|SetType|_|) (t:Type) =
        if t.IsGenericType && t.Name.Contains "Set" && t.GenericTypeArguments.Length > 0 then Some () else None 

    let rec create_sort (solver:Z3Solver)  =
        function
        | IntType -> solver.Ctx.MkIntSort() :> Sort
        | RatType -> solver.Ctx.MkRealSort() :> Sort
        | SetType as t -> 
            let d = create_sort solver t.GenericTypeArguments.[0]
            solver.Ctx.MkArraySort(d, solver.Ctx.MkBoolSort()) :> Sort
        | t -> solver.Ctx.MkUninterpretedSort(t.Name) :> Sort 
        
    let internal create_numeral (solver:Z3Solver) (i:obj)  = 
        match i with
        | :? uint64  as n -> n |> solver.Ctx.MkInt :> ArithExpr
        | :? int64 as n -> n |> solver.Ctx.MkInt :> ArithExpr
        | :? uint32 as n -> n |> solver.Ctx.MkInt :> ArithExpr
        | :? int32 as n -> n |> solver.Ctx.MkInt  :> ArithExpr
        | :? Rational as n -> solver.Ctx.MkReal((int) n.Numerator, (int) n.Denominator) :> ArithExpr
        | :? real as n -> solver.Ctx.MkReal(n.ToString()) :> ArithExpr
        | _ -> failwithf "Cannot create numeral from value %A of type %A." i (i.GetType())

    let internal create_arith_const (solver:Z3Solver) (s:string) (t:Type) = 
        match t with
        | IntType -> (s, s |> solver.Ctx.MkIntConst :> ArithExpr)
        | RatType  -> (s, s |> solver.Ctx.MkRealConst :> ArithExpr) 
        | t -> failwithf "Cannot create arithmetic constant from type %A." t

    let internal create_set_const (solver:Z3Solver) (s:string) (t:Type) =
        let sort = create_sort solver t
        solver.Ctx.MkArrayConst(s, sort, solver.Ctx.MkBoolSort())

    let rec internal create_arith_expr (solver:Z3Solver) (expr:FSharp.Quotations.Expr) : ArithExpr =
        let vars = expr |> expand |> get_vars |> List.map(fun v -> create_arith_const solver v.Name v.Type) |> Map.ofList 
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

    let rec internal create_set_expr (solver:Z3Solver) (expr:FSharp.Quotations.Expr) : ArrayExpr =
        let get_array_var_or_fail (v:Var) = 
            match v.Type with
            | SetType -> create_set_const solver v.Name v.Type.GenericTypeArguments.[0]
            | _ -> failwithf "The variable %A is not a set type." v
        let vars = 
            expr 
            |> expand 
            |> get_vars 
            |> List.map (fun v -> v.Name, get_array_var_or_fail v)
            |> Map.ofList
        match expr with
            | Var v -> vars.[v.Name]
            | Coerce(e, t) when t.Name.StartsWith "ISet" -> create_set_expr solver e
            | NewUnionCase(uc, e) when uc.Name = "Empty" -> let s = create_sort solver expr.Type.GenericTypeArguments.[0] in solver.Ctx.MkEmptySet s
            | U when (src U = "U" || src U = "Set`1.U" || U.ToString() = "Value (Set {x|true:x})") && expr.Type.IsGenericType -> let s = create_sort solver expr.Type.GenericTypeArguments.[0] in solver.Ctx.MkFullSet s
            | Call(None, Op "op_BarPlusBar" ,l::r::[]) -> solver.Ctx.MkSetUnion((create_set_expr solver l), (create_set_expr solver r))
            | Call(None, Op "op_BarMultiplyBar",l::r::[]) -> solver.Ctx.MkSetIntersection(create_set_expr solver l, create_set_expr solver r)
            | _ -> failwithf "The expression %A is not a set expression." expr
    
    let rec internal create_bool_expr (solver:Z3Solver) (expr:FSharp.Quotations.Expr) : BoolExpr =
        let expr' = expand expr
        let vars = 
            expr'
            |> get_vars 
            |> List.choose(fun v -> if v.Type = typeof<bool> then Some v else None)
            |> List.map(fun v -> v.Name, solver.Ctx.MkBoolConst(v.Name)) |> Map.ofList
        match expr' with
        | Var v when v.Type = typeof<bool> -> vars.[v.Name]
        | ValueWithName(v, t, _) when t = typeof<bool> -> solver.Ctx.MkBool(v :?> bool)
        | Bool true -> solver.Ctx.MkTrue()
        | Bool false -> solver.Ctx.MkFalse()
        | Call(None, Op "op_Equality" ,l::r::[]) ->
            match l.Type, r.Type with
            | BoolType, BoolType -> solver.Ctx.MkEq(create_bool_expr solver l, create_bool_expr solver r)
            | SetType, SetType -> solver.Ctx.MkEq(create_set_expr solver l, create_set_expr solver r)
            | IntType, IntType
            | RatType, RatType -> solver.Ctx.MkEq(create_arith_expr solver l, create_arith_expr solver r)
            | _ -> failwith ""
        | Call(None, Op "op_Inequality" ,l::r::[])  -> 
            match l.Type, r.Type with
            | BoolType, BoolType -> solver.Ctx.MkDistinct(create_bool_expr solver l, create_bool_expr solver r)
            | SetType, SetType -> solver.Ctx.MkDistinct(create_set_expr solver l, create_set_expr solver r)
            | IntType, IntType
            | RatType, RatType -> solver.Ctx.MkDistinct(create_arith_expr solver l, create_arith_expr solver r)
            | _ -> failwith ""
        | Call(None, Op "op_BarAmpBar",l::r::[]) -> solver.Ctx.MkAnd(create_bool_expr solver l, create_bool_expr solver r)
        | Call(None, Op "op_BitwiseOr",l::r::[]) -> solver.Ctx.MkOr(create_bool_expr solver l, create_bool_expr solver r)
        | Call(None, Op "Not",r::[]) -> solver.Ctx.MkNot(create_bool_expr solver r)
        | Call(None, Op "op_EqualsEqualsGreater",l::r::[]) -> solver.Ctx.MkImplies(create_bool_expr solver l, create_bool_expr solver r)
        (* Arithmetic constraints *)
        | Call(None, Op "op_GreaterThan",l::r::[]) -> solver.Ctx.MkGt(create_arith_expr solver l, create_arith_expr solver r)
        | Call(None, Op "op_LessThan",l::r::[]) -> solver.Ctx.MkLt(create_arith_expr solver l, create_arith_expr solver r)
        | Call(None, Op "op_LessThanOrEqual",l::r::[]) -> solver.Ctx.MkLe(create_arith_expr solver l, create_arith_expr solver r)
        | Call(None, Op "op_GreaterThanOrEqual",l::r::[]) -> solver.Ctx.MkGe(create_arith_expr solver l, create_arith_expr solver r)
        (* Set constraints *)
        | Call(None, Op "op_BarQmarkBar",l::r::[]) -> solver.Ctx.MkSetMembership(create_set_expr solver l, create_set_expr solver r)
        | Call(None, Op "op_BarLessBar",l::r::[]) -> solver.Ctx.MkSetSubset(create_set_expr solver l, create_set_expr solver r)
        | _ -> failwithf "Cannot create Z3 constraint from %A." expr

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

    let set_solver_param (s:Z3Solver) (k:string) (v:string) = s.SolverParams.Add(s.Ctx.MkSymbol k, s.Ctx.MkSymbol v)

    let reset (s:Z3Solver) = s.Solver.Reset()
    
    let push(s:Z3Solver) = s.Solver.Push()

    let pop(s:Z3Solver) = s.Solver.Pop()

    let get_int_var_model (s:Z3Solver) (a: Expr<bool list>) = check_sat_model s a |> Option.map _get_int_var_model

    let get_rat_var_model (s:Z3Solver) (a: Expr<bool list>) = check_sat_model s a |> Option.map _get_rat_var_model

    let check_sat (s:Z3Solver) a = (Option.isSome <| check_sat_model s a)

    //let get_solver_int_var_model (s:Z3Solver)  |> Option.map _get_int_var_model
    let set_opt_param (s:Z3Solver) (k:string) (v:string) = s.OptimizeParams.Add(s.Ctx.MkSymbol k, s.Ctx.MkSymbol v)

    let assert_opt_hard (s:Z3Solver) (a:Expr<bool list >) = 
        let constraints = a |> expand_list |> List.map(create_bool_expr s) |> List.toArray in s.Optimize.Assert constraints

    let asset_opt_at_most (s:Z3Solver) (a:Expr<bool list >) k = 
        let constraints = a |> expand_list |> List.map(create_bool_expr s) |> List.toArray in s.Ctx.MkAtMost(constraints, k) |> s.Optimize.Assert

    let assert_opt_at_least (s:Z3Solver) (a:Expr<bool list >) k = 
        let constraints = a |> expand_list |> List.map(create_bool_expr s) |> List.toArray in s.Ctx.MkAtLeast(constraints, k) |> s.Optimize.Assert

    let opt_maximize (s:Z3Solver) (a:FSharp.Quotations.Expr) = 
        a |> create_arith_expr  s |> s.Optimize.MkMaximize

    let check_opt_sat (s:Z3Solver)  = 
        let sol = s.Optimize.Check()
        match sol with
        | Status.SATISFIABLE -> true
        | _ -> false

    let get_opt_int_var_model (s:Z3Solver) =
        match s.Solver.Check() with
        | Status.SATISFIABLE -> s.OptModel() |> _get_int_var_model |> Some
        | _ -> None

    [<assembly:InternalsVisibleTo("Sylvester.Tests.Solver.Z3")>]
    do()

 