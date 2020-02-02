namespace Sylvester

open System
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

[<AutoOpen>]
module FsExpr =
    /// Based on: http://www.fssnip.net/bx/title/Expanding-quotations by Tomas Petricek.
    /// Expand variables and calls to methods and propery getters.
    let expand expr =
        let rec expandv vars expr = 
          let expanded = 
            match expr with
            | WithValue(_, _, e) -> expandv vars e
            | Call(body, MethodWithReflectedDefinition meth, args) ->
                let this = match body with Some b -> Expr.Application(meth, b) | _ -> meth
                let res = Expr.Applications(this, [ for a in args -> [a]])
                expandv vars res
            | PropertyGet(body, PropertyGetterWithReflectedDefinition p, []) -> 
                let this = match body with Some b -> b | None -> p
                expandv vars this
            // If the variable has an assignment, then replace it with the expression
            | ExprShape.ShapeVar v when Map.containsKey v vars -> vars.[v]    
            // Else apply expandv recursively on all sub-expressions
            | ExprShape.ShapeVar v -> Expr.Var v        
            | ExprShape.ShapeLambda(v, expr) -> Expr.Lambda(v, expandv vars expr)
            | ExprShape.ShapeCombination(o, exprs) ->
                ExprShape.RebuildShapeCombination(o, List.map (expandv vars) exprs)

          // After expanding, try reducing the expression - we can replace 'let' expressions and applications where the first argument is lambda.
          match expanded with
          | Application(ExprShape.ShapeLambda(v, body), assign)
          | Let(v, assign, body) ->
                expandv (Map.add v (expandv vars assign) vars) body
          | _ -> expanded

        expandv Map.empty expr

    /// Get reflected definition for method info 
    let defForMethodInfo() =(*[omit:(...)]*)
        let cache = System.Collections.Generic.Dictionary<_,_>()
        fun mi -> 
            if cache.ContainsKey mi then cache.[mi]
            else
                let res = Expr.TryGetReflectedDefinition mi
                cache.Add (mi, res)
                res(*[/omit]*)
    
    /// Simplified SpecificCall
    let inline (|Func|_|) expr =(*[omit:(...)]*)
        match expr with
        | Lambdas(_,(Call(_,minfo1,_))) -> function
            | Call(obj, minfo2, args) when minfo1.MetadataToken = minfo2.MetadataToken ->
                Some args
            | _ -> None
        | _ -> failwith "invalid template parameter"(*[/omit]*)
    
    /// Info for the transformation steps
    type Info = {
        Scope: Map<string, Expr>
        Prior: int
        RightOperand: bool
    }
    
    /// Generate a formula pattern for an expression
    /// column number - index of var in the env; '#' is a temp placeholder for a row number
    let generatePattern expr env =     
        (*[omit:Defaults]*)
        let scope = 
            Seq.mapi (fun i name -> name, Expr.Value("R#C" + string (i + 1))) env 
            |> Map.ofSeq
    
        let defaultPrior = 4
        let tryGetReflectedDef = defForMethodInfo()
    
        // info with default priority
        let inline (!!) info = { info with Prior = defaultPrior }(*[/omit]*)
        
        // check if we need to add the parens
        let inline addParens (info: Info) currPrior = 
            info.Prior < currPrior || (info.Prior = currPrior && info.RightOperand)
    
        // let x = ...; let f x y = ...; f 1 x
        // inside f x is Value(1), y is outer x value 
        let updateScope names parms scope =
            Seq.zip names parms
            |> Seq.fold (fun sc -> function
                | name, Var var -> replace name sc.[var.Name] sc
                | name, param -> replace name param sc) scope
    
        // print binary ops: (x op y)
        let rec inline printBinaryOp op x y curr (info: Info) = 
            let left = transform { info with Prior = curr; RightOperand = false } x
            let right = transform { info with Prior = curr; RightOperand = true } y
            let res = left + op + right in if addParens info curr then "(" + res +  ")" else res
    
        // print functions: name(arg1, arg2, ...)
        and inline printFunc name args info  =
            let argValues: string[] = List.map (transform !!info) args |> List.toArray
            sprintf "%s(%s)" name (String.Join (", ", argValues))
    
        // apply function with given parameters
        and applyFunc (var: Var, parms) info =(*[omit:(...)]*)
            match Map.tryFind var.Name info.Scope with
            | Some (Lambdas (((x::_)::_) as vars, expr)) ->
                let newScope = 
                    let names = seq { for [v] in vars do yield v.Name }
                    updateScope names parms info.Scope
    
                transform { info with Scope = newScope } expr
            | _ -> failwith "cannot apply function"(*[/omit]*)
    
        // transform an expression into pattern
        and transform (info: Info) = function
            | Func <@@ (+) @@> [x; y] -> printBinaryOp "+" x y 3 info
            | Func <@@ (-) @@> [x; y] -> printBinaryOp "-" x y 3 info
            | Func <@@ (*) @@> [x; y] -> printBinaryOp "*" x y 2 info
            | Func <@@ (/) @@> [x; y] -> printBinaryOp "/" x y 2 info
            | Func <@@ ( ** ) @@> [x; y] -> printBinaryOp "^" x y 1 info 
            | Func <@@ (~-) @@> [x] -> "-" + transform { info with Prior = 0 } x
            | Func <@@ (~+) @@> [x] -> transform { info with Prior = 0 } x
            | Func <@@ decimal @@> [x] 
            | Func <@@ double @@> [x] 
            | Func <@@ float @@> [x] -> string (transform { info with Prior = 0 } x)
    
            | Func <@@ mduration @@> args -> printFunc "MDURATION" args info
            | Func <@@ accrint @@> args -> printFunc "ACCRINT" args info
            | Lambdas (_, e) -> transform !!info e
    
            // let a, b, ... = 1, 2, ...
            // Note: nested tuples and tuples as return values are not supported
            | Let (_, NewTuple vs, e) -> 
                let res, newScope = (*[omit:Update the scope]*)
                    let tupleItems = List.toArray vs
                    let rec initTuple (e, newScope) =   
                        match e with
                        | Let(v, TupleGet(_, ind), e') -> 
                            initTuple (e', replace v.Name tupleItems.[ind] newScope)
                        | _ -> e, newScope
                    initTuple (e, info.Scope)(*[/omit]*)
                transform { info with Scope = newScope } res
    
            | Let (var, value, e) -> 
                transform { info with Scope = replace var.Name value info.Scope } e
    
            | Value (v, _) -> string v
            // try to replace a varname with its column index
            | Var var -> 
                match Map.tryFind var.Name info.Scope with
                | Some replacement -> transform info replacement
                | _ -> var.Name
            // args.[i] means reference to the (i+1)th column
            | Call(None, mi, _::[Value (i, _)]) when mi.DeclaringType.Name = "IntrinsicFunctions" 
                                                  && mi.Name = "GetArray" -> 
                let ind = unbox i in "R#C" + string (ind + 1)
            
            // replace MakeDecimal with a value
            | Call(None, mi, Value (v, _)::_) when mi.DeclaringType.Name = "IntrinsicFunctions" 
                                                && mi.Name = "MakeDecimal"  -> 
                string v
            // try to inline a method call
            | Call(None, mi, ps) ->
                let names = mi.GetParameters() |> Array.map (fun p -> p.Name)
                let newScope = updateScope names ps info.Scope
                
                match tryGetReflectedDef mi with
                | Some impl ->
                    let rec getCall e =
                        match e with
                        | Lambda(_, Lambda (_, e)) -> getCall e // skip parameters
                        | call -> call
                    transform { info with Scope = newScope } (getCall impl)
                    
                | _ -> failwith (sprintf "Can't get reflected definition for %s" mi.Name)
            // DateTime ctor -> Excel DATE function: DATE(year, month, day)
            | NewObject(ci, Value(y,_)::Value(m,_)::Value(d,_)::_) 
                                                    when ci.DeclaringType.Name="DateTime"-> 
                sprintf "DATE(%A, %A, %A)" y m d    
    
            | Application ((Application args) as f, value) -> 
                (*[omit: collect params for the chain of function applications]*)
                let rec collectParams parms = function
                    | Application(Var f, v) -> f, v :: parms
                    | Application(applArgs, v) -> collectParams (v :: parms) applArgs
                    | expr -> failwith (sprintf "unexpected expression collecting params: %A" expr)(*[/omit]*)
                applyFunc (collectParams [value] f) info
    
            | Application (Var f, value) ->  applyFunc (f, [value]) info
    
            | LetRecursive _ -> failwith "Recursive functions are not supported"
            | expr -> failwith (sprintf "Unknown expression type: %A" expr)
    
        "=" + transform { Prior = defaultPrior; Scope = scope; RightOperand = false } expr

type FsExpr<'t>([<ReflectedDefinition(true)>] expr: Expr<'t>) = 
    member x.Expr = expand expr
  
    

    