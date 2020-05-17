namespace Sylvester

open System
open System.Reflection
open FSharp.Reflection
open FSharp.Quotations

open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape
                
[<AutoOpen>]
module FsExpr =
    let rec range_type a = if FSharpType.IsFunction a then range_type(FSharpType.GetFunctionElements(a) |> snd) else a
    
    let sequal (l:Expr) (r:Expr) = 
        (l.ToString() = r.ToString()) 
        || l.ToString() = sprintf "(%s)" (r.ToString())
        || r.ToString() = sprintf "(%s)" (l.ToString())
    
    let sequal2 (l1:Expr) (l2:Expr) (r1:Expr) (r2:Expr) = sequal l1 r1 && sequal l2 r2
    
    let sequal3 (l1:Expr) (l2:Expr) (l3:Expr) (r1:Expr) (r2:Expr) (r3:Expr)= sequal l1 r1 && sequal l2 r2 && sequal l3 r3

    let vequal (lv1:Var) (lv2:Var) = lv1.Name = lv2.Name && lv1.Type = lv2.Type

    let vequal2 (lv1:Var) (lv2:Var) (rv1:Var) (rv2:Var) = vequal lv1 rv1 && vequal lv2 rv2
    
    let vequal3 (lv1:Var) (lv2:Var) (lv3:Var) (rv1:Var) (rv2:Var) (rv3:Var) = vequal lv1 rv1 && vequal lv2 rv2 && vequal lv3 rv3

    let src expr = decompile expr
        
    let rec body = 
        function
        | Lambda(_, Lambda(v2, b)) -> body (Expr.Lambda(v2, b))
        | Lambda(_, b) -> b
        | Let(_, _, b) -> b
        | expr -> expr

    let rec replace_var_expr (name:string) value expr  =
        match expr with
        | ShapeVar v  when v.Name = name ->  value
        | ShapeVar v -> Expr.Var v
        | ShapeLambda (v, body) -> Expr.Lambda (v, replace_var_expr name value body)
        | ShapeCombination (o, exprs) -> RebuildShapeCombination (o, List.map (replace_var_expr name value) exprs)

    let rec replace_var (var1:Var) (var2:Var) (expr:Expr) : Expr  =
        match expr with
        | ShapeVar v  when v.Name = var1.Name -> Expr.Var var2
        | ShapeVar v -> Expr.Var v
        | ShapeLambda (v, body) -> Expr.Lambda (v, replace_var var1 var2 body)
        | ShapeCombination (o, exprs) -> RebuildShapeCombination (o, List.map (replace_var var1 var2) exprs)

    let get_vars expr =
        let rec rget_vars prev expr =
            match expr with
            | ShapeVar v -> prev @ [v]
            | ShapeLambda (v, body) -> rget_vars (prev @ [v]) body
            | ShapeCombination (_, exprs) ->  List.map (rget_vars prev) exprs |> List.collect id
            
        rget_vars [] expr |> List.distinctBy (fun v -> v.Name)

    let get_var_names expr = get_vars expr |> List.map (fun v -> v.Name)
    
    let contains (var:Var) (expr:Expr) = get_var_names expr |> List.contains var.Name

    let traverse expr f =
        match expr with
        | ShapeVar v -> Expr.Var v
        | ShapeLambda (v, body) -> Expr.Lambda (v, f body)
        | ShapeCombination (o, exprs) -> RebuildShapeCombination (o,List.map f exprs)

    /// Based on: http://www.fssnip.net/bx/title/Expanding-quotations by Tomas Petricek.
    /// Expand variables and calls to methods and propery getters.
    let expand expr =
        let rec rexpand vars expr = 
          let expanded = 
            match expr with
            | WithValue(_, _, e) -> rexpand vars e
            | Coerce(e, t) -> rexpand vars e
            | ValueWithName(o, t, n) -> rexpand vars (Expr.Value(o, t))
            | Call(body, MethodWithReflectedDefinition meth, args) ->
                let this = match body with Some b -> Expr.Application(meth, b) | _ -> meth
                let res = Expr.Applications(this, [ for a in args -> [a]])
                rexpand vars res
            | PropertyGet(body, PropertyGetterWithReflectedDefinition p, []) -> 
                let this = match body with Some b -> b | None -> p
                rexpand vars this
            | PropertyGet(None, p, []) -> rexpand vars (Expr.Var(Var(p.Name, p.PropertyType)))
            // If the variable has an assignment, then replace it with the expression
            | ExprShape.ShapeVar v when Map.containsKey v vars -> vars.[v]    
            // Else apply rexpand recursively on all sub-expressions
            | _ -> traverse expr (rexpand vars)
          // After expanding, try reducing the expression - we can replace 'let' expressions and applications where the first argument is lambda.
          match expanded with
          | Application(ExprShape.ShapeLambda(v, body), assign)
          | Let(v, assign, body) ->
                rexpand (Map.add v (rexpand vars assign) vars) body
          | _ -> expanded

        rexpand Map.empty expr

    let expand_left = 
        function
        | Call(_,_,l::r::[]) when l.Type = r.Type -> expand l 
        | expr -> failwithf "%s is not a binary expression." (src expr)

    let expand_right = 
        function
        | Call(_,_,l::r::[]) when l.Type = r.Type -> expand r
        | expr -> failwithf "%s is not a binary expression." (src expr)

    let expandReflectedDefinitionParam = 
        function
        | WithValue(v, t, e) -> (v, t, expand e)
        | _ -> failwith "Expression is not a reflected definition parameter."

    let binary_call (so, m, l, r) =
        match so with
        | None -> Expr.Call(m, l::r::[])
        | Some o -> Expr.Call(o, m, l::r::[])

    let unary_call (so, m, l) =
        match so with
        | None -> Expr.Call(m, [l])
        | Some o -> Expr.Call(o, m, [l])
