namespace Sylvester

open System.Collections
open System.Collections.Generic
open System.Linq
open FSharp.Quotations
open FSharp.Quotations.Patterns

/// A statement that defines a set using a predicate for set membership.
type ISetBuilder<'t when 't: equality> =
    abstract member Pred:Predicate<'t>
    abstract member PredExpr:Expr
    
/// A statement that defines a set using a predicate for set membership.
and SetBuilder<'t when 't: equality>([<ReflectedDefinition(true)>] pred:Expr<Predicate<'t>>) = 
    let v,t,e = match pred with | WithValue(v, t, e) -> v,t,e | _ -> failwith "Unexpected expression."
    member val Pred = v :?> Predicate<'t>
    member val PredExpr = e
    interface ISetBuilder<'t> with
        member val Pred = v :?> Predicate<'t>
        member val PredExpr = e

/// A logical predicate.
and Predicate<'t when 't: equality> = 't -> bool

/// A set builder statement together with a generating function that defines a sequence. 
type SetGenerator<'t when 't: equality>([<ReflectedDefinition(true)>] pred:Expr<Predicate<'t>>, [<ReflectedDefinition(true)>] gen:Expr<GeneratingFunction<'t>>) = 
    let pv,pt,pe = match pred with | WithValue(v, t, e) -> v,t,e | _ -> failwith "Unexpected expression."
    let gv,gt,ge = match gen with | WithValue(v, t, e) -> v,t,e | _ -> failwith "Unexpected expression."
    let gen = gv :?> GeneratingFunction<'t>
    member val Pred = pv :?> Predicate<'t>
    member val PredExpr = pe
    member val Gen = gen
    member val GenExpr = ge
    member val Seq = Seq.initInfinite gen
    interface IEnumerable<'t> with
        member x.GetEnumerator () = x.Seq.GetEnumerator() 
    interface IEnumerable with
        member x.GetEnumerator () = (x :> IEnumerable<'t>).GetEnumerator () :> IEnumerator
    interface ISetBuilder<'t> with
        member val Pred = pv :?> Predicate<'t>
        member val PredExpr = pe
    
/// A sequence generating function.
and GeneratingFunction<'t when 't: equality> = int -> 't
    
type Build<'t when 't: equality> = SetBuilder<'t>
type Gen<'t when 't: equality> = SetGenerator<'t>

[<AutoOpen>]
module SetBuilder =
    let (|Generator|_|) x =
        match x:IEnumerable<'t> with
        | :? SetGenerator<'t> as g -> Some g
        | _ -> None