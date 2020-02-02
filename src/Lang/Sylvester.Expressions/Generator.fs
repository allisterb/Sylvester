namespace Sylvester

open System
open System.Collections.Generic
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape

type Generator<'e, 't when 'e :> IEnumerable<'t>>([<ReflectedDefinition(true)>] expr: Expr<'e>) = 
    member x.Expr = expand expr

[<AutoOpen>]
[<ReflectedDefinition>]
module Generator =
    let gen x = Generator x 