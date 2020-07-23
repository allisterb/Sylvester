namespace Sylvester

open System
open System.Collections

open FSharp.Quotations
open FSharp.Quotations.Patterns
open Sylvester.Arithmetic
open Sylvester.Collections

type IReflexiveRelation = interface end

type ISymmetricRelation = interface end

type IAntiSymmetricRelation = interface end

type ITransitiveRelation = interface end

type IEquivalenceRelation =
    inherit IReflexiveRelation
    inherit ISymmetricRelation
    inherit ITransitiveRelation

type Relation<'a, 'b when 'a : equality and 'b : equality> = Set<'a * 'b>