namespace Sylvester

open System

open Sylvester.Arithmetic

[<AutoOpen>]
module Dimension = 
    type zero = dim<0>
    
    type one = dim<1>
    
    type two = dim<2>
    
    type three = dim<3>
    
    type four = dim<4>
    
    type five = dim<5>
    
    type six = dim<6>
    
    type seven = dim<7>
    
    type eight = dim<8>
    
    type nine = dim<9>
    
    type ten = dim<10>
    
    let zero = new zero()
    
    let one = new one()
    
    let two = new two()
    
    let three = new three()
    
    let four = new four()
    
    let five = new five()
    
    let six = new six()
    
    let seven = new seven()
    
    let eight = new eight()
    
    let nine = new nine()
    
    let ten = new ten()

module rank = 
    type zero = Nat<0>

    type one = Nat<1>

    type two = Nat<2>

    type three = Nat<3>

    type four = Nat<4>

    type five = Nat<5>

    type six = Nat<6>

    type seven = Nat<7>

    type eight = Nat<8>

    type nine = Nat<9>

    type ten = Nat<10>

    let zero = new zero()

    let one = new one()

    let two = new two()

    let three = new three()

    let four = new four()

    let five = new five()

    let six = new six()

    let seven = new seven()

    let eight = new eight()

    let nine = new nine()

    let ten = new ten()

/// A linear algebra object whose rank and dimensions may be unknown until runtime
type IUnknownShape =
    abstract Rank:Option<int> with get,set
    abstract Dims:Option<int64[]> with get,set
    abstract Data: Array with get,set

/// A linear algebra object whose rank is known at compile time
type IPartialShape<'n when 'n :> Number> = 
    inherit IUnknownShape
    
/// A linear algebra object whose rank and dimensions are known at compile time    
type IFullShape<'n when 'n :> Number> = 
    inherit IPartialShape<'n>

type IScalar =
    inherit IFullShape<rank.zero>

type IVector<'n> = 
    inherit IFullShape<rank.one>
    abstract Dim0: 'n

type IMatrix<'r, 'c when 'r :> Number and 'c :> Number> = 
    inherit IFullShape<rank.two>