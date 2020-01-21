namespace Sylvester.tf

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


module Rank = 
    type zero = N<0>

    type one = N<1>

    type two = N<2>

    type three = N<3>

    type four = N<4>

    type five = N<5>

    type six = N<6>

    type seven = N<7>

    type eight = N<8>

    type nine = N<9>

    type ten = N<10>

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

/// A TF tensor whose rank and dimensions may be unknown until runtime
type IUnknownShape =
    abstract member Rank:Option<int> with get,set
    abstract member Dims:Option<int64[]> with get,set

/// A TF tensor whose rank is known at compile time
type IPartialShape<'n when 'n :> Number> = 
    inherit IUnknownShape
    abstract member Rank:'n

/// A TF tensor whose rank and dimensions are known at compile time    
type IFullShape<'n when 'n :> Number> = 
    inherit IPartialShape<'n>

type IScalar = IFullShape<Rank.zero>

type IVector = IFullShape<Rank.one>

type IMatrix = IFullShape<Rank.two>

type ITensor3 = IFullShape<Rank.three>

type ITensor4 = IFullShape<Rank.four>

type ITensor5 = IFullShape<Rank.five>

type ITensor6 = IFullShape<Rank.six>

type ITensor7 = IFullShape<Rank.seven>

type ITensor8 = IFullShape<Rank.eight>

type ITensor9 = IFullShape<Rank.nine>

type ITensor10 = IFullShape<Rank.ten>