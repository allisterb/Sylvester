namespace Sylvester.Tensors

open System

open Sylvester
open Sylvester.Arithmetic

module Rank = 
    type zero = n<0>

    type one = n<1>

    type two = n<2>

    type three = n<3>

    type four = n<4>

    type five = n<5>

    type six = n<6>

    type seven = n<7>

    type eight = n<8>

    type nine = n<9>

    type ten = n<10>

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

/// A tensor whose rank and dimensions may be unknown until runtime
type IUnknownShape =
    abstract member Rank:Option<int> with get,set
    abstract member Dims:Option<int64[]> with get,set

/// A tensor whose rank is known at compile time
type IPartialShape<'n when 'n :> Number> = 
    inherit IUnknownShape
    abstract member Rank:'n

/// A tensor whose rank and dimensions are known at compile time    
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