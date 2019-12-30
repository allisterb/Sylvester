namespace Sylvester.Tensors

open System

open Sylvester
open Sylvester.Arithmetic
open Sylvester.Arithmetic.N10
open Sylvester.Collections

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

type IScalar = IFullShape<zero>

type IVector = IFullShape<one>

type IMatrix = IFullShape<two>

type ITensor3 = IFullShape<three>

type ITensor4 = IFullShape<four>

type ITensor5 = IFullShape<five>

type ITensor6 = IFullShape<six>

type ITensor7 = IFullShape<seven>

type ITensor8 = IFullShape<eight>

type ITensor9 = IFullShape<nine>

type ITensor10 = IFullShape<ten>