namespace Sylvester

open System

open Sylvester
open Arithmetic

module Dimension = 
    type ``0`` = dim<0>
    
    type ``1`` = dim<1>
    
    type ``2`` = dim<2>
    
    type ``3`` = dim<3>
    
    type ``4`` = dim<4>
    
    type ``5`` = dim<5>
    
    type ``6`` = dim<6>
    
    type ``7`` = dim<7>
    
    type ``8`` = dim<8>
    
    type ``9`` = dim<9>
    
    type ``10`` = dim<10>
    
    let ``0`` = new ``0``()
    
    let ``1`` = new ``1``()
    
    let ``2`` = new ``2``()
    
    let ``3`` = new ``3``()
    
    let ``4`` = new ``4``()
    
    let ``5`` = new ``5``()
    
    let ``6`` = new ``6``()
    
    let ``7`` = new ``7``()
    
    let ``8`` = new ``8``()
    
    let ``9`` = new ``9``()
    
    let ``10`` = new ``10``()

    let inline pp l =
        l +== ``0`` <?>  (``0``,
            l +== ``1`` <?>  (``1``, 
                l +== ``2`` <?>  (``2``, 
                    l +== ``3`` <?>  (``3``,
                        l +== ``4`` <?> (``4``, 
                            l +== ``5`` <?>  (``5``, 
                                l +== ``6`` <?>  (``6``, 
                                    l +== ``7`` <?>  (``7``, 
                                        l +== ``8`` <?>  (``8``, 
                                            l +== ``9`` <?>  (``9``, 
                                                l +== ``10`` <?>  (``10``, l)))))))))))

/// A linear algebra object whose rank and dimensions may be unknown until runtime
type IUnknownShape =
    abstract Rank:Option<int> with get,set
    abstract Dims:Option<int64[]> with get,set
 
/// A linear algebra object whose rank is known at compile time
type IPartialShape<'n when 'n :> Number> = 
    inherit IUnknownShape
    
/// A linear algebra object whose rank and dimensions are known at compile time    
type IFullShape<'n when 'n :> Number> = 
    inherit IPartialShape<'n>

type IScalar =
    inherit IFullShape<dim<0>>

type IVector<'n> = 
    inherit IFullShape<dim<1>>
    abstract Dim0: 'n

type IMatrix<'r, 'c when 'r :> Number and 'c :> Number> = 
    inherit IFullShape<dim<2>>
    abstract Dim0:'r
    abstract Dim1:'c