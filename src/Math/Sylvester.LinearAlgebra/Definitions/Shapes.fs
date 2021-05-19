namespace Sylvester

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
    
    type ``11`` = dim<11>

    type ``12`` = dim<12>

    type ``13`` = dim<13>

    type ``14`` = dim<14>

    type ``15`` = dim<15>

    type ``16`` = dim<16>

    type ``17`` = dim<17>

    type ``18`` = dim<18>

    type ``19`` = dim<19>

    type ``20`` = dim<20>

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

    let ``11`` = new ``11``()
    
    let ``12`` = new ``12``()
    
    let ``13`` = new ``13``()
    
    let ``14`` = new ``14``()
    
    let ``15`` = new ``15``()
    
    let ``16`` = new ``16``()
    
    let ``17`` = new ``17``()
    
    let ``18`` = new ``18``()
    
    let ``19`` = new ``19``()
    
    let ``20`` = new ``10``()

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
                                                l +== ``10`` <?>  (``10``, 
                                                    l +== ``11`` <?>  (``11``, 
                                                        l +== ``12`` <?>  (``12``, 
                                                            l +== ``13`` <?>  (``13``, 
                                                                l +== ``14`` <?>  (``14``, 
                                                                    l +== ``15`` <?>  (``15``, 
                                                                        l +== ``16`` <?>  (``16``, 
                                                                            l +== ``17`` <?>  (``17``, 
                                                                                l +== ``18`` <?>  (``18``, 
                                                                                    l +== ``19`` <?>  (``19``, 
                                                                                        l +== ``20`` <?>  (``20``, l)))))))))))))))))))))

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