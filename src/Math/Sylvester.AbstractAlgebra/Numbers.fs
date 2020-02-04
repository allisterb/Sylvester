namespace Sylvester

[<AutoOpen>]
module Numbers = 
    /// Ring of 32-bit positive integers.
    let Zpos = IntegerRing(infiniteSeq id)

    /// Ring of 32-bit negative integers
    let Zneg = IntegerRing(infiniteSeq ((*) -1))

    /// Ring of integers
    let Z = IntegerRing(Zpos |+| Zneg)

    /// Rings of integers mod n

    let Z1 = CommutativeRing(Z, Mod.(+) 1, Mod.(*) 1, 0, 1, (~-))
    let Z2 = CommutativeRing(Z, Mod.(+) 2, Mod.(*) 2, 0, 1, (~-))
    let Z3 = CommutativeRing(Z, Mod.(+) 3, Mod.(*) 3, 0, 1, (~-))
    let Z4 = CommutativeRing(Z, Mod.(+) 4, Mod.(*) 4, 0, 1, (~-))
    let Z5 = CommutativeRing(Z, Mod.(+) 5, Mod.(*) 5, 0, 1, (~-))
    let Z6 = CommutativeRing(Z, Mod.(+) 6, Mod.(*) 6, 0, 1, (~-))
    let Z7 = CommutativeRing(Z, Mod.(+) 7, Mod.(*) 7, 0, 1, (~-))
    let Z8 = CommutativeRing(Z, Mod.(+) 8, Mod.(*) 8, 0, 1, (~-))
    let Z9 = CommutativeRing(Z, Mod.(+) 9, Mod.(*) 9, 0, 1, (~-))