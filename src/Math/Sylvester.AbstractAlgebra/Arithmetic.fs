namespace Sylvester

[<RequireQualifiedAccess>]
module internal Mod = 
    // Based on: https://paulor.land/writing/algebra-beyond-numbers/index.html#modular-arithmetic
    let modulo n x = (x % n + n) % n
    
    let (+) n x y = modulo n (x + y)
    let (-) n x y = modulo n (x - y)
    let (*) n x y = modulo n (x * y)


