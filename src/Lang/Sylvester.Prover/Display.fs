namespace Sylvester

open System.Reflection
open FSharp.Quotations


module FormulaDisplay = 
    let get_unicode_for_func f =
        let info = (getFuncInfo FormulaModuleType f)
        let a = info.GetCustomAttributes( typeof<UnicodeAttribute>, true).[0]
        if a = null then "" else let u = (a :?> UnicodeAttribute) in u.Symbol
    
