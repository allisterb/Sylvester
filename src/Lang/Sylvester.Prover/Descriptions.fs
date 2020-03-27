namespace Sylvester

open System
open System.Reflection

/// Description attribute that can be applied to types or variables
type DescAttribute(name: string, ?desc: string ) =
    inherit System.Attribute()
    member val Name:string = name with get, set
    member val Description:string = defaultArg desc ""

module Descriptions =
    /// Text description of a formula pattern.
    type PatternDescription = PatternDescription of string * string with
        member x.Name = let (PatternDescription(n, d)) = x in n
        member x.Description = let (PatternDescription(n, d)) = x in d
           
    /// Create a pattern description from a name and an example formula.
    let pattern_desc name example  = PatternDescription(name, example |> body |> src)

    /// Text description of an axiom based on a formula pattern.
    type AxiomDescription = AxiomDescription of string * PatternDescription with
        member x.TheoryName = let (AxiomDescription(n, d)) = x in n
        member x.Name = let (AxiomDescription(n, d)) = x in d.Name
        member x.Description = let (AxiomDescription(n, d)) = x in d.Description
           
    /// Create an axiom name from a name and an example formula.
    let axiom_desc theoryName (printer:string->string) (patternDesc:PatternDescription)  = 
        AxiomDescription(theoryName, PatternDescription(patternDesc.Name, (printer patternDesc.Description)))

    let set_axiom_desc_theory (a:AxiomDescription) theoryName  = AxiomDescription(theoryName, PatternDescription(a.Name, a.Description))
    
    let get_desc<'t> = 
        let a = typeof<'t>.GetCustomAttributes( typeof<DescAttribute>, true)
        if (a = null || a.Length = 0) then ("","") else let d = (a.[0] :?> DescAttribute) in (d.Name, d.Description)