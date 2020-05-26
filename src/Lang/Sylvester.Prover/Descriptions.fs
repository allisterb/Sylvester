﻿namespace Sylvester

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
    let axiom_desc theoryName (patternDesc:PatternDescription)  = 
        AxiomDescription(theoryName, PatternDescription(patternDesc.Name, patternDesc.Description))

    let set_axiom_desc_theory theoryName (a:AxiomDescription)  = AxiomDescription(theoryName, PatternDescription(a.Name, a.Description))
    