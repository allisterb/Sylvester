#load "Include.fsx"

open Sylvester
open Sylvester.NLU.Wit

let wc = new WitClient()

wc.GetMeaning("p or not p")

