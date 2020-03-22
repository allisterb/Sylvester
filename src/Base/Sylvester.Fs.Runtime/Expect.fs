namespace Sylvester

open System
open ExpectNet

module Expect =

    let contains (s:Session) (q:string) (timeout:Nullable<int> option) (retries: Nullable<int> option) = 
        let t = defaultArg timeout (Nullable<int>())
        let r = defaultArg retries (Nullable<int>())
        s.Expect.Contains(q, t, r)

    let is_match (m:IResult)  = m.IsMatch
