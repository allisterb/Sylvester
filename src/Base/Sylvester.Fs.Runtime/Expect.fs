namespace Sylvester

open System
open ExpectNet

module Expect =

    let contains (s:Session) (q:string) = s.Expect.Contains(q)

    let isMatch (m:IResult) = m.IsMatch
