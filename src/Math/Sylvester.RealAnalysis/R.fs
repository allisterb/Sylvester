namespace Sylvester

type Real = float

module R =
    let reals = SetBuilder(fun (_:Real) -> true) |> Set 
    let R = Field(AdditiveGroup(reals), MultiplicativeGroup(reals))
    let openInterval left right = R.Set.Subset(fun x -> x > left && x < right)
    let closedinterval left right = R.Set.Subset(fun x -> x >= left && x <= right)
    let line (origin:float) (step:float) = infiniteSeq (fun x -> x >= origin) (fun n -> origin + (((float) n) * step))
    let axis step = line 0.0 step

