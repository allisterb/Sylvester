namespace Sylvester

type Real = float

type R = Real

type R2 = R * R

type R3 = R * R * R

type R4 = R * R * R * R

module R =
    let reals = SetBuilder(fun (_:R) -> true) |> Set 
    let R = Field(AdditiveGroup(reals), MultiplicativeGroup(reals))
    let openInterval left right = R.Set.Subset(fun x -> x > left && x < right)
    let closedinterval left right = R.Set.Subset(fun x -> x >= left && x <= right)
    let line (origin:R) (step:R) = infiniteSeq (fun x -> x >= origin) (fun n -> origin + (((float) n) * step))
    let axis step = line 0.0 step

    