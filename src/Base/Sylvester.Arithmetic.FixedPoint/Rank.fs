namespace Sylvester.Arithmetic

type dim<'n when 'n :> Number> = 'n

type Rank<'dim1 when 'dim1 :> Number> = Rank of 'dim1
    with static member Shape = [|number<'dim1>.Val|]

type Rank<'dim1, 'dim2 when 'dim1 :> Number and 'dim2 :> Number> = Rank of ('dim1 * 'dim2)
    with static member Shape = [|number<'dim1>.Val; number<'dim2>.Val|]

type Rank<'dim1, 'dim2, 'dim3 when 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number> = Rank of ('dim1 * 'dim2 * 'dim3)
    with static member Shape = [|number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val|]

type Rank<'dim1, 'dim2, 'dim3, 'dim4 when 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 'dim4 :> Number> = 
    Rank of ('dim1 * 'dim2 * 'dim3 * 'dim4)
    with static member Shape = [|number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val; number<'dim4>.Val|]

type Rank<'dim1, 'dim2, 'dim3, 'dim4, 'dim5 when 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 'dim4 :> Number and 'dim5 :> Number> = 
    Rank of ('dim1 * 'dim2 * 'dim3 *'dim4 * 'dim5)
    with static member Shape = [|number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val; number<'dim4>.Val; number<'dim5>.Val|]

type Rank<'dim1, 'dim2, 'dim3, 'dim4, 'dim5, 'dim6 when 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 'dim4 :> Number and 'dim5 :> Number and 'dim6 :> Number> = 
    Rank of ('dim1 * 'dim2 * 'dim3 *'dim4 * 'dim5 * 'dim6)
    with static member Shape = [|number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val; number<'dim4>.Val; number<'dim5>.Val; number<'dim6>.Val|]

type Rank<'dim1, 'dim2, 'dim3, 'dim4, 'dim5, 'dim6, 'dim7 when 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 'dim4 :> Number and 'dim5 :> Number and 'dim6 :> Number and 'dim7 :> Number> = 
    Rank of ('dim1 * 'dim2 * 'dim3 *'dim4 * 'dim5 * 'dim6 * 'dim7)
    with static member Shape = [|number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val; number<'dim4>.Val; number<'dim5>.Val; number<'dim6>.Val; number<'dim7>.Val|]

type Rank<'dim1, 'dim2, 'dim3, 'dim4, 'dim5, 'dim6, 'dim7, 'dim8 when 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 'dim4 :> Number and 'dim5 :> Number and 'dim6 :> Number and 'dim7 :> Number and 'dim8 :> Number> = 
    Rank of ('dim1 * 'dim2 * 'dim3 *'dim4 * 'dim5 * 'dim6 * 'dim7 * 'dim8)
    with static member Shape = [|number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val; number<'dim4>.Val; number<'dim5>.Val; number<'dim6>.Val; number<'dim7>.Val; number<'dim8>.Val|]

type Rank<'dim1, 'dim2, 'dim3, 'dim4, 'dim5, 'dim6, 'dim7, 'dim8, 'dim9 when 'dim1 :> Number and 'dim2 :> Number and 'dim3 :> Number and 'dim4 :> Number and 'dim5 :> Number and 'dim6 :> Number and 'dim7 :> Number and 'dim8 :> Number and 'dim9 :> Number> = 
    Rank of ('dim1 * 'dim2 * 'dim3 *'dim4 * 'dim5 * 'dim6 * 'dim7 * 'dim8 * 'dim9)
    with static member Shape = [|number<'dim1>.Val; number<'dim2>.Val; number<'dim3>.Val; number<'dim4>.Val; number<'dim5>.Val; number<'dim6>.Val; number<'dim7>.Val; number<'dim8>.Val; number<'dim9>.Val|]
