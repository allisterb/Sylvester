namespace Sylvester

type ClosedEconomy() =
    inherit EconomicModel()

    member val RepresentativeConsumer = new ConsumptionLeisure() with get, set
    member val RepresentativeFirm = new ProfitMaximization() with get, set
