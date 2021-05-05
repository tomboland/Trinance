#if INTERACTIVE
#I __SOURCE_DIRECTORY__
#r "nuget: Http.Fs"
#r "nuget: Hopac"
#r "nuget: Fsharp.Json"
#endif
open BinanceApi

let apiContext = {
    ApiKey = BinanceApiKey "" ; 
    ApiSecret = BinanceApiSecret "" ; 
    ApiRoot = BinanceApiRoot "https://api.binance.com" ; 
}

let TradeAmount = 1M

type SymbolPairExchangeRate = {
    basis: Asset
    quote: Asset
    exchangeRate: ExchangeRate
}

let getTradingSymbols (symbols: ExchangeInfoSymbolDTO list): Map<Symbol, SymbolPair> =
    symbols
    |> List.map(
        fun symbol ->
        (
            Symbol symbol.symbol,
            { basis = Asset symbol.baseAsset; quote = Asset symbol.quoteAsset }
        )
    ) |> Map.ofList

let getSymbolPairExchangeRates (symbolPairs: Map<Symbol, SymbolPair>) (tickers: TickerPricesDTO): SymbolPairExchangeRate list =
    tickers
    |> List.map (
        fun ticker ->
        (
            let symbol = Symbol ticker.symbol
            let pair = symbolPairs.[symbol]
            { basis = pair.basis; quote = pair.quote; exchangeRate = ExchangeRate (decimal ticker.price) }
        )
    )

let normalisedExchangeRateFromOriginAsset (asset: Asset) (symbolPairExchangeRates: SymbolPairExchangeRate list) =
    symbolPairExchangeRates
    |> List.choose(
        function
        | pair when pair.basis = asset -> Some (pair.quote, pair.exchangeRate)
        | pair when pair.quote = asset ->
            let (ExchangeRate exchangeRate) = pair.exchangeRate
            Some (pair.basis, ExchangeRate (1.0M / exchangeRate ))
        | _ -> None
    )

let normalisedExchangeRateToExitAsset (asset: Asset) (symbolPairExchangeRates: SymbolPairExchangeRate list) =
    symbolPairExchangeRates
    |> List.choose(
        function
        | pair when pair.quote = asset -> Some (pair.basis, pair.exchangeRate)
        | pair when pair.basis = asset ->
            let (ExchangeRate exchangeRate) = pair.exchangeRate
            Some (pair.quote, ExchangeRate (1.0M / exchangeRate ))
        | _ -> None
    )

let matchAssetsThroughIntermediate (origin: Asset) (exit: Asset) (symbolPairExchangeRates: SymbolPairExchangeRate list) =
    []

[<EntryPoint>]
let main argv =
    let exchangeInfo = 
        exchangeInfoRequest apiContext
        |> function
        | Success result -> result
        | Failure err -> failwith $"{err}"
    let tickers =
        tickerRequest apiContext
        |> function
        | Success result -> result
        | Failure err -> failwith $"{err}"

    let tradingSymbols = getTradingSymbols exchangeInfo.symbols
    let symbolPairExchangeRates = getSymbolPairExchangeRates tradingSymbols tickers

    printfn "%A" symbolPairExchangeRates

    0