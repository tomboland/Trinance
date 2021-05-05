module BinanceApi
open System
open System.Text
open System.Security.Cryptography
open HttpFs.Client
open System.Net
open FSharp.Json
open Hopac

type BinanceApiKey = BinanceApiKey of string
type BinanceApiSecret = BinanceApiSecret of string
type BinanceApiRoot = BinanceApiRoot of string

type BinanceApiContext = {
    ApiKey: BinanceApiKey;
    ApiSecret: BinanceApiSecret;
    ApiRoot: BinanceApiRoot;
}

type ApiResponseError = 
    | Unknown = -1000
    | Disconnected = -1001
    | Unauthorized = -1002
    | TooManyRequests = -1003
    | UnexpectedResp = -1006
    | Timeout = -1007
    | InvalidMessage = -1013
    | UnknownOrderComposition = -1014
    | TooManyOrders = -1015
    | ServiceShuttingDown = -1016
    | UnsupportedOperation = -1020
    | InvalidTimestamp = -1021
    | InvalidSignature = -1022
    | IllegalChars = -1100
    | TooManyParameters = -1101
    | MandatoryParamEmptyOrMalformed = -1102
    | UnknownParam = -1103
    | UnreadParameters = -1004
    | ParamEmpty = -1105
    | ParamNotRequired = -1006
    | NoDepth = -1112
    | TifNotRequired = -1114
    | InvaliddTif = -1115
    | InvalidOrderType = -1116
    | InvalidSide = -1117
    | EmptyNewClOrdId = -1118
    | EmptyOrgClOrdId = -1119
    | BadInterval = -1120
    | BadSymbol = -1121
    | InvalidListenKey = -1125
    | MoreThanXxHours = -1127
    | OptionalParamsBadCombo = -1128
    | InvalidParameter = -1130
    | BadApiId = -2008
    | DuplicateApiKeyDesc = -2009
    | CancelAllFail = -2012
    | NoSuchOrder = -2013
    | BadApiKeyFmt = -2014
    | RejectedMbxKey = -2015

type ApiResponseErrorMsg = ApiResponseErrorMsg of string

type ApiResponseFailure = { code : int ; msg : string }

type ApiResult<'a> = 
    | Success of 'a
    | Failure of ApiResponseFailure

type SymbolStatus =
    | TRADING = 1
    | BREAK = 2

type ExchangeInfoSymbolFilterDTO = {
    filterType: string
    minPrice: string option
    maxPrice: string option
    tickSize: string option
    multiplierUp: string option
    multiplierDown: string option
    avgPriceMins: int option
}

type ExchangeInfoSymbolDTO = {
    symbol: string
    status: string
    baseAsset: string
    baseAssetPrecision: int
    quoteAsset: string
    quoteAssetPrecision: int
    baseCommissionPrecision: int
    quoteCommissionPrecision: int
    orderTypes: string list
    icebergAllowed: bool
    ocoAllowed: bool
    quoteOrderQtyMarketAllowed: bool
    isSpotTradingAllowed: bool
    isMarginTradingAllowed: bool
    filters: ExchangeInfoSymbolFilterDTO list
    permissions: string list
}

type ExchangeInfoRateLimitsDTO = {
    rateLimitType: string
    interval: string
    intervalNum: int
    limit: int
}

type ExchangeInfoDTO = {
    timezone: string
    serverTime: int64
    rateLimits: ExchangeInfoRateLimitsDTO list
    symbols: ExchangeInfoSymbolDTO list
}
type TickerPriceDTO = {
    symbol: string
    price: string
}

type AccountType = 
| MARGIN = 1
| SPOT = 2

type BalanceDTO = {
    asset: string
    free: string
    locked: string
}

type AccountInformationDTO = {
    makerCommission: int
    takerCommission: int
    buyerCommission: int
    sellerCommission: int
    canTrade: bool
    canWithdraw: bool
    canDeposit: bool
    updateTime: int64
    accountType: AccountType
    balances: BalanceDTO list
    permissions: string list option
}

type TickerPricesDTO = TickerPriceDTO list

type Asset = Asset of string

type Balance = {
    asset: Asset
    free: decimal
    locked: decimal
}

type Symbol = Symbol of string
type SymbolPair = {
    basis: Asset
    quote: Asset
}

type ExchangeRate = ExchangeRate of decimal

type BinanceApiEndpoint = BinanceApiEndpoint of string
let ExchangeInfoEndpoint = BinanceApiEndpoint "api/v1/exchangeInfo"
let TickerPriceEndpoint = BinanceApiEndpoint "api/v3/ticker/price"
let AccountInformationEndpoint = BinanceApiEndpoint "api/v3/account"

let unixTime () = DateTimeOffset(DateTime.Now).ToUnixTimeMilliseconds()

let responseToCodeBody (response : Response) : int * string =
    use ms = new IO.MemoryStream()
    response.body.CopyTo(ms)
    let responseBody = ms.ToArray() |> Encoding.ASCII.GetString
    (response.statusCode, responseBody)

let handleResponse (successFunc: (string -> 'a)) (response: Response) =
    let (code, body) = responseToCodeBody response
    match enum code with
    | HttpStatusCode.OK -> Success (successFunc body)
    | _ -> body |> Json.deserialize<ApiResponseFailure> |> Failure

let makeBinanceRequest (apiKey: BinanceApiKey) (req: Request): Response =
    let (BinanceApiKey uApiKey) = apiKey
    req
    |> Request.setHeader (Custom ("X-MBX-APIKEY", uApiKey))
    |> getResponse
    |> run

let bytesToHexString (bytes : byte[]) : string =
    bytes
    |> Seq.map (fun c -> c.ToString("X2"))
    |> Seq.reduce (+)

let signRequest (apiSecret: BinanceApiSecret) (queryString: string) =
    let (BinanceApiSecret uApiSecret) = apiSecret
    let secretBytes = Encoding.ASCII.GetBytes uApiSecret
    let secretQuery = Encoding.ASCII.GetBytes queryString
    use hmac = new HMACSHA256(secretBytes)
    hmac.ComputeHash(secretQuery)
    |> bytesToHexString

let makeBinanceUrl (apiRoot: BinanceApiRoot) (endpoint: BinanceApiEndpoint) (queryString: string) =
    let (BinanceApiRoot uApiRoot) = apiRoot
    let (BinanceApiEndpoint uEndpoint) = endpoint
    $"{uApiRoot}/{uEndpoint}?{queryString}"

let accountInformationRequest (context: BinanceApiContext) =
    let now = string (unixTime ())
    let signature = signRequest context.ApiSecret $"timestamp={now}"
    let queryString = $"timestamp={now}&signature={signature}"
    Request.createUrl Get (makeBinanceUrl context.ApiRoot AccountInformationEndpoint queryString)
    |> makeBinanceRequest context.ApiKey
    |> handleResponse Json.deserialize<AccountInformationDTO>

let exchangeInfoRequest (context: BinanceApiContext) =
    Request.createUrl Get (makeBinanceUrl context.ApiRoot ExchangeInfoEndpoint "")
    |> makeBinanceRequest context.ApiKey
    |> handleResponse Json.deserialize<ExchangeInfoDTO>

let tickerRequest (context: BinanceApiContext) =
    Request.createUrl Get (makeBinanceUrl context.ApiRoot TickerPriceEndpoint "")
    |> makeBinanceRequest context.ApiKey
    |> handleResponse Json.deserialize<TickerPricesDTO>
