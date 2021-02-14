unit Poloniex;

{$mode objfpc}{$H+}

interface

uses
  HyperX;

type

  TPoloAcc = (paAll=0, paExchange, paLending, paMargin);
  TPoloOpt = (poNone=0, poFillOrKill, poImmediateOrCancel, poPostOnly);

  { TwpPoloniex }

  TwpPoloniex = class(THyperX) //WP = Wrapper + Parser
  public
    constructor Create;
    function Buy(const CurrencyPair: String; const Rate, Amount: Extended; const Option: TPoloOpt=poNone; const ClientOrderID: Int64=0): String;
    function CancelAllOrders(const CurrencyPair: String=''): String;
    function CancelLoanOffer(const OrderNumber: Int64): String;
    function CancelOrder(const OrderNumber: Int64; const ClientOrderID: Int64=0): String;
    function CloseMarginPosition(const CurrencyPair: String): String;
    function CreateLoanOffer(const Currency: String; const Amount, LendingRate: Extended; const Duration: Byte=2; const AutoRenew: Boolean=True): String;
    function GenerateNewAddress(const Currency: String): String;
    function GetMarginPosition(const CurrencyPair: String='all'): String;
    function MarginBuy(const CurrencyPair: String; const Rate, Amount: Extended; const LendingRate: Extended=0.02; const ClientOrderID: Int64=0): String;
    function MarginSell(const CurrencyPair: String; const Rate, Amount: Extended; const LendingRate: Extended=0.02; const ClientOrderID: Int64=0): String;
    function MoveOrder(const OrderNumber: Int64; const Rate: Extended; const Amount: Extended=0; const Option: TPoloOpt=poNone; const ClientOrderID: Int64=0): String;
    function Return24hVolume: String;
    function ReturnActiveLoans: String;
    function ReturnAvailableAccountBalances: String;
    function ReturnBalances: String;
    function ReturnChartData(const CurrencyPair: String; const Period: Int64=86400; const Beginning: Int64=0; const Ending: Int64=0): String;
    function ReturnCompleteBalances(const Account: TPoloAcc=paAll): String;
    function ReturnCurrencies: String;
    function ReturnDepositAddresses: String;
    function ReturnDepositsWithdrawals(const Beginning: Int64=0; const Ending: Int64=0): String;
    function ReturnFeeInfo: String;
    function ReturnLendingHistory(const Beginning: Int64=0; const Ending: Int64=0; const Limit: Int64=0): String;
    function ReturnLoanOrders(const Currency: String): String;
    function ReturnMarginAccountSummary: String;
    function ReturnOpenLoanOffers: String;
    function ReturnOpenOrders(const CurrencyPair: String='all'): String;
    function ReturnOrderBook(const CurrencyPair: String='all'; const Depth: Byte=50): String;
    function ReturnOrderStatus(const OrderNumber: Int64): String;
    function ReturnOrderTrades(const OrderNumber: Int64): String;
    function ReturnPrivateTradeHistory(const CurrencyPair: String='all'; const Beginning: Int64=0; const Ending: Int64=0): String;
    function ReturnPublicTradeHistory(const CurrencyPair: String; const Beginning: Int64=0; const Ending: Int64=0): String;
    function ReturnTicker: String;
    function ReturnTradableBalances: String;
    function Sell(const CurrencyPair: String; const Rate, Amount: Extended; const Option: TPoloOpt=poNone; const ClientOrderID: Int64=0): String;
    function ToggleAutoRenew(const OrderNumber: Int64): String;
    function TransferBalance(const Currency: String; const FromAccount, ToAccount: TPoloAcc; const Amount: Extended): String;
    function Withdraw(const Currency, Address: String; const Amount: Extended; const PaymentID: Int64=0): String;
    procedure PrivateSubscribe(const Channel: String);
    procedure PublicSubscribe(const Channel: String);
    procedure Unsubscribe(const Channel: String);
  end;

implementation

uses
  SysUtils;

const //DEF = Default
  DEF_PRIVATEAPI='https://poloniex.com/tradingApi';
  DEF_PUBLICAPI='https://poloniex.com/public';
  DEF_WEBSOCKETAPI='wss://api2.poloniex.com';

{ TwpPoloniex }

constructor TwpPoloniex.Create;
begin
  inherited Create;
  PrivateAPI:=DEF_PRIVATEAPI;
  PublicAPI:=DEF_PUBLICAPI;
  WebSocketAPI:=DEF_WEBSOCKETAPI;
  SetCryptoParams(['sha512', '-hmac', SecretMarker]);
  SetPrivateParams([
    '-v',
    '-H', 'Key: '+KeyMarker,
    '-H', 'Sign: '+SignMarker,
    '-d', DataMarker,
    PrivateAPI]);
  SetPublicParams([
    '-v',
    PublicAPI+DataMarker
  ]);
  SetWebSocketParams([
    '-v',
    '-B', PipeBufferSize.ToString,
    WebSocketAPI
  ]);
end;

function TwpPoloniex.Buy(const CurrencyPair: String; const Rate, Amount: Extended; const Option: TPoloOpt=poNone; const ClientOrderID: Int64=0): String;
const
  DataStr='command=buy&nonce=';
  pCurrencyPair='&currencyPair=';
  pRate='&rate=';
  pAmount='&amount=';
  pClientOrderID='&clientOrderId=';
  pFillOrKill='&fillOrKill=1';
  pImmediateOrCancel='&immediateOrCancel=1';
  pPostOnly='&postOnly=1';
var
  Request: String='';
begin
  Request:=
    DataStr+NonceMarker+
    pCurrencyPair+CurrencyPair+
    pRate+FloatToStr(Rate)+
    pAmount+FloatToStr(Amount);
  if (ClientOrderID>0)
    then Request:=Request+pClientOrderID+IntToStr(ClientOrderID);
  case Option of
    poFillOrKill: Request:=Request+pFillOrKill;
    poImmediateOrCancel: Request:=Request+pImmediateOrCancel;
    poPostOnly: Request:=Request+pPostOnly;
  end;
  Result:=PrivateRequest(Request);
  Request:='';
end;

function TwpPoloniex.CancelAllOrders(const CurrencyPair: String): String;
const
  DataStr='command=cancelAllOrders&nonce=';
  pCurrencyPair='&currencyPair=';
var
  Request: String='';
begin
  Request:=DataStr+NonceMarker;
  if (CurrencyPair<>'')
    then Request:=Request+pCurrencyPair+CurrencyPair;
  Result:=PrivateRequest(Request);
  Request:='';
end;

function TwpPoloniex.CancelLoanOffer(const OrderNumber: Int64): String;
const
  DataStr='command=cancelLoanOffer&nonce=';
  pOrderNumber='&orderNumber=';
begin
  Result:=PrivateRequest(
    DataStr+NonceMarker+
    pOrderNumber+IntToStr(OrderNumber)
  );
end;

function TwpPoloniex.CancelOrder(const OrderNumber: Int64; const ClientOrderID: Int64): String;
const
  DataStr='command=cancelOrder&nonce=';
  pOrderNumber='&orderNumber=';
  pClientOrderID='&clientOrderId=';
var
  Request: String='';
begin
  Request:=DataStr+NonceMarker;
  if (ClientOrderID>0)
    then Request:=Request+pClientOrderID+IntToStr(ClientOrderID)
    else Request:=Request+pOrderNumber+IntToStr(OrderNumber);
  Result:=PrivateRequest(Request);
  Request:='';
end;

function TwpPoloniex.CloseMarginPosition(const CurrencyPair: String): String;
const
  DataStr='command=closeMarginPosition&nonce=';
  pCurrencyPair='&currencyPair=';
begin
  Result:=PrivateRequest(
    DataStr+NonceMarker+
    pCurrencyPair+CurrencyPair
  );
end;

function TwpPoloniex.CreateLoanOffer(const Currency: String; const Amount, LendingRate: Extended; const Duration: Byte=2; const AutoRenew: Boolean=True): String;
const
  DataStr='command=createLoanOffer&nonce=';
  pCurrency='&currency=';
  pAmount='&amount=';
  pLendingRate='&lendingRate=';
  pDuration='&duration=';
  pAutoRenew='&autoRenew=';
begin
  Result:=PrivateRequest(
    DataStr+NonceMarker+
    pCurrency+Currency+
    pAmount+FloatToStr(Amount)+
    pLendingRate+FloatToStr(LendingRate)+
    pDuration+IntToStr(Duration)+
    pAutoRenew+BoolToStr(AutoRenew, '1', '0')
  );
end;

function TwpPoloniex.GenerateNewAddress(const Currency: String): String;
const
  DataStr='command=generateNewAddress&nonce=';
  pCurrency='&currency=';
begin
  Result:=PrivateRequest(
    DataStr+NonceMarker+
    pCurrency+Currency
  );
end;

function TwpPoloniex.GetMarginPosition(const CurrencyPair: String): String;
const
  DataStr='command=getMarginPosition&nonce=';
  pCurrencyPair='&currencyPair=';
begin
  Result:=PrivateRequest(
    DataStr+NonceMarker+
    pCurrencyPair+CurrencyPair
  );
end;

function TwpPoloniex.MarginBuy(const CurrencyPair: String; const Rate, Amount: Extended; const LendingRate: Extended; const ClientOrderID: Int64): String;
const
  DataStr='command=marginBuy&nonce=';
  pCurrencyPair='&currencyPair=';
  pRate='&rate=';
  pAmount='&amount=';
  pLendingRate='&lendingRate=';
  pClientOrderID='&clientOrderId=';
var
  Request: String='';
begin
  Request:=
    DataStr+NonceMarker+
    pCurrencyPair+CurrencyPair+
    pRate+FloatToStr(Rate)+
    pAmount+FloatToStr(Amount);
  if (LendingRate>0)
    then Request:=Request+pLendingRate+FloatToStr(LendingRate);
  if (ClientOrderID>0)
    then Request:=Request+pClientOrderID+IntToStr(ClientOrderID);
  Result:=PrivateRequest(Request);
  Request:='';
end;

function TwpPoloniex.MarginSell(const CurrencyPair: String; const Rate, Amount: Extended; const LendingRate: Extended; const ClientOrderID: Int64): String;
const
  DataStr='command=marginSell&nonce=';
  pCurrencyPair='&currencyPair=';
  pRate='&rate=';
  pAmount='&amount=';
  pLendingRate='&lendingRate=';
  pClientOrderID='&clientOrderId=';
var
  Request: String='';
begin
  Request:=
    DataStr+NonceMarker+
    pCurrencyPair+CurrencyPair+
    pRate+FloatToStr(Rate)+
    pAmount+FloatToStr(Amount);
  if (LendingRate>0)
    then Request:=Request+pLendingRate+FloatToStr(LendingRate);
  if (ClientOrderID>0)
    then Request:=Request+pClientOrderID+IntToStr(ClientOrderID);
  Result:=PrivateRequest(Request);
  Request:='';
end;

function TwpPoloniex.MoveOrder(const OrderNumber: Int64; const Rate: Extended; const Amount: Extended; const Option: TPoloOpt; const ClientOrderID: Int64): String;
const
  DataStr='command=moveOrder&nonce=';
  pOrderNumber='&orderNumber=';
  pClientOrderID='&clientOrderId=';
  pRate='&rate=';
  pAmount='&amount=';
  pFillOrKill='&fillOrKill=1';
  pImmediateOrCancel='&immediateOrCancel=1';
  pPostOnly='&postOnly=1';
var
  Request: String='';
begin
  Request:=
    DataStr+NonceMarker+
    pOrderNumber+IntToStr(OrderNumber)+
    pRate+FloatToStr(Rate);
  if (Amount>0)
    then Request:=Request+pAmount+FloatToStr(Amount);
  if (ClientOrderID>0)
    then Request:=Request+pClientOrderID+IntToStr(ClientOrderID);
  case Option of
    poFillOrKill: Request:=Request+pFillOrKill;
    poImmediateOrCancel: Request:=Request+pImmediateOrCancel;
    poPostOnly: Request:=Request+pPostOnly;
  end;
  Result:=PrivateRequest(Request);
  Request:='';
end;

function TwpPoloniex.Return24hVolume: String;
const
  QueryStr='?command=return24hVolume';
begin
  Result:=PublicRequest(QueryStr);
end;

function TwpPoloniex.ReturnActiveLoans: String;
const
  DataStr='command=returnActiveLoans&nonce=';
begin
  Result:=PrivateRequest(DataStr+NonceMarker);
end;

function TwpPoloniex.ReturnAvailableAccountBalances: String;
const
  DataStr='command=returnAvailableAccountBalances&nonce=';
begin
  Result:=PrivateRequest(DataStr+NonceMarker);
end;

function TwpPoloniex.ReturnBalances: String;
const
  DataStr='command=returnBalances&nonce=';
begin
  Result:=PrivateRequest(DataStr+NonceMarker);
end;

function TwpPoloniex.ReturnChartData(const CurrencyPair: String; const Period: Int64; const Beginning: Int64; const Ending: Int64): String;
const
  QueryStr='?command=returnChartData';
  pCurrencyPair='&currencyPair=';
  pPeriod='&period=';
  pStart='&start=';
  pEnd='&end=';
var
  Request: String='';
begin
  Request:=
    PublicAPI+QueryStr+
    pCurrencyPair+CurrencyPair+
    pPeriod+IntToStr(Period)+
    pStart+IntToStr(Beginning);
  if (Ending>0)
    then Request:=Request+pEnd+IntToStr(Ending);
  Result:=PublicRequest(Request);
  Request:='';
end;

function TwpPoloniex.ReturnCompleteBalances(const Account: TPoloAcc=paAll): String;
const
  DataStr='command=returnCompleteBalances&nonce=';
  pAccount='&account=';
var
  Request: String='';
begin
  Request:=DataStr+NonceMarker;
  case Account of
    paAll: Request:=Request+pAccount+'all';
    //paExchange: Request:=Request+pAccount+'exchange';
    //paLending: Request:=Request+pAccount+'margin';
    //paMargin: Request:=Request+pAccount+'lending';
  end;
  Result:=PrivateRequest(Request);
  Request:='';
end;

function TwpPoloniex.ReturnCurrencies: String;
const
  QueryStr='?command=returnCurrencies';
begin
  Result:=PublicRequest(QueryStr);
end;

function TwpPoloniex.ReturnDepositAddresses: String;
const
  DataStr='command=returnDepositAddresses&nonce=';
begin
  Result:=PrivateRequest(DataStr+NonceMarker);
end;

function TwpPoloniex.ReturnDepositsWithdrawals(const Beginning: Int64; const Ending: Int64): String;
const
  DataStr='command=returnDepositsWithdrawals&nonce=';
  pStart='&start=';
  pEnd='&end=';
begin
  Result:=PrivateRequest(
    DataStr+NonceMarker+
    pStart+IntToStr(Beginning)+
    pEnd+IntToStr(Ending)
  );
end;

function TwpPoloniex.ReturnFeeInfo: String;
const
  DataStr='command=returnFeeInfo&nonce=';
begin
  Result:=PrivateRequest(DataStr+NonceMarker);
end;

function TwpPoloniex.ReturnLendingHistory(const Beginning: Int64; const Ending: Int64; const Limit: Int64): String;
const
  DataStr='command=returnLendingHistory&nonce=';
  pStart='&start=';
  pEnd='&end=';
  pLimit='&limit=';
var
  Request: String='';
begin
  Request:=
    DataStr+NonceMarker;
  if (Beginning>0)
    then Request:=Request+pStart+IntToStr(Beginning);
  if (Ending>0)
    then Request:=Request+pEnd+IntToStr(Ending);
  if (Limit>0)
    then Request:=Request+pLimit+IntToStr(Limit);
  Result:=PrivateRequest(Request);
  Request:='';
end;

function TwpPoloniex.ReturnLoanOrders(const Currency: String): String;
const
  QueryStr='?command=returnLoanOrders';
  pCurrency='&currency=';
begin
  Result:=PublicRequest(
    PublicAPI+QueryStr+
    pCurrency+Currency
  );
end;

function TwpPoloniex.ReturnMarginAccountSummary: String;
const
  DataStr='command=returnMarginAccountSummary&nonce=';
begin
  Result:=PrivateRequest(DataStr+NonceMarker);
end;

function TwpPoloniex.ReturnOpenLoanOffers: String;
const
  DataStr='command=returnOpenLoanOffers&nonce=';
begin
  Result:=PrivateRequest(DataStr+NonceMarker);
end;

function TwpPoloniex.ReturnOpenOrders(const CurrencyPair: String): String;
const
  DataStr='command=returnOpenOrders&nonce=';
  pCurrencyPair='&currencyPair=';
begin
  Result:=PrivateRequest(
    DataStr+NonceMarker+
    pCurrencyPair+CurrencyPair
  );
end;

function TwpPoloniex.ReturnOrderBook(const CurrencyPair: String; const Depth: Byte): String;
const
  QueryStr='?command=returnOrderBook';
  pCurrencyPair='&currencyPair=';
  pDepth='&depth=';
begin
  Result:=PublicRequest(
    PublicAPI+QueryStr+
    pCurrencyPair+CurrencyPair+
    pDepth+IntToStr(Depth)
  );
end;

function TwpPoloniex.ReturnOrderStatus(const OrderNumber: Int64): String;
const
  DataStr='command=returnOrderStatus&nonce=';
  pOrderNumber='&orderNumber=';
begin
  Result:=PrivateRequest(
    DataStr+NonceMarker+
    pOrderNumber+IntToStr(OrderNumber)
  );
end;

function TwpPoloniex.ReturnOrderTrades(const OrderNumber: Int64): String;
const
  DataStr='command=returnOrderTrades&nonce=';
  pOrderNumber='&orderNumber=';
begin
  Result:=PrivateRequest(
    DataStr+NonceMarker+
    pOrderNumber+IntToStr(OrderNumber)
  );
end;

function TwpPoloniex.ReturnPrivateTradeHistory(const CurrencyPair: String; const Beginning: Int64; const Ending: Int64): String;
const
  DataStr='command=returnTradeHistory&nonce=';
  pCurrencyPair='&currencyPair=';
  pStart='&start=';
  pEnd='&end=';
var
  Request: String='';
begin
  Request:=
    DataStr+NonceMarker+
    pCurrencyPair+CurrencyPair;
  if (Beginning>0)
    then Request:=Request+pStart+IntToStr(Beginning);
  if (Ending>0)
    then Request:=Request+pEnd+IntToStr(Ending);
  Result:=PrivateRequest(Request);
  Request:='';
end;

function TwpPoloniex.ReturnPublicTradeHistory(const CurrencyPair: String; const Beginning: Int64; const Ending: Int64): String;
const
  QueryStr='?command=returnTradeHistory';
  pCurrencyPair='&currencyPair=';
  pStart='&start=';
  pEnd='&end=';
var
  Request: String='';
begin
  Request:=
    PublicAPI+QueryStr+
    pCurrencyPair+CurrencyPair;
  if (Beginning>0)
    then Request:=Request+pStart+IntToStr(Beginning);
  if (Ending>0)
    then Request:=Request+pEnd+IntToStr(Ending);
  Result:=PublicRequest(Request);
  Request:='';
end;

function TwpPoloniex.ReturnTicker: String;
const
  QueryStr='?command=returnTicker';
begin
  Result:=PublicRequest(QueryStr);
end;

function TwpPoloniex.ReturnTradableBalances: String;
const
  DataStr='command=returnTradableBalances&nonce=';
begin
  Result:=PrivateRequest(DataStr+NonceMarker);
end;

function TwpPoloniex.Sell(const CurrencyPair: String; const Rate, Amount: Extended; const Option: TPoloOpt; const ClientOrderID: Int64): String;
const
  DataStr='command=sell&nonce=';
  pCurrencyPair='&currencyPair=';
  pRate='&rate=';
  pAmount='&amount=';
  pClientOrderID='&clientOrderId=';
  pFillOrKill='&fillOrKill=1';
  pImmediateOrCancel='&immediateOrCancel=1';
  pPostOnly='&postOnly=1';
var
  Request: String='';
begin
  Request:=
    DataStr+NonceMarker+
    pCurrencyPair+CurrencyPair+
    pRate+FloatToStr(Rate)+
    pAmount+FloatToStr(Amount);
  if (ClientOrderID>0)
    then Request:=Request+pClientOrderID+IntToStr(ClientOrderID);
  case Option of
    poFillOrKill: Request:=Request+pFillOrKill;
    poImmediateOrCancel: Request:=Request+pImmediateOrCancel;
    poPostOnly: Request:=Request+pPostOnly;
  end;
  Result:=PrivateRequest(Request);
  Request:='';
end;

function TwpPoloniex.ToggleAutoRenew(const OrderNumber: Int64): String;
const
  DataStr='command=toggleAutoRenew&nonce=';
  pOrderNumber='&orderNumber=';
begin
  Result:=PrivateRequest(
    DataStr+NonceMarker+
    pOrderNumber+IntToStr(OrderNumber)
  );
end;

function TwpPoloniex.TransferBalance(const Currency: String; const FromAccount, ToAccount: TPoloAcc; const Amount: Extended): String;
const
  DataStr='command=transferBalance&nonce=';
  pCurrency='&currency=';
  pAmount='&amount=';
  pFrom='&fromAccount=';
  pTo='&toAccount=';
var
  Request: String='';
begin
  Request:=
    DataStr+NonceMarker+
    pCurrency+Currency+
    pAmount+FloatToStr(Amount);
  case FromAccount of
    paExchange: Request:=Request+pFrom+'exchange';
    paLending: Request:=Request+pFrom+'lending';
    paMargin: Request:=Request+pFrom+'margin';
  end;
  case ToAccount of
    paExchange: Request:=Request+pTo+'exchange';
    paLending: Request:=Request+pTo+'lending';
    paMargin: Request:=Request+pTo+'margin';
  end;
  Result:=PrivateRequest(Request);
  Request:='';
end;

function TwpPoloniex.Withdraw(const Currency, Address: String; const Amount: Extended; const PaymentID: Int64): String;
const
  DataStr='command=withdraw&nonce=';
  pCurrency='&currency=';
  pAddress='&address=';
  pAmount='&amount=';
  pPaymentID='&paymentId=';
var
  Request: String='';
begin
  Request:=
    DataStr+NonceMarker+
    pCurrency+Currency+
    pAddress+Address+
    pAmount+FloatToStr(Amount);
  if (PaymentID>0)
    then Request:=Request+pPaymentID+IntToStr(PaymentID);
  Result:=PrivateRequest(Request);
  Request:='';
end;

procedure TwpPoloniex.PrivateSubscribe(const Channel: String);
const
  pChannel='#channel#';
var
  Data: String='';
  Message: String='';
begin
  Data:='nonce='+NonceMarker;
  Message:=
    '{"command": "subscribe", '+
    '"channel": "'+pChannel+'", '+
    '"key": '+KeyMarker.QuotedString('"')+', '+
    '"payload": '+DataMarker.QuotedString('"')+', '+
    '"sign": '+SignMarker.QuotedString('"')+'}';
  Message:=StringReplace(Message, pChannel, Channel, [rfReplaceAll]);
  Send2PrivateWSQ(Data, Message, False);
  Data:='';
  Message:='';
end;

procedure TwpPoloniex.PublicSubscribe(const Channel: String);
const
  pChannel='#channel#';
  DataStr='{ "command": "subscribe", "channel": "'+pChannel+'" }';
begin
  Send2PublicWSQ(StringReplace(DataStr, pChannel, Channel, [rfReplaceAll]), False);
end;

procedure TwpPoloniex.Unsubscribe(const Channel: String);
const
  pChannel='#channel#';
  DataStr='{ "command": "unsubscribe", "channel": "'+pChannel+'" }';
begin
  Send2PublicWSQ(StringReplace(DataStr, pChannel, Channel, [rfReplaceAll]), False);
end;

end.
