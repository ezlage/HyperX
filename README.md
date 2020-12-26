# HyperX  
  
It is a proof of concept that aims to allow programs written in Lazarus and Freepascal to take advantage of any modern DCE (Digital Currency Exchange) API (Application Programming Interface).  
  
To avoid reinventing the wheel, external console programs are called through pipeline, commands are passed as parameters or sent to the STDIN, while responses are received from the STDOUT. The use of pipes is widely discussed, but this code has been tested and tuned in order to achieve satisfactory performance.  
  
As this code can be used only for data communication, it is necessary to develop a wrapper for each DCE/API, as well as a parser, both descending or relying on HyperX. Of course, you also need to design and develop strategies that can earn (or loss) money for you.  
  
### Dependencies  
    
 - [WebSocat](https://github.com/vi/websocat/releases/latest) or similar  
 - [OpenSSL](https://slproweb.com/products/Win32OpenSSL.html) or similar  
 - [cURL](https://curl.haxx.se/download.html) or similar  
  
### Usage  
  
The more logical processors and the higher the priority, the better. So, don't forget to set the CPU affinity and the priority of the program! From now on, many settings will be changed only for example purposes. **Be aware: default values are recommended!**  
  
##### Initialization  
  
```  
uses  
  Process;  
  
var  
  HPX: THyperX=nil;  
  
begin  
  HPX:=THyperX.Create;  
  with HPX do begin  
    //If you want to change the pipeline buffer size for all subprocesses  
    //57344=56 KiB; This setting seems to have effect only on Windows!  
    PipeBufferSize:=57344; //If you don't specify, the default value applies: 65536 (64 KiB)  
    //If you want to change the priority for all subprocesses  
    ProcessPriority:=ppHigh; //If you don't specify, the default value applies: ppRealTime  
    //If you want to free right after terminating  
    FreeOnTerminate:=True; //If you don't specify, the default value applies: False  
    //If you want to change the thread priority  
    Priority:=tpHighest; //If you don't specify, the default value applies: tpTimeCritical  
  end;  
end;  
```  
  
##### Starting WebSocket communication  
```  
with HPX do begin  
  //After how long without receiving data should the WebSocket node times out?  
  Timeout:=15000; //If you don't specify, the default value applies: 5000 (ms) 
  //If you don't want the WebSocket node to restart in case of a timeout  
  AutoRestart:=False; //If you don't specify, the default value applies: True   
  //If you want to use a custom WebSocket client  
  WebSocketClient:='some_websocket_client'; //If you don't specify, the default value applies: 'websocat'  
  //You need to provide the parameters that your WebSocket client needs  
  //The example below is for Poloniex, using WebSocat  
  SetWebSocketParams([  
    '-v', //Verbose mode (-v) is optional  
    '-B', PipeBufferSize.ToString, //Messages larger than this limit will be splitted  
    'wss://api2.poloniex.com'  
  ]);  
  Start; //From now on, all commands in the queue will be sent as soon as possible  
end;  
```  
  
##### Preparing to call remote methods  
```  
with HPX do begin  
  //If you don't want the HyperText node to retry in case of an empty response  
  AutoRetry:=False; //If you don't specify, the default value applies: True  
  //If you want to limit how many times to try to send a command  
  RetryLimit:=3; //If you don't specify, the default value applies: 2  
  //If you want to use a custom HyperText client  
  HyperTextClient:='some_hypertext_client'; //If you don't specify, the default value applies: 'curl'  
  //The above adjustments are enough to call public methods  
  //But for private methods, in addition to the above, the following are required  
  //If you want to use a custom cryptography utility  
  CryptoUtil:='some_crypto_util'; //If you don't specify, the default value applies: 'openssl'  
  //Markers are used to make it possible to work with any command syntax 
  //They will be automatically replaced with the appropriate value for each call  
  DataMark:='some_data_mark'; //If you don't specify, the default value applies: '#data#'  
  KeyMark:='some_key_mark'; //If you don't specify, the default value applies: '#key#'  
  NonceMark:='some_nonce_mark'; //If you don't specify, the default value applies: '#nonce#'  
  PassMark:='some_pass_mark'; //If you don't specify, the default value applies: '#pass#'  
  SecretMark:='some_secret_mark'; //If you don't specify, the default value applies: '#secret#'  
  SignMark:='some_sign_mark'; //If you don't specify, the default value applies: '#sign#'  
  SplitMark:='some_split_mark'; //If you don't specify, the default value applies: '#split#'  
  MarksToStrip.Clear; //If you don't clear, the default entries will remain: '(stdin)='   
  MarksToStrip.Add('some_strip_mark'); //If you want to strip something else
  //The settings below apply to both HyperText and WebSocket nodes  
  //Setting the way to sign messages for Poloniex, using OpenSSL  
  SetCryptoParams(['sha512', '-hmac', SecretMark]);  
  //Here, the sensitive data that identifies you to the DCE/API  
  Secret:='some_api_secret';  
  Key:='some_api_key';  
  //In some cases, you also need to set a passphrase, but not for Poloniex  
  Pass:='some_api_pass';  
end;  
```  
  
##### Public subscriptions  
```  
with HPX do begin  
  //You can queue an API command before or after the communication starts  
  Send2PublicWSQ('{"command": "subscribe", "channel": "BTC_ETH"}'); //ETH-BTC channel subscription  
  Send2PublicWSQ('{"command": "subscribe", "channel": "BTC_DASH"}'); //DASH-BTC channel subscription  
end;  
```  
  
##### Private subscriptions  
```  
with HPX do begin  
  //You can queue an API command before or after the communication starts  
  Send2PrivateWSQ(  
    'nonce='+NonceMark,  
    '{"command": "subscribe", '+  
     '"channel": 1000, '+  
     '"key": '+KeyMark.QuotedString('"')+', '+  
     '"payload": '+DataMark.QuotedString('"')+', '+  
     '"sign": '+SignMark.QuotedString('"')+'}'  
   );  
end;  
```  
  
##### Reading data received from WebSocket  
```  
with HPX do begin  
  if (Received.Count>0)  
    then begin //Don't do this in parallel!  
      //You will probably need to check if the line has been split  
      //You also need to check if the data is valid  
      //After all validations, do something with the data  
      DoSomethingWith(Received.Strings[0]);  
      Received.Delete(0); //Yeah, don't let it grow!  
    end;  
end;  
```  
  
##### Stopping WebSocket communication  
```  
begin  
  HPX.Terminate; //Before, if you wish, you can send unsubscribe commands  
  //As HyperX is based on TThread, you can't start it again unless you recreate it  
  //You can still send non-WebSocket requests  
end;  
```  
  
##### Executing public requests  
```  
var  
  Response: String='';  
begin  
  //The example below is for Poloniex, using cURL  
  Response:=PublicRequest(['https://poloniex.com/public?command=returnTicker']);  
  //You will probably need to check if the response is valid  
  //After validating, do something with the data  
  DoSomethingWith(Response);  
end;  
```  
  
##### Executing private requests  
```  
var  
  Response: String='';  
begin  
  with HPX do begin  
    //The example below is for Poloniex, using cURL  
    Response:=PrivateRequest(  
      'command=returnCompleteBalances&nonce='+NonceMark, [  
      '-X', 'POST', '-d', DataMark.QuotedString('"'),  
      '-H', '"Key: '+KeyMark+'"',  
      '-H', '"Sign: '+SignMark+'"',  
      'https://poloniex.com/tradingApi']  
    );  
  end;  
  //You will probably need to check if the response is valid  
  //After validating, do something with the data  
  DoSomethingWith(Response);  
end;  
```  
  
##### Reading warnings, exceptions, errors and other information  
```  
with HPX do begin  
  if (Logbook.Count>0)  
    then begin //Don't do this in parallel!  
      DoSomethingWith(Logbook.Strings[0]);  
      Logbook.Delete(0); //Yeah, don't let it grow!    
    end;  
end;  
```  
  
##### Finalization  
```  
uses  
  SysUtils;  
  
begin  
  FreeAndNil(HPX); //That simple!  
end;  
```  
  
### Tips, tricks and suggestions  
  
 - HyperTextClient, WebSocketClient and CryptoUtil parameters can receive full paths, file names with or without extension; The programs can be found in the PATH, current and working directories.  
 - For sequencing and synchronization, the same set of keys, secrets and passwords for private channels must not be used by more than one software or instance.  
 - The binaries of your project and its dependencies must have the same architecture!  
  
### Change log and roadmap  
  
#### Pending development  
  
- Basic JSON and XML capabilities  
- Better metrics and measurement for an overview of performance  
- Better queuing, control and contingency capabilities  
- More comprehensive test suite  
  
#### v0.0.0.1: Improvements and fixes  
  
- Additions to documentation  
- Aesthetic adjustments in the test suite  
- Constants DEF_CRYPTOUTIL, DEF_HTCLIENT and DEF_WSCLIENT added  
- HyperTextClient, WebSocketClient and CriptoUtil assignments fixed  
- Large responses jamming on PublicRequest, PrivateRequest and GetSignature fixed  
- NextNonce and FNonce changed from QWord to Int64  
- ProcessOptions property added  
- SetProcPriority and SetPipeSize enhanced  
- Time base altered from local to UTC  
- XMR address removed from donation option list  
  
#### v0.0.0.0: First public pre-release  
  
- Support for public and private channels under WebSocket, HTTP and TLS/SSL  
- TThread-based with Critical Section for sequencing and synchronization  
- Basic queuing, control and contingency capabilities  
- Limited metrics and measurement  
- Test suite included  
  
### License, credits, feedback and donation  
  
Creative Commons Zero v1.0 Universal  
Developed by [Ezequiel Lage](https://twitter.com/ezlage), Sponsored by [Lageteck](https://lageteck.com)  
For feedback, contact@lageteck.com  
  
#### Please, support this initiative!  
USDC: 0x38be157daf7448616ba5d4d500543c6dec8214cc  
BTC: 1KMBgg1h3TGPCWZyi4iFo55QvYrdo5JyRc  
ETH: 0x06f1382300723600b3fa9c16ae254e20264cb955  
DASH: Xt7BNFyCBxPdnubx5Yp1MjTn7sJLSnEd5i  
LTC: LZJPrFv7a7moL6oUHPo8ecCC9FcbY49uRe  
ZEC: t1eGBTghmxVbPVf4cco3MrEiQ8MZPQAzBFo  
DCR: Dscy8ziqa2qz1oFNcbTXDyt3V1ZFZttdRcn  