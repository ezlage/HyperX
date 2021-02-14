# HyperX

It allows programs written in Lazarus and Free Pascal to take advantage of modern CRUD, REST and WebSocket APIs.

Although it can serve many other purposes, it is being developed to be the data communication component of [EƵRA](https://github.com/ezlage/EZRA), the Ezequiel’s Robot for Automated and Algorithmic Trading. In that specific implementation, there are two other related components: [xLedger](https://github.com/ezlage/xLedger), the middleware; and [Hazard](https://github.com/ezlage/Hazard), the automated and algorithmic trader.

To avoid reinventing the wheel, external console programs are called via pipeline, commands are passed as parameters or sent to the STDIN, while responses are received from the STDOUT. The use of pipes is widely discussed, but this code has been tested and tuned to achieve satisfactory performance. HyperX is a descendant of TThread and has nested TProcess for running external programs.

Logically, it can be considered that there are three nodes: HyperText (HTTP/HTTPS), based on cURL; WebSocket (WS/WSS), based on WebSocat; and Crypto (Ciphering/Hashing), based on OpenSSL. HT, WS and CR are abbreviations that can refer to them, respectively. The first two depend on the third for data signing and authentication, a requirement on private channels.

### Dependencies

- [cURL](https://curl.haxx.se/download.html) or similar
- [OpenSSL](https://wiki.openssl.org/index.php/Binaries) or similar
- [WebSocat](https://github.com/vi/websocat/releases/latest) or similar

It is possible to use programs other than cURL, OpenSSL or WebSocat. The chosen architecture and design aim at flexibility and adaptability. There are no intentional limitations, but some changes may be necessary.

### Usage

Don't forget to adjust the CPU affinity and the program's priority for your use case! From now on, many settings will be changed for example purposes only. **Be aware: default values are recommended!**

##### Initializing
```Pascal
uses
  Process;

var
  HPX: THyperX=nil;

begin
  HPX:=THyperX.Create;
  with HPX do begin
    //To change the pipeline buffer size for all subprocesses (57344=56 KiB)
    PipeBufferSize:=57344; //If not specified, the default applies: 65536 (64 KiB)
    //To change the behavior of all subprocesses
    //poUsePipes will always be added and poWaitOnExit will always be removed
    ProcessOptions:=[]; //If not specified, the default applies: [poUsePipes, poNoConsole]
    //To change the priority for all subprocesses
    ProcessPriority:=ppHigh; //If not specified, the default applies: ppRealTime
    //To free right after terminating
    FreeOnTerminate:=True; //If not specified, the default applies: False
    //To change the thread priority
    Priority:=tpHighest; //If not specified, the default applies: tpTimeCritical
    //To use local time instead of UTC
    TimeOffset:=0; //If you don't specify, the default applies: GetLocalTimeOffset (minutes)
    //To change the date and time format used in logs
    //Any format that misses a minute or more of accuracy will be ignored
    DateTimeFmt:='dd-mm-yyyy hh:nn'; //If not specified, the default applies: 'yyyy-mm-dd hh:nn:ss.zzz'
    //Don't call Start yet!
  end;
end;
```

##### Configuring markers and request processing
```Pascal
with HPX do begin
  //Markers are used to do some pre-processing or post-processing on requests
  //Thanks to them it is possible to support different syntaxes and external programs
  //Although the need may be rare, you can change them
  //To change the marker that will be replaced by the data
  DataMarker:='some_data_marker'; //If not specified, the default applies: '#dat#'
  //To change the marker that will be replaced by the key
  KeyMarker:='some_key_marker'; //If not specified, the default applies: '#key#'
  //To change the marker that will be replaced by the nonce
  NonceMarker:='some_nonce_marker'; //If not specified, the default applies: '#nnc#'
  //To change the marker that will be replaced by the passphrase
  PassMarker:='some_pass_marker'; //If not specified, the default applies: '#pss#'
  //To change the marker that will be replaced by the secret
  SecretMarker:='some_secret_marker'; //If not specified, the default applies: '#sct#'
  //To change the marker of messages that can't be resent in the event of a restart
  SendOnceMarker:='some_sendonce_marker'; //If not specified, the default applies: '#sdn#'
  //To change the marker that will be replaced by the data signature
  SignMarker:='some_sign_marker'; //If not specified, the default applies: '#sig#'
  //To change the marker that separates the message from the data within the send queue
  SplitMarker:='some_split_marker'; //If not specified, the default applies: '#spl#'
  //To change the markers that will be removed from the pipeline output
  MarkersToStrip.Clear; //If not specified, the default remains: '(stdin)='
  MarkersToStrip.Add('some_strip_mark');
end;
```

##### Configuring the Crypto node
```Pascal
with HPX do begin
  //To use a custom cryptography utility/toolkit
  CryptoUtil:='some_crypto_util'; //If not specified, the default applies: 'openssl'
  //The settings below affects both HyperText and WebSocket nodes
  //Setting the way to sign data for Poloniex, using OpenSSL
  SetCryptoParams(['sha512', '-hmac', SecretMark]);
  //Here, the sensitive data that identifies you to private channels or APIs
  Secret:='some_api_secret';
  Key:='some_api_key';
  //In some cases, you also need to set a passphrase, but not for Poloniex
  Pass:='some_api_pass';
end;
```

##### Configuring the HyperText node
```Pascal
with HPX do begin
  //If you don't want to retry in case of an empty response
  AutoRetry:=False; //If not specified, the default applies: True
  //To limit how many times to try to send a command
  RetryLimit:=3; //If not specified, the default applies: 2
  //To use a custom HyperText client
  HyperTextClient:='some_hypertext_client'; //If not specified, the default applies: 'curl'
  //To provide the parameters that your HyperText client needs for public calls
  //For HTTP GET requests, data can be passed as Query String
  //The example below is for Poloniex, using cURL
  PublicAPI:='https://poloniex.com/public';
  SetPublicParams([ //Be careful with quotes, especially on Linux
    '-v', //Verbose mode, optional
    PublicAPI+DataMarker
  ]);
  //To provide the parameters that your HyperText client needs for private calls
  //For HTTP POST requests, data can be passed as parameter
  //The example below is for Poloniex, using cURL
  SetPrivateParams([ //Be careful with quotes, especially on Linux
    '-v', //Verbose mode, optional
    '-H', 'Key: '+KeyMarker,
    '-H', 'Sign: '+SignMarker,
    '-d', DataMarker,
    PrivateAPI
  ]);
end;
```

##### Configuring the WebSocket node
```Pascal
with HPX do begin
  //After how long without receiving data should the WebSocket node times out?
  Timeout:=15000; //If not specified, the default applies: 5000 (ms)
  //If you don't want the WebSocket node to restart in case of a timeout
  AutoRestart:=False; //If not specified, the default applies: True
  //How long to wait before resending previous messages?
  WaitOnRestart:=15000; //If not specified, the default applies: 5000 (ms)
  //To use a custom WebSocket client
  WebSocketClient:='some_websocket_client'; //If not specified, the default applies: 'websocat'
  //To provide the parameters that your WebSocket client needs
  //The example below is for Poloniex, using WebSocat
  //Data will be passed later via STDIN
  WebSocketAPI:='wss://api2.poloniex.com';
  SetWebSocketParams([ //Be careful with quotes, especially on Linux
    '-v', //Verbose mode, optional
    '-B', PipeBufferSize.ToString, //Messages larger than this limit will be splitted
    WebSocketAPI
  ]);
end;
```

##### WebSocket communication
```Pascal
with HPX do begin
  //You can queue an API command before or after starting the communication
  Start; //From now on, all commands in the queue will be sent as soon as possible
  //For better examples, take a look at the poloniex.pas file!  
  //Subscription to Poloniex's ETH-BTC public channel
  Send2PublicWSQ('{"command": "subscribe", "channel": "BTC_ETH"}');
  //Subscription to Poloniex's LTC-BTC public channel
  Send2PublicWSQ('{"command": "subscribe", "channel": "BTC_LTC"}');
  //Subscription to Poloniex's Account Notifications private channel
  Send2PrivateWSQ(
    'nonce='+NonceMark,
    '{"command": "subscribe", '+
     '"channel": 1000, '+
     '"key": '+KeyMark.QuotedString('"')+', '+
     '"payload": '+DataMark.QuotedString('"')+', '+
     '"sign": '+SignMark.QuotedString('"')+'}'
  );
  //If OnNewData is not assigned, received data will saved here
  if (Received.Count>0)
    then begin //Don't do this in parallel!
      //You will probably need to check if the line has been split
      //You also need to check if the data is valid
      //After all validations, do something with the data
      DoSomethingWith(Received.Strings[0]);
      Received.Delete(0); //Yeah, don't let it grow!
    end;
  //You can unsubscribe for specific data streams intead of terminate entirely!
  //For better examples, take a look at the poloniex.pas file!  
  Terminate; //Do this only when you no longer want to receive any data
  //As HyperX is based on TThread, you can't start it again unless you destroy and recreate it
  //You can still send non-WebSocket requests
end;
```

##### CRUD and REST requests
```Pascal
var
  PublicResponse: String='';
  PrivateResponse: String='';
begin
  //For better examples, take a look at the poloniex.pas file!
  //The example below is for Poloniex, using GET through cURL
  PublicResponse:=PublicRequest('?command=returnTicker');
  //You will probably need to check if the response is valid
  //After validating, do something with the data
  DoSomethingWith(PublicResponse);
  PublicResponse:='';
  //For better examples, take a look at the poloniex.pas file!
  //The example below is for Poloniex, using POST through cURL
  PrivateResponse:=PrivateRequest('command=returnCompleteBalances&nonce='+NonceMark);
  //You will probably need to check if the response is valid
  //After validating, do something with the data
  DoSomethingWith(PrivateResponse);
  PrivateResponse:='';
  //For better examples, take a look at the poloniex.pas file!
end;
```

##### Reading internal log messages
```Pascal
with HPX do begin
  //Internal is every log written by HyperX or its descendants
  //If OnInternalLog is not assigned, logs will saved here
  if (InternalLog.Count>0)
    then begin //Don't do this in parallel!
      DoSomethingWith(InternalLog.Strings[0]);
      InternalLog.Delete(0); //Yeah, don't let it grow!
    end;
end;
```

##### Reading external log messages
```Pascal
with HPX do begin
  //External is everything obtained through STDERR from each call to external programs
  //If OnExternalLog is not assigned, logs will saved here
  if (ExternalLog.Count>0)
    then begin //Don't do this in parallel!
      DoSomethingWith(ExternalLog.Strings[0]);
      ExternalLog.Delete(0); //Yeah, don't let it grow!
    end;
end;
```

##### Finishing
```Pascal
uses
  SysUtils;

begin
  FreeAndNil(HPX); //That simple!
end;
```

### Tips, tricks and suggestions

- For sequencing and synchronization, the same set of keys, secrets and passwords must not be used by more than one software or instance.
- HyperTextClient, WebSocketClient and CryptoUtil properties can receive full paths, file names with or without extension; The programs can be found in the PATH, current and working directories.
- If you notice changes or increments that may be beneficial for any use case, give the suggestion or send a pull request.
- When using events, be careful with thread safety and be aware that some things should only be done within the scope of the main thread, others should be done asynchronously and so on.

### Change log and roadmap

#### Pending development

- Basic JSON and XML capabilities
- Better metrics and measurement for an overview of performance
- Better queuing, logging, control and contingency capabilities
- Mac adjustments and tests
- More comprehensive test suite
- Parser for Poloniex

#### v0.0.0.2: Several changes and improvements

- Additions to documentation
- Added a wrapper for Poloniex (poloniex.pas; it doesn't cover Futures API)
- Added events OnExternalLog, OnInternalLog and OnNewData
- Added functions ConcatLog, FindProgram and PrepLog
- Added marker and param SendOnce for WebSocket calls
- Added procedures ForwardData and ForwardLog
- Added properties PrivateAPI, PublicAPI and WebSocketAPI
- Added properties TimeOffset, WaitOnRestart and DateTimeFmt
- Adjustments for Linux (cmen, cthreads, appearance, etc)
- Better scrollbar movements on Test Suite
- Changed class visibility of objects, variables, procedures and functions
- Generic but standardized logging capabilities
- Logbook splitted into InternalLog and ExternalLog
- Public and Private requests are now on top of CustomRequest
- SetProcOptions enhanced
- Stability enhancements on CustomRequest, GetSignature and Execute
- Successfully tested on Windows 10, Debian 10 and OpenSUSE Tumbleweed 20210130
- The compiler UTF8 directives have been removed
- TProcessUTF8 changed to TProcess
- Several other changes, improvements and enhancements

#### v0.0.0.1: Improvements and fixes

- Additions to documentation
- Aesthetic adjustments in the test suite
- Constants DEF_CRYPTOUTIL, DEF_HTCLIENT and DEF_WSCLIENT added
- Default time base altered from local to UTC
- Fix for large responses on PublicRequest, PrivateRequest and GetSignature
- HyperTextClient, WebSocketClient and CriptoUtil assignments fixed
- NextNonce and FNonce changed from QWord to Int64
- ProcessOptions property added
- SetProcPriority and SetPipeSize enhanced
- XMR address removed from donation option list

#### v0.0.0.0: First public pre-release

- Support for public and private channels under WebSocket, HTTP and TLS/SSL
- TThread-based with a Critical Section for sequencing and synchronization
- Basic queuing, control and contingency capabilities
- Limited metrics and measurement
- Test suite included

### License, credits, feedback and donation

Creative Commons Zero v1.0 Universal  
Developed by [Ezequiel Lage](https://twitter.com/ezlage), Sponsored by [Lageteck](https://lageteck.com)  
Any and all suggestions, criticisms and contributions are welcome!  
Be in touch at contact@lageteck.com  

#### Please, support this initiative!
BTC: 1KMBgg1h3TGPCWZyi4iFo55QvYrdo5JyRc  
DASH: Xt7BNFyCBxPdnubx5Yp1MjTn7sJLSnEd5i  
DCR: Dscy8ziqa2qz1oFNcbTXDyt3V1ZFZttdRcn  
ETH: 0x06f1382300723600b3fa9c16ae254e20264cb955  
LTC: LZJPrFv7a7moL6oUHPo8ecCC9FcbY49uRe  
USDC: 0x38be157daf7448616ba5d4d500543c6dec8214cc  
ZEC: t1eGBTghmxVbPVf4cco3MrEiQ8MZPQAzBFo  