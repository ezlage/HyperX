unit HyperX;

{$mode objfpc}{$H+}

interface

uses
  Classes, Types, SysUtils, Process;

type

  { THyperX }

  THyperX = class(TThread)
  strict private
    FAutoRestart, FAutoRetry: Boolean;
    FCountOfRetries, FCountOfStops, FNonce, FPipeSize, FTimeOffset, FTimeout, FWaitOnRestart: Int64;
    FCriticalSection: TRTLCriticalSection;
    FCryptoParams, FExtLog, FIntLog, FMarkersToStrip, FRecvd: TStringList;
    FCryptoUtil, FDateTimeFmt, FHTClient, FKey, FPass, FPrivAPI, FPubAPI, FSecret, FWSAPI, FWSClient: String;
    FDataMarker, FKeyMarker, FNonceMarker, FPassMarker, FSecretMarker, FSendOnceMarker, FSignMarker, FSplitMarker: String;
    FEnd, FLastRcv, FStart: TDateTime;
    FOnExternalLog, FOnInternalLog, FOnNewData: TGetStrProc;
    FPrivateParams, FPublicParams, FWSParams, FPrivSendQ, FPrivSentQ, FPubSendQ, FPubSentQ: TStringList;
    FProcOptions: TProcessOptions;
    FProcPriority: TProcessPriority;
    FRetryLimit: Byte;
    function GetCryptoUtil: String;
    function GetElapsed: Int64;
    function GetHTClient: String;
    function GetWSClient: String;
    function NextNonce: Int64;
    procedure ForwardData(const AValue: String);
    procedure ForwardLog(const AValue: String);
    procedure SetCryptoUtil(const AValue: String);
    procedure SetDataMarker(const AValue: String);
    procedure SetDateTimeFmt(const AValue: String);
    procedure SetHTClient(const AValue: String);
    procedure SetKey(const AValue: String);
    procedure SetKeyMarker(const AValue: String);
    procedure SetNonceMarker(const AValue: String);
    procedure SetPass(const AValue: String);
    procedure SetPassMarker(const AValue: String);
    procedure SetPipeSize(const AValue: Int64);
    procedure SetPrivAPI(const AValue: String);
    procedure SetProcOptions(const AValue: TProcessOptions);
    procedure SetProcPriority(const AValue: TProcessPriority);
    procedure SetPubAPI(const AValue: String);
    procedure SetSecret(const AValue: String);
    procedure SetSecretMarker(const AValue: String);
    procedure SetSendOnceMarker(const AValue: String);
    procedure SetSignMarker(const AValue: String);
    procedure SetSplitMarker(const AValue: String);
    procedure SetWSAPI(const AValue: String);
    procedure SetWSClient(const AValue: String);
  strict protected
    procedure RaiseException(const E: Exception);
    procedure WriteLog(const Message: String=''; const Complement: String=''; const Header: String='');
  private
    function CheckMarker(const Marker: String): Boolean;
    function CheckParams(const Params: array of String): Boolean;
    function CheckParams(const Params: TStrings): Boolean;
    function ConcatLog(const StrArray: array of String): String;
    function FindProgram(const ANameOrPath: String): String;
    function PrepLog(const Str: String; const SuppressLB: Boolean=True): String;
    function Replace(const OldValue, NewValue: String; const Params: array of String): TStringDynArray;
    function Strip(const AValue: String): String;
    procedure Replace(const OldValue, NewValue: String; const Params: TStrings);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    function CustomRequest(const Data: String; const Params: array of String; const Signed: Boolean): String;
    function CustomRequest(const Data: String; const Params: TStrings; const Signed: Boolean): String;
    function GetSignature(const Data: String): String;
    function PrivateRequest(const Data: String): String;
    function PublicRequest(const Data: String): String;
    procedure Send2PrivateWSQ(const Data, Message: String; const SendOnce: Boolean=True);
    procedure Send2PublicWSQ(const Message: String; const SendOnce: Boolean=False);
    procedure SetCryptoParams(const Params: array of String);
    procedure SetPrivateParams(const Params: array of String);
    procedure SetPublicParams(const Params: array of String);
    procedure SetWebSocketParams(const Params: array of String);
    procedure Terminate;
  published
    property AutoRestart: Boolean read FAutoRestart write FAutoRestart default True;
    property AutoRetry: Boolean read FAutoRetry write FAutoRetry default True;
    property CountOfRetries: Int64 read FCountOfRetries;
    property CountOfStops: Int64 read FCountOfStops;
    property CryptoUtil: String read GetCryptoUtil write SetCryptoUtil;
    property DataMarker: String read FDataMarker write SetDataMarker;
    property DateTimeFmt: String read FDateTimeFmt write SetDateTimeFmt;
    property ElapsedTime: Int64 read GetElapsed;
    property ExternalLog: TStringList read FExtLog;
    property HyperTextClient: String read GetHTClient write SetHTClient;
    property InternalLog: TStringList read FIntLog;
    property Key: String write SetKey;
    property KeyMarker: String read FKeyMarker write SetKeyMarker;
    property MarkersToStrip: TStringList read FMarkersToStrip;
    property NonceMarker: String read FNonceMarker write SetNonceMarker;
    property OnExternalLog: TGetStrProc read FOnExternalLog write FOnExternalLog;
    property OnInternalLog: TGetStrProc read FOnInternalLog write FOnInternalLog;
    property OnNewData: TGetStrProc read FOnNewData write FOnNewData;
    property Pass: String write SetPass;
    property PassMarker: String read FPassMarker write SetPassMarker;
    property PipeBufferSize: Int64 read FPipeSize write SetPipeSize default 65536;
    property Priority default tpTimeCritical;
    property PrivateAPI: String read FPrivAPI write SetPrivAPI;
    property ProcessOptions: TProcessOptions read FProcOptions write SetProcOptions default [poUsePipes, poNoConsole];
    property ProcessPriority: TProcessPriority read FProcPriority write SetProcPriority default ppRealTime;
    property PublicAPI: String read FPubAPI write SetPubAPI;
    property Received: TStringList read FRecvd;
    property RetryLimit: Byte read FRetryLimit write FRetryLimit default 2;
    property Secret: String write SetSecret;
    property SecretMarker: String read FSecretMarker write SetSecretMarker;
    property SendOnceMarker: String read FSendOnceMarker write SetSendOnceMarker;
    property SignMarker: String read FSignMarker write SetSignMarker;
    property SplitMarker: String read FSplitMarker write SetSplitMarker;
    property TimeOffset: Int64 read FTimeOffset write FTimeOffset;
    property Timeout: Int64 read FTimeout write FTimeout default 5000;
    property WaitOnRestart: Int64 read FWaitOnRestart write FWaitOnRestart default 5000;
    property WebSocketAPI: String read FWSAPI write SetWSAPI;
    property WebSocketClient: String read GetWSClient write SetWSClient;
  end;

implementation

uses
  {$IFDEF UNIX}Unix, {$ENDIF}DateUtils, FileUtil, LazFileUtils, Math;

const //DEF = Default
  DEF_CRPTUTIL='openssl';
  DEF_HTCLIENT='curl';
  DEF_WSCLIENT='websocat';
  FMT_DATETIME='yyyy-mm-dd hh:nn:ss.zzz';
  LOG_CENSORED='-censored-';
  LOG_GENERIC='Generic log!';
  LOG_NONLITERAL='-non-literal-';
  LOG_SUPLINEBREAK='-slb-';
  MRK_DATA='#dat#';
  MRK_KEY='#key#';
  MRK_NONCE='#nnc#';
  MRK_PASS='#pss#';
  MRK_SECRET='#sct#';
  MRK_SENDONCE='#sdn#';
  MRK_SIGN='#sig#';
  MRK_SPLIT='#spl#';
  MRK_STRIP='(stdin)=';

{ THyperX }

constructor THyperX.Create;
begin
  //Objects
  inherited Create(True);
  InitCriticalSection(FCriticalSection);
  FCryptoParams:=TStringList.Create;
  FExtLog:=TStringList.Create;
  FIntLog:=TStringList.Create;
  FMarkersToStrip:=TStringList.Create;
  FPrivateParams:=TStringList.Create;
  FPrivSendQ:=TStringList.Create;
  FPrivSentQ:=TStringList.Create;
  FPublicParams:=TStringList.Create;
  FPubSendQ:=TStringList.Create;
  FPubSentQ:=TStringList.Create;
  FRecvd:=TStringList.Create;
  FWSParams:=TStringList.Create;
  //Properties
  FreeOnTerminate:=False;
  Priority:=tpTimeCritical;
  //Variables
  {$IFDEF UNIX}ReReadLocalTime;{$ENDIF}
  FTimeOffset:=GetLocalTimeOffset;
  FEnd:=IncMinute(Now, FTimeOffset);
  FLastRcv:=FEnd;
  FNonce:=DateTimeToUnix(FEnd);
  FStart:=FEnd;
  FAutoRestart:=True;
  FAutoRetry:=True;
  FCountOfRetries:=0;
  FCountOfStops:=0;
  FKey:='';
  FPass:='';
  FRetryLimit:=2;
  FSecret:='';
  FTimeout:=5000;
  FWaitOnRestart:=5000;
  SetDateTimeFmt(FMT_DATETIME);
  SetPipeSize(65536);
  SetProcOptions([poUsePipes, poNoConsole]);
  SetProcPriority(ppRealTime);
  //Markers
  FMarkersToStrip.Add(MRK_STRIP);
  SetDataMarker(MRK_DATA);
  SetKeyMarker(MRK_KEY);
  SetNonceMarker(MRK_NONCE);
  SetPassMarker(MRK_PASS);
  SetSecretMarker(MRK_SECRET);
  SetSendOnceMarker(MRK_SENDONCE);
  SetSignMarker(MRK_SIGN);
  SetSplitMarker(MRK_SPLIT);
  //External programs
  SetCryptoUtil(DEF_CRPTUTIL);
  SetHTClient(DEF_HTCLIENT);
  SetWSClient(DEF_WSCLIENT);
  //Cleanup
end;

destructor THyperX.Destroy;
begin
  if not(Suspended)
    then begin
      Terminate;
      WaitFor;
    end;
  //Properties
  FreeOnTerminate:=False;
  Priority:=tpNormal;
  //Variables
  FAutoRestart:=False;
  FAutoRetry:=False;
  FCountOfRetries:=0;
  FCountOfStops:=0;
  FCryptoUtil:='';
  FDataMarker:='';
  FEnd:=0;
  FHTClient:='';
  FKey:='';
  FKeyMarker:='';
  FLastRcv:=0;
  FNonce:=0;
  FNonceMarker:='';
  FPass:='';
  FPassMarker:='';
  FPipeSize:=0;
  FPrivAPI:='';
  FProcOptions:=[];
  FProcPriority:=ppNormal;
  FPubAPI:='';
  FRetryLimit:=0;
  FSecret:='';
  FSecretMarker:='';
  FSendOnceMarker:='';
  FSignMarker:='';
  FSplitMarker:='';
  FStart:=0;
  FTimeOffset:=0;
  FTimeout:=0;
  FWaitOnRestart:=0;
  FWSAPI:='';
  FWSClient:='';
  //Objects
  FCryptoParams.Free;
  FExtLog.Free;
  FIntLog.Free;
  FMarkersToStrip.Free;
  FPrivateParams.Free;
  FPrivSendQ.Free;
  FPrivSentQ.Free;
  FPublicParams.Free;
  FPubSendQ.Free;
  FPubSentQ.Free;
  FRecvd.Free;
  FWSParams.Free;
  DoneCriticalSection(FCriticalSection);
  inherited Destroy;
end;

function THyperX.CheckMarker(const Marker: String): Boolean;
begin
  Result:=False;
  if (Marker<>'') and (Marker=Marker.Trim) and (Marker<>FDataMarker) and (Marker<>FSendOnceMarker) and (Marker<>FKeyMarker) and
     (Marker<>FNonceMarker) and (Marker<>FPassMarker) and (Marker<>FSecretMarker) and (Marker<>FSignMarker) and (Marker<>FSplitMarker)
        then Result:=True
        else WriteLog(LOG_GENERIC, Marker, 'CheckMarker');
end;

function THyperX.CheckParams(const Params: array of String): Boolean;
var
  p: Integer=0;
begin
  Result:=False;
  if (Length(Params)>0)
    then begin
      for p:=High(Params) downto Low(Params) do
        if (Params[p].Trim<>'')
          then begin
            Result:=True;
            Break;
          end;
      p:=0;
    end
    else WriteLog(LOG_GENERIC, ConcatLog(Params), 'CheckParams');
end;

function THyperX.CheckParams(const Params: TStrings): Boolean;
var
  p: Integer=0;
begin
  Result:=False;
  with Params do try
    if (Count>0)
      then begin
        for p:=Count-1 downto 0 do
          if (Params.Strings[p].Trim<>'')
            then begin
              Result:=True;
              Break;
            end;
        p:=0;
      end
      else WriteLog(LOG_GENERIC, Params.Text, 'CheckParams');
  except
    on E: Exception do RaiseException(E);
  end;
end;

function THyperX.ConcatLog(const StrArray: array of String): String;
var
  Count: Integer=0;
begin
  Result:='';
  if (Length(StrArray)>0)
    then begin
      for Count:=High(StrArray) downto Low(StrArray) do
        Result:=StrArray[Count]+' '+Result;
      Result:=Result.Trim;
    end;
end;

function THyperX.CustomRequest(const Data: String; const Params: array of String; const Signed: Boolean): String;
var
  Attempts: Byte=0;
  CurNonce: Int64=0;
  HT_Process: TProcess=nil;
  PrepData: String='';
  Recvd: String='';
  Error: String='';
begin
  Result:='';
  if (CheckParams(Params))
    then begin
      WriteLog(LOG_GENERIC, ConcatLog([FHTClient, ConcatLog(Params)]), 'CustomRequest');
      HT_Process:=TProcess.Create(nil);
      EnterCriticalSection(FCriticalSection);
      with HT_Process do try
        Executable:=FHTClient;
        Options:=FProcOptions;
        PipeBufferSize:=FPipeSize;
        Priority:=FProcPriority;
        try
          repeat
            Result:='';
            Parameters.AddStrings(Params, True);
            if (Signed)
              then begin
                CurNonce:=NextNonce;
                PrepData:=StringReplace(Data, FNonceMarker, CurNonce.ToString, [rfReplaceAll]);
                Replace(FSignMarker, GetSignature(PrepData), Parameters);
              end
              else PrepData:=Data;
            Replace(FDataMarker, PrepData, Parameters);
            WriteLog(LOG_GENERIC, ConcatLog([Executable, ConcatLog(Parameters.ToStringArray)]), 'CustomRequest');
            Replace(FKeyMarker, FKey, Parameters);
            Replace(FPassMarker, FPass, Parameters);
            Replace(FSecretMarker, FSecret, Parameters);
            Active:=True;
            CloseInput;
            repeat
              with Output do if (NumBytesAvailable>0)
                then begin
                  SetLength(Recvd, NumBytesAvailable);
                  ReadBuffer(Recvd[1], Length(Recvd));
                  Result:=Result+Recvd;
                end;
              with Stderr do if (NumBytesAvailable>0)
                then begin
                  SetLength(Error, NumBytesAvailable);
                  ReadBuffer(Error[1], Length(Error));
                  ForwardLog(Error);
                end;
            until not(Running) and (Output.NumBytesAvailable=0) and (Stderr.NumBytesAvailable=0) and ((Recvd<>'') or (Error<>''));
            WriteLog(
              LOG_GENERIC,
              ConcatLog([
                BoolToStr(Running, True),
                Output.NumBytesAvailable.ToString,
                Stderr.NumBytesAvailable.ToString,
                Length(Recvd).ToString,
                Length(Error).ToString
              ]),
              'CustomRequest'
            );
            Result:=Strip(Result).Trim;
            if (Attempts>0)
              then FCountOfRetries+=1;
            Attempts+=1;
            Active:=False;
          until (Result<>'') or (Attempts>=FRetryLimit) or (FAutoRetry=False);
          WriteLog(
            LOG_GENERIC,
            ConcatLog([
              Length(Result).ToString,
              Attempts.ToString,
              BoolToStr(FAutoRetry, True)
            ]),
            'CustomRequest'
          );
        except
          on E: Exception do RaiseException(E);
        end;
      finally
        LeaveCriticalSection(FCriticalSection);
        FreeAndNil(HT_Process);
        Attempts:=0;
        CurNonce:=0;
        PrepData:='';
        Recvd:='';
        Error:='';
      end;
    end
    else WriteLog(LOG_GENERIC, ConcatLog([Data, ConcatLog(Params)]), 'CustomRequest');
end;

function THyperX.CustomRequest(const Data: String; const Params: TStrings; const Signed: Boolean): String;
begin
  Result:=CustomRequest(Data, Params.ToStringArray, Signed);
end;

function THyperX.FindProgram(const ANameOrPath: String): String;
var
  FileName: String='';
  NameOrPath: String='';
  DefaultPath: String='';
  ProgramPath: String='';
  WorkingPath: String='';
begin
  Result:='';
  NameOrPath:=TrimFilename(ANameOrPath);
  if (NameOrPath<>'')
    then begin
      FileName:=ExtractFileName(NameOrPath);
      ProgramPath:=IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+FileName;
      WorkingPath:=IncludeTrailingPathDelimiter(GetCurrentDir)+FileName;
      DefaultPath:=FindDefaultExecutablePath(FileName);
      if (FileExists(NameOrPath))
        then Result:=NameOrPath
        else if (NameOrPath<>FileName) and (FileExists(FileName))
          then Result:=FileName
          else if (FileExists(ProgramPath))
            then Result:=ProgramPath
            else if (FileExists(WorkingPath))
              then Result:=WorkingPath
              else if (DefaultPath<>'')
                then Result:=DefaultPath
                else WriteLog(LOG_GENERIC, ANameOrPath, 'FindProgram');
      FileName:='';
      NameOrPath:='';
      DefaultPath:='';
      ProgramPath:='';
      WorkingPath:='';
    end
    else WriteLog(LOG_GENERIC, ANameOrPath, 'FindProgram');
end;

function THyperX.GetCryptoUtil: String;
begin
  Result:=FCryptoUtil;
end;

function THyperX.GetElapsed: Int64;
begin
  Result:=MilliSecondsBetween(FStart, FEnd);
end;

function THyperX.GetHTClient: String;
begin
  Result:=FHTClient;
end;

function THyperX.GetSignature(const Data: String): String;
var
  Attempts: Byte=0;
  TLSSL_Process: TProcess=nil;
  Recvd: String='';
  Error: String='';
begin
  Result:='';
  if (CheckParams(FCryptoParams))
    then begin
      WriteLog(LOG_GENERIC, ConcatLog([FDataMarker, '|', FCryptoUtil, ConcatLog(FCryptoParams.ToStringArray)]), 'GetSignature');
      TLSSL_Process:=TProcess.Create(nil);
      with TLSSL_Process do try
        Executable:=FCryptoUtil;
        Options:=FProcOptions;
        PipeBufferSize:=FPipeSize;
        Priority:=FProcPriority;
        Parameters.AddStrings(FCryptoParams, True);
        WriteLog(
          LOG_GENERIC,
          ConcatLog([
            Data,
            '|',
            FCryptoUtil,
            ConcatLog(FCryptoParams.ToStringArray)
          ]),
          'GetSignature'
        );
        Replace(FKeyMarker, FKey, Parameters);
        Replace(FPassMarker, FPass, Parameters);
        Replace(FSecretMarker, FSecret, Parameters);
        try
          repeat
            Result:='';
            Active:=True;
            Input.Write(Data[1], Length(Data));
            CloseInput;
            repeat
              with Output do if (NumBytesAvailable>0)
                then begin
                  SetLength(Recvd, NumBytesAvailable);
                  ReadBuffer(Recvd[1], Length(Recvd));
                  if (Length(Recvd)>0)
                    then Result:=Result+Recvd;
                end;
              with Stderr do if (NumBytesAvailable>0)
                then begin
                  SetLength(Error, NumBytesAvailable);
                  ReadBuffer(Error[1], Length(Error));
                  ForwardLog(Error);
                end;
            until not(Running) and (Output.NumBytesAvailable=0) and (Stderr.NumBytesAvailable=0) and ((Recvd<>'') or (Error<>''));
            WriteLog(
              LOG_GENERIC,
              ConcatLog([
                BoolToStr(Running, True),
                Output.NumBytesAvailable.ToString,
                Stderr.NumBytesAvailable.ToString,
                Length(Recvd).ToString,
                Length(Error).ToString
              ]),
              'GetSignature'
            );
            Result:=Strip(Result).Trim;
            if (Attempts>0)
              then FCountOfRetries+=1;
            Attempts+=1;
          until (Result<>'') or (Attempts>=FRetryLimit) or (FAutoRetry=False);
          WriteLog(
            LOG_GENERIC,
            ConcatLog([
              Length(Result).ToString,
              Attempts.ToString,
              BoolToStr(FAutoRetry, True)
            ]),
            'GetSignature'
          );
        except
          on E: Exception do RaiseException(E);
        end;
      finally
        FreeAndNil(TLSSL_Process);
        Attempts:=0;
        Recvd:='';
        Error:='';
      end;
    end
    else WriteLog(LOG_GENERIC, Data, 'GetSignature');
end;

function THyperX.GetWSClient: String;
begin
  Result:=FWSClient;
end;

function THyperX.NextNonce: Int64;
begin
  FNonce:=Max(FNonce+1, DateTimeToUnix(IncMinute(Now, FTimeOffset)));
  Result:=FNonce;
  WriteLog(LOG_GENERIC, Result.ToString, 'NextNonce');
end;

function THyperX.PrepLog(const Str: String; const SuppressLB: Boolean): String;
var
  Count: Integer=0;
begin
  Result:='';
  if (Str.Trim<>'')
    then begin
      Result:=Str.Replace(#13, #10, [rfReplaceAll]);
      repeat
        Result:=StringReplace(Result, #10#10, #10, [rfReplaceAll], Count);
      until (Count=0);
      if (SuppressLB)
        then Result:=Result.Replace(#10, LOG_SUPLINEBREAK, [rfReplaceAll])
        else Result:=Result.Replace(#10, LineEnding, [rfReplaceAll]);
      repeat
        Result:=StringReplace(Result, #32#32, #32, [rfReplaceAll], Count);
      until (Count=0);
      Result:=Result.Trim;
    end;
end;

function THyperX.PrivateRequest(const Data: String): String;
begin
  Result:=CustomRequest(Data, FPrivateParams.ToStringArray, True);
end;

function THyperX.PublicRequest(const Data: String): String;
begin
  Result:=CustomRequest(Data, FPublicParams.ToStringArray, False);
end;

function THyperX.Replace(const OldValue, NewValue: String; const Params: array of String): TStringDynArray;
var
  p: Integer=0;
begin
  SetLength(Result, 0);
  if (OldValue<>NewValue) and (OldValue<>'') and (CheckParams(Params))
    then begin
      SetLength(Result, Length(Params));
      for p:=Length(Params)-1 downto 0 do
        Result[p]:=StringReplace(Params[p], OldValue, NewValue, [rfReplaceAll]);
    end
    else WriteLog(LOG_GENERIC, ConcatLog([OldValue, NewValue, ConcatLog(Params)]), 'Replace');
end;

function THyperX.Strip(const AValue: String): String;
var
  p: Integer=0;
begin
  Result:=AValue;
  with FMarkersToStrip do if (AValue<>'') and (CheckParams(FMarkersToStrip))
    then for p:=Count-1 downto 0 do begin
      if (CheckMarker(FMarkersToStrip[p]))
        then Result:=StringReplace(Result, FMarkersToStrip[p], '', [rfReplaceAll]);
    end
    else WriteLog(LOG_GENERIC, ConcatLog([AValue, FMarkersToStrip.Text]), 'Strip');
end;

procedure THyperX.Execute;
var
  Buffer: String='';
  WS_Process: TProcess=nil;

  procedure PrivateSend;
  var
    Splitted: TStringDynArray=nil;
    CurNonce: Int64=0;
    Data: String='';
    Message: String='';
  begin
    with FPrivSendQ do if (Count>0)
      then begin
        EnterCriticalSection(FCriticalSection);
        try
          try
            CurNonce:=NextNonce;
            Splitted:=StringReplace(Strings[0], FSendOnceMarker, '', [rfReplaceAll]).Split(FSplitMarker);
            Data:=Splitted[Low(Splitted)];
            Message:=Splitted[High(Splitted)]+LineEnding;
            Data:=StringReplace(Data, FNonceMarker, CurNonce.ToString, [rfReplaceAll]);
            Message:=(StringReplace(Message, FDataMarker, Data, [rfReplaceAll]));
            Message:=(StringReplace(Message, FSignMarker, GetSignature(Data), [rfReplaceAll]));
            WriteLog(LOG_GENERIC, Message, 'PrivateSend');
            Message:=(StringReplace(Message, FKeyMarker, FKey, [rfReplaceAll]));
            Message:=(StringReplace(Message, FPassMarker, FPass, [rfReplaceAll]));
            Message:=(StringReplace(Message, FSecretMarker, FSecret, [rfReplaceAll]));
            WS_Process.Input.Write(Message[1], Length(Message));
            if not(Strings[0].Contains(FSendOnceMarker))
              then FPrivSentQ.Add(Strings[0]);
            WriteLog(
              LOG_GENERIC,
              ConcatLog([
                Length(Data).ToString,
                Length(Message).ToString,
                BoolToStr(Strings[0].Contains(FSendOnceMarker), True)
              ]),
              'PrivateSend'
            );
            Delete(0);
          except
            on E: Exception do RaiseException(E);
          end;
        finally
          LeaveCriticalSection(FCriticalSection);
          Splitted[High(Splitted)]:='';
          Splitted[Low(Splitted)]:='';
          SetLength(Splitted, 0);
          CurNonce:=0;
          Message:='';
          Data:='';
        end;
      end;
  end;

  procedure PublicSend;
  begin
    with FPubSendQ do if (Count>0)
      then begin
        Buffer:=Strings[0]+LineEnding;
        Buffer:=Buffer.Replace(FSendOnceMarker, '', [rfReplaceAll]);
        WriteLog(LOG_GENERIC, Buffer, 'PublicSend');
        WS_Process.Input.Write(Buffer[1], Length(Buffer));
        if not(Strings[0].Contains(FSendOnceMarker))
          then FPubSentQ.Add(Strings[0]);
        WriteLog(
          LOG_GENERIC,
          ConcatLog([
            Length(Buffer).ToString,
            BoolToStr(Strings[0].Contains(FSendOnceMarker), True)
          ]),
          'PublicSend'
        );
        Delete(0);
      end;
  end;

  procedure GetData;
  begin
    with WS_Process do with Output do begin
      if (NumBytesAvailable>0)
        then begin
          SetLength(Buffer, NumBytesAvailable);
          Read(Buffer[1], Length(Buffer));
          ForwardData(Buffer);
          FLastRcv:=IncMinute(Now, FTimeOffset);
        end;
    end;
  end;

  procedure GetError;
  begin
    with WS_Process do with Stderr do begin
      if (NumBytesAvailable>0)
        then begin
          SetLength(Buffer, NumBytesAvailable);
          Read(Buffer[1], Length(Buffer));
          ForwardLog(Buffer);
          FLastRcv:=IncMinute(Now, FTimeOffset);
        end;
    end;
  end;

begin
  WS_Process:=TProcess.Create(nil);
  with WS_Process do if (CheckParams(FWSParams))
    then try
      try
        FStart:=IncMinute(Now, FTimeOffset);
        FEnd:=FStart;
        FLastRcv:=FEnd;
        Executable:=FWSClient;
        Options:=FProcOptions;
        PipeBufferSize:=FPipeSize;
        Priority:=FProcPriority;
        Parameters.AddStrings(FWSParams, True);
        repeat
          Active:=True;
          if (FCountOfStops>0) then Sleep(FWaitOnRestart);
          FLastRcv:=IncMinute(Now, FTimeOffset);
          repeat
            PrivateSend;
            PublicSend;
            GetData;
            GetError;
            if not(Terminated) and (FAutoRestart) and (FTimeout<>0) and (MilliSecondsBetween(FLastRcv, IncMinute(Now, FTimeOffset))>FTimeout)
              then begin
                Active:=False;
                FCountOfStops+=1;
                WriteLog(
                  LOG_GENERIC,
                  ConcatLog([
                    BoolToStr(Terminated, True),
                    BoolToStr(FAutoRestart, True),
                    FTimeout.ToString,
                    FormatDateTime(FDateTimeFmt, FLastRcv)
                  ]),
                  'Execute'
                );
                FPrivSentQ.AddStrings(FPrivSendQ);
                FPrivSendQ.AddStrings(FPrivSentQ, True);
                FPubSentQ.AddStrings(FPubSendQ);
                FPubSendQ.AddStrings(FPubSentQ, True);
                Active:=True;
                Sleep(FWaitOnRestart);
                FLastRcv:=IncMinute(Now, FTimeOffset);
              end;
            FEnd:=IncMinute(Now, FTimeOffset);
          until not(Running) or (Terminated);
          WriteLog(
            LOG_GENERIC,
            ConcatLog([
              BoolToStr(Running, True),
              BoolToStr(Terminated, True)
            ]),
            'Execute'
          );
          Active:=False;
          FCountOfStops+=1;
          FPrivSentQ.AddStrings(FPrivSendQ);
          FPrivSendQ.AddStrings(FPrivSentQ, True);
          FPubSentQ.AddStrings(FPubSendQ);
          FPubSendQ.AddStrings(FPubSentQ, True);
          FEnd:=IncMinute(Now, FTimeOffset);
        until not(FAutoRestart) or (Terminated);
        WriteLog(
          LOG_GENERIC,
          ConcatLog([
            BoolToStr(FAutoRestart, True),
            BoolToStr(Terminated, True)
          ]),
          'Execute'
        );
      except
        on E: Exception do RaiseException(E);
      end;
    finally
      FreeAndNil(WS_Process);
      Buffer:='';
      FEnd:=IncMinute(Now, FTimeOffset);
    end;
end;

procedure THyperX.ForwardData(const AValue: String);
begin
  if (AValue.Trim<>'')
    then begin
      if Assigned(FOnNewData)
        then try
          FOnNewData(AValue);
        except
          on E: Exception do begin
            FOnNewData:=nil;
            FRecvd.Add(AValue);
            RaiseException(E);
          end;
        end
        else FRecvd.Add(AValue);
    end;
end;

procedure THyperX.ForwardLog(const AValue: String);
begin
  if (AValue.Trim<>'')
    then begin
      if Assigned(FOnExternalLog)
        then try
          FOnExternalLog(PrepLog(AValue, False));
        except
          on E: Exception do begin
            FOnExternalLog:=nil;
            FExtLog.AddText(PrepLog(AValue, False));
            RaiseException(E);
          end;
        end
        else FExtLog.AddText(PrepLog(AValue, False));
    end;
end;

procedure THyperX.RaiseException(const E: Exception);
begin
  FIntLog.AddText(
    'An exception was raised!'+LineEnding+
    '  When: '+FormatDateTime(FDateTimeFmt, IncMinute(Now, FTimeOffset))+';'+LineEnding+
    '  Unit: "'+E.UnitName+'";'+LineEnding+
    '  Class: "'+E.ClassName+'";'+LineEnding+
    '  Message: ['+E.Message.Trim+'].'+LineEnding
  );
end;

procedure THyperX.Replace(const OldValue, NewValue: String; const Params: TStrings);
var
  p: Integer=0;
begin
  with Params do try
    if (OldValue<>'') and (OldValue<>NewValue) and (Count>0)
      then for p:=Count-1 downto 0 do
        Params.Strings[p]:=StringReplace(Params.Strings[p], OldValue, NewValue, [rfReplaceAll])
      else WriteLog(LOG_GENERIC, ConcatLog([OldValue, NewValue, Params.Text]), 'Replace');
  except
    on E: Exception do RaiseException(E);
  end;
end;

procedure THyperX.Send2PrivateWSQ(const Data, Message: String; const SendOnce: Boolean);
begin
  if (Message.Trim<>'')
    then case SendOnce of
      True: FPrivSendQ.Add(Data.Trim+FSplitMarker+FSendOnceMarker+Message.Trim);
      False: FPrivSendQ.Add(Data.Trim+FSplitMarker+Message.Trim);
    end
    else WriteLog(LOG_GENERIC, ConcatLog([Data, Message, BoolToStr(SendOnce, True)]), 'Send2PrivateWSQ');
end;

procedure THyperX.Send2PublicWSQ(const Message: String; const SendOnce: Boolean);
begin
  if (Message.Trim<>'')
    then case SendOnce of
      False: FPubSendQ.Add(Message.Trim);
      True: FPubSendQ.Add(Message.Trim+FSendOnceMarker);
    end
    else WriteLog(LOG_GENERIC, ConcatLog([Message, BoolToStr(SendOnce, True)]), 'Send2PublicWSQ');
end;

procedure THyperX.SetCryptoParams(const Params: array of String);
begin
  if (CheckParams(Params))
    then FCryptoParams.AddStrings(Params, True)
    else WriteLog(LOG_GENERIC, ConcatLog(Params), 'SetCryptoParams');
end;

procedure THyperX.SetCryptoUtil(const AValue: String);
var
  Path: String='';
begin
  Path:=FindProgram(AValue);
  if (Path<>'')
    then begin
      FCryptoUtil:=Path;
      Path:='';
    end
    else begin
      if (FCryptoUtil='')
        then FCryptoUtil:=DEF_CRPTUTIL;
      WriteLog(LOG_GENERIC, AValue, 'SetCryptoUtil');
    end;
end;

procedure THyperX.SetDataMarker(const AValue: String);
begin
  if (Suspended) and (CheckMarker(AValue.Trim))
    then begin
      Replace(FDataMarker, AValue.Trim, FCryptoParams);
      Replace(FDataMarker, AValue.Trim, FPrivateParams);
      Replace(FDataMarker, AValue.Trim, FPublicParams);
      Replace(FDataMarker, AValue.Trim, FPrivSendQ);
      Replace(FDataMarker, AValue.Trim, FPubSendQ);
      Replace(FDataMarker, AValue.Trim, FWSParams);
      FDataMarker:=AValue.Trim;
    end
    else WriteLog(LOG_GENERIC, ConcatLog([BoolToStr(Suspended, True), AValue]), 'SetDataMarker');
end;

procedure THyperX.SetDateTimeFmt(const AValue: String);
var
  AMoment: TDateTime=0;
  Converted: TDateTime=0;
  StrMoment: String='';
begin
  if (AValue.Trim<>'')
    then try
      try
        AMoment:=Now;
        StrMoment:=FormatDateTime(AValue, AMoment);
        TryStrToDateTime(StrMoment, Converted);
        if MinutesBetween(AMoment, Converted)=0
          then FDateTimeFmt:=AValue.Trim
          else begin
            FDateTimeFmt:=FMT_DATETIME;
            WriteLog(LOG_GENERIC, AValue, 'SetDateTimeFmt');
          end;
      except
        on E: Exception do begin
          RaiseException(E);
          FDateTimeFmt:=FMT_DATETIME;
        end;
      end;
    finally
      AMoment:=0;
      Converted:=0;
      StrMoment:='';
    end
    else WriteLog(LOG_GENERIC, AValue, 'SetDateTimeFmt');
end;

procedure THyperX.SetHTClient(const AValue: String);
var
  Path: String='';
begin
  Path:=FindProgram(AValue);
  if (Path<>'')
    then begin
      FHTClient:=Path;
      Path:='';
    end
    else begin
      if (FHTClient='')
        then FHTClient:=DEF_HTCLIENT;
      WriteLog(LOG_GENERIC, AValue, 'SetHTClient');
    end;
end;

procedure THyperX.SetKey(const AValue: String);
begin
  if (FKey<>AValue.Trim) and (AValue.Trim<>'')
    then FKey:=AValue.Trim
    else WriteLog(LOG_GENERIC, LOG_CENSORED, 'SetKey');
end;

procedure THyperX.SetKeyMarker(const AValue: String);
begin
  if (Suspended) and (CheckMarker(AValue.Trim))
    then begin
      Replace(FKeyMarker, AValue.Trim, FCryptoParams);
      Replace(FKeyMarker, AValue.Trim, FPrivateParams);
      Replace(FKeyMarker, AValue.Trim, FPublicParams);
      Replace(FKeyMarker, AValue.Trim, FPrivSendQ);
      Replace(FKeyMarker, AValue.Trim, FPubSendQ);
      Replace(FKeyMarker, AValue.Trim, FWSParams);
      FKeyMarker:=AValue.Trim;
    end
    else WriteLog(LOG_GENERIC, ConcatLog([BoolToStr(Suspended, True), AValue]), 'SetKeyMarker');
end;

procedure THyperX.SetNonceMarker(const AValue: String);
begin
  if (Suspended) and (CheckMarker(AValue.Trim))
    then begin
      Replace(FNonceMarker, AValue.Trim, FCryptoParams);
      Replace(FNonceMarker, AValue.Trim, FPrivateParams);
      Replace(FNonceMarker, AValue.Trim, FPublicParams);
      Replace(FNonceMarker, AValue.Trim, FPrivSendQ);
      Replace(FNonceMarker, AValue.Trim, FPubSendQ);
      Replace(FNonceMarker, AValue.Trim, FWSParams);
      FNonceMarker:=AValue.Trim;
    end
    else WriteLog(LOG_GENERIC, ConcatLog([BoolToStr(Suspended, True), AValue]), 'SetNonceMarker');
end;

procedure THyperX.SetPass(const AValue: String);
begin
  if (FPass<>AValue.Trim) and (AValue.Trim<>'')
    then FPass:=AValue.Trim
    else WriteLog(LOG_GENERIC, LOG_CENSORED, 'SetPass');
end;

procedure THyperX.SetPassMarker(const AValue: String);
begin
  if (Suspended) and (CheckMarker(AValue.Trim))
    then begin
      Replace(FPassMarker, AValue.Trim, FCryptoParams);
      Replace(FPassMarker, AValue.Trim, FPrivateParams);
      Replace(FPassMarker, AValue.Trim, FPublicParams);
      Replace(FPassMarker, AValue.Trim, FPrivSendQ);
      Replace(FPassMarker, AValue.Trim, FPubSendQ);
      Replace(FPassMarker, AValue.Trim, FWSParams);
      FPassMarker:=AValue.Trim;
    end
    else WriteLog(LOG_GENERIC, ConcatLog([BoolToStr(Suspended, True), AValue]), 'SetPassMarker');
end;

procedure THyperX.SetPipeSize(const AValue: Int64);
begin
  if (Suspended) and (AValue<>FPipeSize) and (AValue>0)
    then FPipeSize:=AValue
    else WriteLog(
      LOG_GENERIC,
      ConcatLog([
        BoolToStr(Suspended, True),
        AValue.ToString,
        FPipeSize.ToString
      ]),
      'SetPipeSize'
    );
end;

procedure THyperX.SetPrivAPI(const AValue: String);
begin
  if (AValue.Trim<>FPrivAPI) and (AValue.Trim<>'')
    then begin
      Replace(FPrivAPI, AValue.Trim, FPrivateParams);
      FPrivAPI:=AValue.Trim;
    end
    else WriteLog(LOG_GENERIC, ConcatLog([AValue, FPrivAPI]), 'SetPrivAPI');
end;

procedure THyperX.SetPrivateParams(const Params: array of String);
begin
  if (CheckParams(Params))
    then FPrivateParams.AddStrings(Params, True)
    else WriteLog(LOG_GENERIC, ConcatLog(Params), 'SetPrivateParams');
end;

procedure THyperX.SetProcOptions(const AValue: TProcessOptions);
begin
  if (Suspended) and (FProcOptions<>AValue)
    then begin
      FProcOptions:=AValue+[poUsePipes, poNoConsole];
      if (poNewConsole in FProcOptions)
        then Exclude(FProcOptions, poNoConsole);
      Exclude(FProcOptions, poWaitOnExit);
    end
    else WriteLog(LOG_GENERIC, ConcatLog([BoolToStr(Suspended, True), LOG_NONLITERAL]), 'SetProcOptions');
end;

procedure THyperX.SetProcPriority(const AValue: TProcessPriority);
begin
  if (Suspended) and (FProcPriority<>AValue)
    then FProcPriority:=AValue
    else WriteLog(LOG_GENERIC, ConcatLog([BoolToStr(Suspended, True), Ord(AValue).ToString]), 'SetProcPriority');
end;

procedure THyperX.SetPubAPI(const AValue: String);
begin
  if (AValue.Trim<>FPubAPI) and (AValue.Trim<>'')
    then FPubAPI:=AValue.Trim
    else WriteLog(LOG_GENERIC, ConcatLog([AValue, FPubAPI]), 'SetPubAPI');
end;

procedure THyperX.SetPublicParams(const Params: array of String);
begin
  if (CheckParams(Params))
    then FPublicParams.AddStrings(Params, True)
    else WriteLog(LOG_GENERIC, ConcatLog(Params), 'SetPublicParams');
end;

procedure THyperX.SetSecret(const AValue: String);
begin
  if (FSecret<>AValue.Trim) and (AValue.Trim<>'')
    then FSecret:=AValue.Trim
    else WriteLog(LOG_GENERIC, LOG_CENSORED, 'SetSecret');
end;

procedure THyperX.SetSecretMarker(const AValue: String);
begin
  if (Suspended) and (CheckMarker(AValue.Trim))
    then begin
      Replace(FSecretMarker, AValue.Trim, FCryptoParams);
      Replace(FSecretMarker, AValue.Trim, FPrivateParams);
      Replace(FSecretMarker, AValue.Trim, FPublicParams);
      Replace(FSecretMarker, AValue.Trim, FPrivSendQ);
      Replace(FSecretMarker, AValue.Trim, FPubSendQ);
      Replace(FSecretMarker, AValue.Trim, FWSParams);
      FSecretMarker:=AValue.Trim;
    end
    else WriteLog(LOG_GENERIC, ConcatLog([BoolToStr(Suspended, True), AValue]), 'SetSecretMarker');
end;

procedure THyperX.SetSendOnceMarker(const AValue: String);
begin
  if (Suspended) and (CheckMarker(AValue.Trim))
    then begin
      Replace(FSendOnceMarker, AValue.Trim, FCryptoParams);
      Replace(FSendOnceMarker, AValue.Trim, FPrivateParams);
      Replace(FSendOnceMarker, AValue.Trim, FPublicParams);
      Replace(FSendOnceMarker, AValue.Trim, FPrivSendQ);
      Replace(FSendOnceMarker, AValue.Trim, FPubSendQ);
      Replace(FSendOnceMarker, AValue.Trim, FWSParams);
      FSendOnceMarker:=AValue.Trim;
    end
    else WriteLog(LOG_GENERIC, ConcatLog([BoolToStr(Suspended, True), AValue]), 'SetSendOnceMarker');
end;

procedure THyperX.SetSignMarker(const AValue: String);
begin
  if (Suspended) and (CheckMarker(AValue.Trim))
    then begin
      Replace(FSignMarker, AValue.Trim, FCryptoParams);
      Replace(FSignMarker, AValue.Trim, FPrivateParams);
      Replace(FSignMarker, AValue.Trim, FPublicParams);
      Replace(FSignMarker, AValue.Trim, FPrivSendQ);
      Replace(FSignMarker, AValue.Trim, FPubSendQ);
      Replace(FSignMarker, AValue.Trim, FWSParams);
      FSignMarker:=AValue.Trim;
    end
    else WriteLog(LOG_GENERIC, ConcatLog([BoolToStr(Suspended, True), AValue]), 'SetSignMarker');
end;

procedure THyperX.SetSplitMarker(const AValue: String);
begin
  if (Suspended) and (CheckMarker(AValue.Trim))
    then begin
      Replace(FSplitMarker, AValue.Trim, FCryptoParams);
      Replace(FSplitMarker, AValue.Trim, FPrivateParams);
      Replace(FSplitMarker, AValue.Trim, FPublicParams);
      Replace(FSplitMarker, AValue.Trim, FPrivSendQ);
      Replace(FSplitMarker, AValue.Trim, FPubSendQ);
      Replace(FSplitMarker, AValue.Trim, FWSParams);
      FSplitMarker:=AValue.Trim;
    end
    else WriteLog(LOG_GENERIC, ConcatLog([BoolToStr(Suspended, True), AValue]), 'SetSplitMarker');
end;

procedure THyperX.SetWebSocketParams(const Params: array of String);
begin
  if (Suspended) and (CheckParams(Params))
    then FWSParams.AddStrings(Params, True)
    else WriteLog(LOG_GENERIC, ConcatLog([BoolToStr(Suspended, True), ConcatLog(Params)]), 'SetWebSocketParams');
end;

procedure THyperX.SetWSAPI(const AValue: String);
begin
  if (Suspended) and (AValue.Trim<>FWSAPI) and (AValue.Trim<>'')
    then FWSAPI:=AValue.Trim
    else WriteLog(LOG_GENERIC, ConcatLog([BoolToStr(Suspended, True), AValue, FWSAPI]), 'SetWSAPI');
end;

procedure THyperX.SetWSClient(const AValue: String);
var
  Path: String='';
begin
  Path:=FindProgram(AValue);
  if (Path<>'')
    then begin
      FWSClient:=Path;
      Path:='';
    end
    else begin
      if (FWSClient='')
        then FWSClient:=DEF_WSCLIENT;
      WriteLog(LOG_GENERIC, AValue, 'SetWSClient');
    end;
end;

procedure THyperX.Terminate;
begin
  FAutoRestart:=False;
  inherited Terminate;
end;

procedure THyperX.WriteLog(const Message: String=''; const Complement: String=''; const Header: String='');
var
  Log: String='';
begin
  if (Header<>'')
    then Log:=', '+Header;
  if (Concat(Header, Message, Complement).Trim<>'')
    then begin
      Log:=FormatDateTime(FDateTimeFmt, IncMinute(Now, FTimeOffset))+Log;
      Log:=Log+': ';
      if Message<>''
        then Log:=Log+Message+' ['+Complement+']'
        else Log:=Log+' ['+Complement+']';
    end;
  Log:=PrepLog(Log, True);
  if Assigned(FOnInternalLog)
    then try
      FOnInternalLog(Log);
    except
      on E: Exception do begin
        FOnInternalLog:=nil;
        FIntLog.Add(Log);
        RaiseException(E);
      end;
    end
    else FIntLog.Add(Log);
  Log:='';
end;

end.