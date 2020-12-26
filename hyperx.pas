unit HyperX;

{$mode objfpc}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, UTF8Process, Process;

type

  { THyperX }

  TArrOfStr = Array of String;

  THyperX = class(TThread)
  protected
    FAutoRestart, FAutoRetry: Boolean;
    FCountOfRetries, FCountOfStops, FPipeSize, FTimeout: QWord;
    FCriticalSection: TRTLCriticalSection;
    FCryptoParams, FIWEL, FMarksToStrip, FRecvd, FPubSendQ, FPubSentQ, FPrivSendQ, FPrivSentQ: TStringList;
    FCryptoUtil, FHTClient: String;
    FDataMark, FKeyMark, FNonceMark, FPassMark, FSecretMark, FSignMark, FSplitMark: String;
    FEnd, FLastRcv, FStart: TDateTime;
    FKey, FPass, FSecret: String;
    FNonce: Int64;
    FProcOptions: TProcessOptions;
    FProcPriority: TProcessPriority;
    FRetryLimit: Byte;
    FWSProcess: TProcessUTF8;
    function GetCryptoUtil: String;
    function GetElapsed: Int64;
    function GetHTClient: String;
    function GetWSClient: String;
    function ReplaceMark(const OldValue, NewValue: String; const Params: Array of String): TArrOfStr;
    function StripMarks(const OldValue: String; const Marks: TStringList): String;
    procedure RaiseException(const E: Exception);
    procedure ReplaceMark(const OldValue, NewValue: String; const Params: TStrings);
    procedure SetCryptoUtil(const AValue: String);
    procedure SetDataMark(const AValue: String);
    procedure SetHTClient(const AValue: String);
    procedure SetKey(const AValue: String);
    procedure SetKeyMark(const AValue: String);
    procedure SetNonceMark(const AValue: String);
    procedure SetPass(const AValue: String);
    procedure SetPassMark(const AValue: String);
    procedure SetPipeSize(const AValue: QWord);
    procedure SetProcOptions(const AValue: TProcessOptions);
    procedure SetProcPriority(const AValue: TProcessPriority);
    procedure SetSecret(const AValue: String);
    procedure SetSecretMark(const AValue: String);
    procedure SetSignMark(const AValue: String);
    procedure SetSplitMark(const AValue: String);
    procedure SetWSClient(const AValue: String);
  private
    function CheckParams(const Params: Array of String): Boolean;
    function CheckParams(const Params: TStrings): Boolean;
    function ListToArray(const StringList: TStringList): TArrOfStr;
    function NextNonce: Int64;
  public
    constructor Create;
    destructor Destroy; override;
    function GetSignature(const Data: String; const Params: Array of String): String;
    function PrivateRequest(const Data: String; const Params: Array of String): String;
    function PublicRequest(const Params: Array of String): String;
    procedure Execute; override;
    procedure Send2PrivateWSQ(const Data, Message: String);
    procedure Send2PublicWSQ(const Data: String);
    procedure SetCryptoParams(const Params: Array of String);
    procedure SetWebSocketParams(const Params: Array of String);
    procedure Terminate;
    property AutoRestart: Boolean read FAutoRestart write FAutoRestart default True;
    property AutoRetry: Boolean read FAutoRetry write FAutoRetry default True;
    property CountOfRetries: QWord read FCountOfRetries;
    property CountOfStops: QWord read FCountOfStops;
    property CryptoUtil: String read GetCryptoUtil write SetCryptoUtil;
    property DataMark: String read FDataMark write SetDataMark;
    property ElapsedTime: Int64 read GetElapsed;
    property HyperTextClient: String read GetHTClient write SetHTClient;
    property Key: String write SetKey;
    property KeyMark: String read FKeyMark write SetKeyMark;
    property Logbook: TStringList read FIWEL;
    property MarksToStrip: TStringList read FMarksToStrip;
    property NonceMark: String read FNonceMark write SetNonceMark;
    property Pass: String write SetPass;
    property PassMark: String read FPassMark write SetPassMark;
    property PipeBufferSize: QWord read FPipeSize write SetPipeSize default 65536;
    property Priority default tpTimeCritical;
    property ProcessOptions: TProcessOptions read FProcOptions write SetProcOptions default [poNoConsole, poUsePipes];
    property ProcessPriority: TProcessPriority read FProcPriority write SetProcPriority default ppRealTime;
    property Received: TStringList read FRecvd;
    property RetryLimit: Byte read FRetryLimit write FRetryLimit default 2;
    property Secret: String write SetSecret;
    property SecretMark: String read FSecretMark write SetSecretMark;
    property SignMark: String read FSignMark write SetSignMark;
    property SplitMark: String read FSplitMark write SetSplitMark;
    property Timeout: QWord read FTimeout write FTimeout default 5000;
    property WebSocketClient: String read GetWSClient write SetWSClient;
  end;

implementation

uses
  DateUtils, FileUtil, LazFileUtils, Math;

const
  DEF_CRYPTOUTIL='openssl';
  DEF_HTCLIENT='curl';
  DEF_WSCLIENT='websocat';
  DEF_DATAMARK='#data#';
  DEF_KEYMARK='#key#';
  DEF_NONCEMARK='#nonce#';
  DEF_PASSMARK='#pass#';
  DEF_SECRETMARK='#secret#';
  DEF_SIGNMARK='#sign#';
  DEF_SPLITMARK='#split#';
  DEF_STRIPMARK='(stdin)=';

{ THyperX }

constructor THyperX.Create;
begin //Refined at 2020-12-26 17:02
  inherited Create(True);
  InitCriticalSection(FCriticalSection);
  //Variables
  FEnd:=IncMinute(Now, GetLocalTimeOffset);
  FNonce:=DateTimeToUnix(FEnd);
  FStart:=FEnd;
  FLastRcv:=FEnd;
  FAutoRestart:=True;
  FAutoRetry:=True;
  FCountOfRetries:=0;
  FCountOfStops:=0;
  FKey:='';
  FPass:='';
  FPipeSize:=65536;
  FProcOptions:=[poNoConsole, poUsePipes];
  FProcPriority:=ppRealTime;
  FRetryLimit:=2;
  FSecret:='';
  FTimeout:=5000;
  //Properties
  FreeOnTerminate:=False;
  Priority:=tpTimeCritical;
  //Objects
  FWSProcess:=TProcessUTF8.Create(nil);
  with FWSProcess do begin
    Options:=FProcOptions;
    PipeBufferSize:=FPipeSize;
    Priority:=FProcPriority;
  end;
  FCryptoParams:=TStringList.Create;
  FIWEL:=TStringList.Create;
  FMarksToStrip:=TStringList.Create;
  FPrivSendQ:=TStringList.Create;
  FPrivSentQ:=TStringList.Create;
  FPubSendQ:=TStringList.Create;
  FPubSentQ:=TStringList.Create;
  FRecvd:=TStringList.Create;
  //Marks
  FMarksToStrip.Add(DEF_STRIPMARK);
  SetDataMark(DEF_DATAMARK);
  SetKeyMark(DEF_KEYMARK);
  SetNonceMark(DEF_NONCEMARK);
  SetPassMark(DEF_PASSMARK);
  SetSecretMark(DEF_SECRETMARK);
  SetSignMark(DEF_SIGNMARK);
  SetSplitMark(DEF_SPLITMARK);
  //External programs
  SetCryptoUtil(DEF_CRYPTOUTIL);
  SetHTClient(DEF_HTCLIENT);
  SetWSClient(DEF_WSCLIENT);
end;

destructor THyperX.Destroy;
begin //Refined at 2020-12-25 23:09
  //Properties
  FreeOnTerminate:=False;
  Priority:=tpNormal;
  //Objects
  if not(Finished)
    then Terminate;
  FWSProcess.Free;
  FCryptoParams.Free;
  FIWEL.Free;
  FMarksToStrip.Free;
  FRecvd.Free;
  FPrivSendQ.Free;
  FPrivSentQ.Free;
  FPubSendQ.Free;
  FPubSentQ.Free;
  //Variables
  FAutoRestart:=False;
  FAutoRetry:=False;
  FCountOfRetries:=0;
  FCountOfStops:=0;
  FCryptoUtil:='';
  FDataMark:='';
  FEnd:=0;
  FHTClient:='';
  FKey:='';
  FKeyMark:='';
  FLastRcv:=0;
  FNonce:=0;
  FNonceMark:='';
  FPass:='';
  FPassMark:='';
  FPipeSize:=0;
  FProcOptions:=[];
  FProcPriority:=ppNormal;
  FRetryLimit:=0;
  FSecret:='';
  FSecretMark:='';
  FSignMark:='';
  FSplitMark:='';
  FStart:=0;
  FTimeout:=0;
  DoneCriticalSection(FCriticalSection);
  inherited Destroy;
end;

function THyperX.CheckParams(const Params: Array of String): Boolean;
var //Refined at 2020-03-22 14:36
  p: Integer=0;
begin
  Result:=False;
  if (Length(Params)>0)
    then for p:=High(Params) downto Low(Params) do
      if ({%H-}Params[p].Trim<>'')
        then Result:=True; //Could an Exit call save a few milliseconds?
end;

function THyperX.CheckParams(const Params: TStrings): Boolean;
var //Refined at 2020-03-22 14:39
  p: Integer=0;
begin
  Result:=False;
  with Params do try
    if (Count>0)
      then for p:=Count-1 downto 0 do
        if (Params.Strings[p].Trim<>'')
          then Result:=True; //Could an Exit call save a few milliseconds?
  except
    on E: Exception do RaiseException(E);
  end;
end;

function THyperX.GetCryptoUtil: String;
begin //Refined at 2020-03-22 14:44
  Result:=ExtractFileNameOnly(FCryptoUtil);
end;

function THyperX.GetElapsed: Int64;
begin //Refined at 2020-03-22 14:43
  Result:=MilliSecondsBetween(FStart, FEnd);
end;

function THyperX.GetHTClient: String;
begin //Refined at 2020-03-22 14:44
  Result:=ExtractFileNameOnly(FHTClient);
end;

function THyperX.GetSignature(const Data: String; const Params: Array of String): String;
var //Refined at 2020-12-26 03:29
  Attempts: Byte=0;
  Buffer: String='';
  TLSSL_Process: TProcessUTF8=nil;
begin
  if (CheckParams(Params))
    then begin
      TLSSL_Process:=TProcessUTF8.Create(nil);
      with TLSSL_Process do try
        Executable:=FCryptoUtil;
        Options:=FProcOptions;
        Parameters.AddStrings(Params);
        PipeBufferSize:=FPipeSize;
        Priority:=FProcPriority;
        ReplaceMark(FKeyMark, FKey, Parameters);
        ReplaceMark(FPassMark, FPass, Parameters);
        ReplaceMark(FSecretMark, FSecret, Parameters);
        try
          repeat
            if (Attempts>0)
              then FCountOfRetries+=1;
            Result:='';
            Execute;
            Input.Write(Data[1], Length(Data));
            CloseInput;
            with Output do repeat
              SetLength(Buffer, NumBytesAvailable);
              ReadBuffer(Buffer[1], Length(Buffer));
              if (Length(Buffer)>0)
                then Result:=Result+Buffer;
            until not(Running) and (NumBytesAvailable=0);
            Result:=StripMarks(Result, FMarksToStrip).Trim;
            Attempts+=1;
          until (Result<>'') or (Attempts=FRetryLimit) or (AutoRetry=False);
        except
          on E: Exception do RaiseException(E);
        end;
      finally
        Attempts:=0;
        Buffer:='';
        FreeAndNil(TLSSL_Process);
      end;
    end;
end;

function THyperX.GetWSClient: String;
begin //Refined at 2020-03-22 16:56
  Result:=ExtractFileNameOnly(FWSProcess.Executable);
end;

function THyperX.ListToArray(const StringList: TStringList): TArrOfStr;
var //Refined at 2020-03-22 17:01
  p: Integer=0;
begin
  SetLength(Result, 0);
  with StringList do try
    if (Count>0)
      then begin
        for p:=0 to Count-1 do begin
          SetLength(Result, Length(Result)+1);
          Result[High(Result)]:=StringList[p];
        end;
        p:=0;
      end;
  except
    on E: Exception do RaiseException(E);
  end;
end;

function THyperX.NextNonce: Int64;
begin //Refined at 2020-12-26 17:02
  FNonce:=Max(FNonce+1, DateTimeToUnix(IncMinute(Now, GetLocalTimeOffset)));
  Result:=FNonce;
end;

function THyperX.PrivateRequest(const Data: String; const Params: Array of String): String;
var //Refined at 2020-12-26 03:30
  Attempts: Byte=0;
  Buffer: String='';
  CURL_Process: TProcessUTF8=nil;
  CurNonce: QWord=0;
  PrepData: String='';
begin
  if (CheckParams(Params))
    then begin
      CURL_Process:=TProcessUTF8.Create(nil);
      EnterCriticalSection(FCriticalSection);
      with CURL_Process do try
        Executable:=FHTClient;
        Options:=FProcOptions;
        PipeBufferSize:=FPipeSize;
        Priority:=FProcPriority;
        try
          repeat
            if (Attempts>0)
              then FCountOfRetries+=1;
            Parameters.AddStrings(Params, True);
            CurNonce:=NextNonce;
            PrepData:=StringReplace(Data, FNonceMark, CurNonce.ToString, [rfReplaceAll]);
            ReplaceMark(FDataMark, PrepData, Parameters);
            ReplaceMark(FKeyMark, FKey, Parameters);
            ReplaceMark(FPassMark, FPass, Parameters);
            ReplaceMark(FSecretMark, FSecret, Parameters);
            ReplaceMark(FSignMark, GetSignature(PrepData, ListToArray(FCryptoParams)), Parameters);
            Result:='';
            Execute;
            CloseInput;
            with Output do repeat
              SetLength(Buffer, NumBytesAvailable);
              ReadBuffer(Buffer[1], Length(Buffer));
              if (Length(Buffer)>0)
                then Result:=Result+Buffer;
            until not(Running) and (NumBytesAvailable=0);
            Result:=StripMarks(Result, FMarksToStrip).Trim;
            Attempts+=1;
          until (Result<>'') or (Attempts=FRetryLimit) or (AutoRetry=False);
        except
          on E: Exception do RaiseException(E);
        end;
      finally
        LeaveCriticalSection(FCriticalSection);
        Attempts:=0;
        Buffer:='';
        CurNonce:=0;
        FreeAndNil(CURL_Process);
        PrepData:='';
      end;
    end;
end;

function THyperX.PublicRequest(const Params: Array of String): String;
var //Refined at 2020-12-26 03:31
  Attempts: Byte=0;
  Buffer: String='';
  CURL_Process: TProcessUTF8=nil;
begin
  if CheckParams(Params)
    then begin
      CURL_Process:=TProcessUTF8.Create(nil);
      with CURL_Process do try
        Executable:=FHTClient;
        Options:=FProcOptions;
        Parameters.AddStrings(Params);
        PipeBufferSize:=FPipeSize;
        Priority:=FProcPriority;
        try
          repeat
            if (Attempts>0)
              then FCountOfRetries+=1;
            Result:='';
            Execute;
            CloseInput;
            with Output do repeat
              SetLength(Buffer, NumBytesAvailable);
              ReadBuffer(Buffer[1], Length(Buffer));
              if (Length(Buffer)>0)
                then Result:=Result+Buffer;
            until not(Running) and (NumBytesAvailable=0);
            Result:=StripMarks(Result, FMarksToStrip).Trim;
            Attempts+=1;
          until (Result<>'') or (Attempts=FRetryLimit) or (AutoRetry=False);
        except
          on E: Exception do RaiseException(E);
        end;
      finally
        Attempts:=0;
        Buffer:='';
        FreeAndNil(CURL_Process);
      end;
    end;
end;

function THyperX.ReplaceMark(const OldValue, NewValue: String; const Params: Array of String): TArrOfStr;
var //Refined at 2020-03-22 17:22
  p: Integer=0;
begin
  SetLength(Result, 0);
  if (Length(Params)>0) and (OldValue<>NewValue) and (OldValue<>'')
    then begin
      SetLength(Result, Length(Params));
      for p:=Length(Params)-1 downto 0 do
        Result[p]:=StringReplace(Params[p], OldValue, NewValue, [rfReplaceAll]);
    end;
end;

function THyperX.StripMarks(const OldValue: String; const Marks: TStringList): String;
var //Refined at 2020-03-22 18:23
  p: Integer=0;
begin
  Result:=OldValue;
  with Marks do if (OldValue<>'') and (CheckParams(Marks))
    then for p:=Count-1 downto 0 do
      Result:=StringReplace(Result, Marks[p], '', [rfReplaceAll]);
end;

procedure THyperX.Execute;
var //Refined at 2020-12-26 17:03
  Buffer: String='';
  ToRead: LongWord=0;

  procedure DoReceive;
  begin //Refined at 2020-12-26 17:03
    with FWSProcess do with Output do begin
      ToRead:=NumBytesAvailable;
      if (ToRead>0)
        then begin
          SetLength(Buffer, ToRead);
          Read(Buffer[1], ToRead);
          FRecvd.Add(Buffer);
          FLastRcv:=IncMinute(Now, GetLocalTimeOffset);
        end;
    end;
  end;

  procedure DoPrivateSend;
  var //Refined at 2020-04-08 01:21
    Splitted: TArrOfStr=nil;
    CurNonce: QWord=0;
    Data: String='';
    Message: String='';
  begin
    with FPrivSendQ do if (Count>0)
      then begin
        EnterCriticalSection(FCriticalSection);
        try
          try
            Splitted:=Strings[0].Split(FSplitMark);
            Data:=Splitted[Low(Splitted)];
            Message:=Splitted[High(Splitted)]+LineEnding;
            CurNonce:=NextNonce;
            Data:=StringReplace(Data, FNonceMark, CurNonce.ToString, [rfReplaceAll]);
            Message:=(StringReplace(Message, FDataMark, Data, [rfReplaceAll]));
            Message:=(StringReplace(Message, FKeyMark, FKey, [rfReplaceAll]));
            Message:=(StringReplace(Message, FPassMark, FPass, [rfReplaceAll]));
            Message:=(StringReplace(Message, FSecretMark, FSecret, [rfReplaceAll]));
            Message:=(StringReplace(Message, FSignMark, GetSignature(Data, ListToArray(FCryptoParams)), [rfReplaceAll]));
            FWSProcess.Input.Write(Message[1], Length(Message));
            FPrivSentQ.Add(Strings[0]);
            Delete(0);
          except
            on E: Exception do RaiseException(E);
          end;
        finally
          LeaveCriticalSection(FCriticalSection);
          CurNonce:=0;
          Data:='';
          Message:='';
          Splitted[High(Splitted)]:='';
          Splitted[Low(Splitted)]:='';
          SetLength(Splitted, 0);
        end;
      end;
  end;

  procedure DoPublicSend;
  begin //Refined at 2020-03-22 17:48
    with FPubSendQ do if (Count>0)
      then begin
        Buffer:=Strings[0]+LineEnding;
        FWSProcess.Input.Write(Buffer[1], Length(Buffer));
        FPubSentQ.Add(Strings[0]);
        Delete(0);
      end;
  end;

  procedure GetIWEL;
  begin //Refined at 2020-12-26 17:03
    with FWSProcess do with Stderr do begin
      ToRead:=NumBytesAvailable;
      if (ToRead>0)
        then begin
          SetLength(Buffer, ToRead);
          Read(Buffer[1], ToRead);
          FIWEL.Add(Buffer);
          FLastRcv:=IncMinute(Now, GetLocalTimeOffset);
        end;
    end;
  end;

begin
  with FWSProcess do if (CheckParams(Parameters))
    then try
      FStart:=IncMinute(Now, GetLocalTimeOffset);
      FEnd:=FStart;
      FLastRcv:=FEnd;
      repeat
        Active:=True;
        while Running do begin
          DoPrivateSend;
          DoPublicSend;
          DoReceive;
          GetIWEL;
          FEnd:=IncMinute(Now, GetLocalTimeOffset);
          if (FTimeout<>0) and (MilliSecondsBetween(FLastRcv, IncMinute(Now, GetLocalTimeOffset))>FTimeout)
            then begin
              Active:=False;
              FCountOfStops+=1;
              FPrivSentQ.AddStrings(FPrivSendQ);
              FPrivSendQ.AddStrings(FPrivSentQ, True);
              FPubSentQ.AddStrings(FPubSendQ);
              FPubSendQ.AddStrings(FPubSentQ, True);
              Active:=True;
            end;
        end;
        Active:=False;
        FCountOfStops+=1;
      until not(FAutoRestart);
      DoReceive;
      GetIWEL;
    except
      on E: Exception do RaiseException(E);
    end;
  Buffer:='';
  FEnd:=IncMinute(Now, GetLocalTimeOffset);
  ToRead:=0;
end;

procedure THyperX.RaiseException(const E: Exception);
begin //Refined at 2020-03-22 17:46
  FIWEL.Add(
    'An exception was raised!'+LineEnding+
    '  Unit: "'+E.UnitName+'";'+LineEnding+
    '  Class: "'+E.ClassName+'";'+LineEnding+
    '  Message: ['+E.Message+'].'
  );
end;

procedure THyperX.ReplaceMark(const OldValue, NewValue: String; const Params: TStrings);
var //Refined at 2020-03-22 17:45
  p: Integer=0;
begin
  with Params do try
    if (OldValue<>'') and (OldValue<>NewValue) and (Count>0)
      then for p:=Count-1 downto 0 do
        Params.Strings[p]:=StringReplace(Params.Strings[p], OldValue, NewValue, [rfReplaceAll]);
  except
    on E: Exception do RaiseException(E);
  end;
end;

procedure THyperX.Send2PrivateWSQ(const Data, Message: String);
begin //Refined at 2020-04-05 17:00
  if ({%H-}Message.Trim<>'')
    then FPrivSendQ.Add({%H-}Data.Trim+FSplitMark+Message.Trim);
end;

procedure THyperX.Send2PublicWSQ(const Data: String);
begin //Refined at 2020-03-22 17:43
  if ({%H-}Data.Trim<>'')
    then FPubSendQ.Add(Data.Trim);
end;

procedure THyperX.SetCryptoParams(const Params: Array of String);
begin //Refined at 2020-03-22 17:42
  if (CheckParams(Params))
    then FCryptoParams.AddStrings(Params, True);
end;

procedure THyperX.SetCryptoUtil(const AValue: String);
var //Refined at 2020-12-26 01:53
  DefPath: String='';
begin
  if ({%H-}AValue.Trim<>'')
    then begin
      DefPath:=FindDefaultExecutablePath(LazFileUtils.ExtractFileNameWithoutExt(AValue.Trim));
      if (DefPath<>'')
        then begin
          FCryptoUtil:=DefPath;
          DefPath:='';
        end
        else if (FileExists(AValue.Trim))
          then FCryptoUtil:=AValue.Trim
          else FCryptoUtil:=DEF_CRYPTOUTIL;
    end;
end;

procedure THyperX.SetDataMark(const AValue: String);
begin //Refined at 2020-03-22 18:23
  if ((FDataMark=DEF_DATAMARK) or (FDataMark='')) and (FDataMark<>{%H-}AValue.Trim)
    then FDataMark:=AValue.Trim;
end;

procedure THyperX.SetHTClient(const AValue: String);
var //Refined at 2020-12-26 01:53
  DefPath: String='';
begin
  if ({%H-}AValue.Trim<>'')
    then begin
      DefPath:=FindDefaultExecutablePath(ExtractFileNameOnly(AValue.Trim));
      if (DefPath<>'')
        then begin
          FHTClient:=DefPath;
          DefPath:='';
        end
        else if (FileExists(AValue.Trim))
          then FHTClient:=AValue.Trim
          else FHTClient:=DEF_HTCLIENT;
    end;
end;

procedure THyperX.SetKey(const AValue: String);
begin //Refined at 2020-03-22 17:33
  if (FKey<>{%H-}AValue.Trim) and (AValue.Trim<>'')
    then FKey:=AValue.Trim;
end;

procedure THyperX.SetKeyMark(const AValue: String);
begin //Refined at 2020-03-22 18:22
  if ((FKeyMark=DEF_KEYMARK) or (FKeyMark='')) and (FKeyMark<>{%H-}AValue.Trim)
    then FKeyMark:=AValue.Trim;
end;

procedure THyperX.SetNonceMark(const AValue: String);
begin //Refined at 2020-03-22 18:22
  if ((FNonceMark=DEF_NONCEMARK) or (FNonceMark='')) and (FNonceMark<>{%H-}AValue.Trim)
    then FNonceMark:=AValue.Trim;
end;

procedure THyperX.SetPass(const AValue: String);
begin //Refined at 2020-03-22 17:33
  if (FPass<>{%H-}AValue.Trim) and (AValue.Trim<>'')
    then FPass:=AValue.Trim;
end;

procedure THyperX.SetPassMark(const AValue: String);
begin //Refined at 2020-03-22 18:22
  if ((FPassMark=DEF_PASSMARK) or (FPassMark='')) and (FPassMark<>{%H-}AValue.Trim)
    then FPassMark:=AValue.Trim;
end;

procedure THyperX.SetPipeSize(const AValue: QWord);
begin //Refined at 2020-12-26 01:45
  if not(FWSProcess.Running) and (FPipeSize<>AValue)
    then begin
      FPipeSize:=AValue;
      FWSProcess.PipeBufferSize:=FPipeSize;
    end;
end;

procedure THyperX.SetProcOptions(const AValue: TProcessOptions);
begin //Refined at 2020-12-26 01:47
  if not(FWSProcess.Running) and (FProcOptions<>AValue)
    then begin
      FProcOptions:=AValue;
      if poNoConsole in FProcOptions
        then Exclude(FProcOptions, poNewConsole);
      if poRunSuspended in FProcOptions
        then Exclude(FProcOptions, poWaitOnExit);
      FWSProcess.Options:=FProcOptions;
    end;
end;

procedure THyperX.SetProcPriority(const AValue: TProcessPriority);
begin //Refined at 2020-12-26 01:48
  if not(FWSProcess.Running) and (FProcPriority<>AValue)
    then begin
      FProcPriority:=AValue;
      FWSProcess.Priority:=FProcPriority;
    end;
end;

procedure THyperX.SetSecret(const AValue: String);
begin //Refined at 2020-03-22 17:31
  if (FSecret<>{%H-}AValue.Trim) and (AValue.Trim<>'')
    then FSecret:=AValue.Trim;
end;

procedure THyperX.SetSecretMark(const AValue: String);
begin //Refined at 2020-03-22 18:22
  if ((FSecretMark=DEF_SECRETMARK) or (FSecretMark='')) and (FSecretMark<>{%H-}AValue.Trim)
    then FSecretMark:=AValue.Trim;
end;

procedure THyperX.SetSignMark(const AValue: String);
begin //Refined at 2020-03-22 18:22
  if ((FSignMark=DEF_SIGNMARK) or (FSignMark='')) and (FSignMark<>{%H-}AValue.Trim)
    then FSignMark:=AValue.Trim;
end;

procedure THyperX.SetSplitMark(const AValue: String);
begin //Refined at 2020-04-04 00:06
  if ((FSplitMark=DEF_SPLITMARK) or (FSplitMark='')) and (FSplitMark<>{%H-}AValue.Trim)
    then FSplitMark:=AValue.Trim;
end;

procedure THyperX.SetWebSocketParams(const Params: Array of String);
begin //Refined at 2020-03-22 17:27
  with FWSProcess do if not(Running) and (CheckParams(Params))
    then Parameters.AddStrings(Params, True);
end;

procedure THyperX.SetWSClient(const AValue: String);
var //Refined at 2020-12-26 01:54
  DefPath: String='';
begin
  with FWSProcess do begin
    if not(Running) and ({%H-}AValue.Trim<>'')
      then begin
        DefPath:=FindDefaultExecutablePath(ExtractFileNameOnly(AValue.Trim));
        if (DefPath<>'')
          then begin
            Executable:=DefPath;
            DefPath:='';
          end
          else if (FileExists(AValue.Trim))
            then Executable:=AValue.Trim
            else Executable:=DEF_WSCLIENT;
      end;
  end;
end;

procedure THyperX.Terminate;
begin //Refined at 2020-03-22 17:25
  FAutoRestart:=False;
  if (FWSProcess.Running)
    then FWSProcess.Terminate(0);
  inherited Terminate;
end;

end.
