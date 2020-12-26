unit _hxTester;

{$mode objfpc}{$H+}{$codepage utf8}

interface

uses
  ComCtrls, ExtCtrls, Forms, HyperX, StdCtrls;

type

  { TfrmTester }

  TfrmTester = class(TForm)
    btnPrivateHT, btnPrivateWS, btnPublicHT, btnPublicWS, btnStartStop: TButton;
    edtAPIKey, edtAPISecret: TEdit;
    grbHT, grbLogbook, grbPrivSetts, grbWebSocket: TGroupBox;
    lblHTReceived: TLabel;
    lblWSReceived: TLabel;
    lblFormCenter, lblPrivSettsCenter: TLabel;
    mmHT, mmLog, mmWS: TMemo;
    stbTester: TStatusBar;
    tmTester: TTimer;
    procedure btnPrivateSendClick(Sender: TObject);
    procedure btnPrivateWSClick(Sender: TObject);
    procedure btnPublicSendClick(Sender: TObject);
    procedure btnPublicWSClick(Sender: TObject);
    procedure btnStartStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmTesterStartTimer(Sender: TObject);
  end;

var
  frmTester: TfrmTester;
  HPX: THyperX=nil;

implementation

uses
  FileUtil, SysUtils;

{$R *.lfm}

{ TfrmTester }

procedure TfrmTester.btnPrivateSendClick(Sender: TObject);
var
  AllRight: Boolean=True;
begin
  mmLog.ScrollBars:=ssAutoVertical;
  with HPX do begin
    if (FindDefaultExecutablePath(HyperTextClient)='')
      then with mmLog.Lines do begin
        Add('The "'+HyperTextClient+'" executable was not found!');
        Add('    You can get cURL at "https://curl.haxx.se/download.html".');
        AllRight:=False;
      end;
    if (FindDefaultExecutablePath(CryptoUtil)='')
      then with mmLog.Lines do begin
        Add('The "'+CryptoUtil+'" executable was not found!');
        Add('    You can get OpenSSL at "https://slproweb.com/products/Win32OpenSSL.html".');
        AllRight:=False;
      end;
    if (Trim(edtAPIKey.Text)='')
      then begin
        stbTester.SimpleText:=' API Key missing!';
        edtAPIKey.Clear;
        edtAPIKey.SetFocus;
      end
      else if (Trim(edtAPISecret.Text)='')
        then begin
          stbTester.SimpleText:=' API Secret missing!';
          edtAPISecret.Clear;
          edtAPISecret.SetFocus;
        end
        else if (AllRight)
          then with mmHT.Lines do begin
            edtAPIKey.Enabled:=False;
            edtAPISecret.Enabled:=False;
            Key:=Trim(edtAPIKey.Text);
            Secret:=Trim(edtAPISecret.Text);
            Clear;
            Add(PrivateRequest(
              'command=returnCompleteBalances&nonce='+NonceMark, [
              '-X', 'POST', '-d', DataMark.QuotedString('"'),
              '-H', '"Key: '+KeyMark+'"',
              '-H', '"Sign: '+SignMark+'"',
              'https://poloniex.com/tradingApi']
            ));
            mmLog.Lines.Add('Command "ReturnCompleteBalances" executed!');
          end;
  end;
  mmLog.ScrollBars:=ssAutoVertical;
end;

procedure TfrmTester.btnPrivateWSClick(Sender: TObject);
begin
  with HPX do if (Trim(edtAPIKey.Text)='')
    then begin
      stbTester.SimpleText:=' API Key missing!';
      edtAPIKey.Clear;
      edtAPIKey.SetFocus;
    end
    else if (Trim(edtAPISecret.Text)='')
      then begin
        stbTester.SimpleText:=' API Secret missing!';
        edtAPISecret.Clear;
        edtAPISecret.SetFocus;
      end
      else begin
        btnPrivateWS.Enabled:=False;
        edtAPIKey.Enabled:=False;
        edtAPISecret.Enabled:=False;
        Key:=Trim(edtAPIKey.Text);
        Secret:=Trim(edtAPISecret.Text);
        Send2PrivateWSQ(
          'nonce='+NonceMark,
          '{"command": "subscribe", '+
           '"channel": 1000, '+
           '"key": '+KeyMark.QuotedString('"')+', '+
           '"payload": '+DataMark.QuotedString('"')+', '+
           '"sign": '+SignMark.QuotedString('"')+'}'
        );
        mmLog.Lines.Add('Subscription command to "Account Notifications" sent!');
      end;
end;

procedure TfrmTester.btnPublicSendClick(Sender: TObject);
begin
  mmLog.ScrollBars:=ssAutoVertical;
  with HPX do
    if (FindDefaultExecutablePath(HyperTextClient)='')
      then with mmLog.Lines do begin
        Add('The "'+HyperTextClient+'" executable was not found!');
        Add('    You can get cURL at "https://curl.haxx.se/download.html".');
      end
      else with mmHT.Lines do begin
        Clear;
        Add(PublicRequest(['https://poloniex.com/public?command=returnTicker']));
        mmLog.Lines.Add('Command "ReturnTicker" executed!');
      end;
end;

procedure TfrmTester.btnPublicWSClick(Sender: TObject);
begin
  btnPublicWS.Enabled:=False;
  HPX.Send2PublicWSQ('{"command": "subscribe", "channel": "BTC_ETH"}');
  mmLog.Lines.Add('Subscription command to "BTC/ETH" sent!');
end;

procedure TfrmTester.btnStartStopClick(Sender: TObject);
begin
  mmLog.ScrollBars:=ssAutoVertical;
  with HPX do
    case (btnStartStop.Tag) of
      0: if (FindDefaultExecutablePath(WebSocketClient)='')
        then with mmLog.Lines do begin
          Add('The "'+WebSocketClient+'" executable was not found!');
          Add('    You can get WebSocat at "https://github.com/vi/websocat/releases/latest".');
          btnStartStop.Enabled:=False;
        end
        else begin
          Start;
          mmLog.Lines.Add('WebSocket listening start!');
          btnStartStop.Tag:=1;
          btnStartStop.Caption:='Stop';
          btnPublicWS.Enabled:=True;
          btnPrivateWS.Enabled:=True;
        end;
      1: begin
        Terminate;
        mmLog.Lines.Add('WebSocket listening time: '+IntToStr(HPX.ElapsedTime div 1000)+'s.');
        btnStartStop.Tag:=0;
        btnStartStop.Enabled:=False;
        btnStartStop.Caption:='Start';
        btnPublicWS.Enabled:=False;
        btnPrivateWS.Enabled:=False;
      end;
    end;
end;

procedure TfrmTester.FormCreate(Sender: TObject);
begin
  HPX:=THyperX.Create;
  with HPX do begin
    SetCryptoParams(['sha512', '-hmac', SecretMark]);
    SetWebSocketParams([
      '-v',
      '-B', PipeBufferSize.ToString,
      'wss://api2.poloniex.com'
    ]);
  end;
  tmTester.Enabled:=True;
  mmLog.Lines.Add('');
  mmLog.VertScrollBar.Position:=0
end;

procedure TfrmTester.FormDestroy(Sender: TObject);
begin
  tmTester.Enabled:=False;
  FreeAndNil(HPX);
end;

procedure TfrmTester.tmTesterStartTimer(Sender: TObject);
begin
  if (mmWS.Lines.Count>5000)
    then mmWS.Clear;
  if (mmLog.Lines.Count>2500)
    then mmLog.Clear;
  with HPX do begin
    if (Received.Count>0)
      then begin
        if (Received.Strings[0]<>'')
          then mmWS.Lines.Add(Received.Strings[0]);
        Received.Delete(0);
      end;
    if (Logbook.Count>0)
      then begin
        if (Logbook.Strings[0]<>'')
          then mmLog.Lines.Add(Logbook.Strings[0]);
        Logbook.Delete(0);
      end;
  end;
end;

end.

