unit _hxTester;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, ComCtrls, ExtCtrls, StdCtrls, Poloniex;

type

  { TfrmTester }

  TfrmTester = class(TForm)
    btnPrivateHT, btnPrivateWS, btnPublicHT, btnPublicWS, btnStartStop: TButton;
    cbExtMsgs: TCheckBox;
    edtAPIKey, edtAPISecret: TEdit;
    grbHT, grbLogbook, grbPrivSetts, grbWebSocket: TGroupBox;
    lblFormCenter, lblHTReceived, lblPrivSettsCenter, lblWSReceived: TLabel;
    mmHT, mmLog, mmWS: TMemo;
    pnlExtMsgs: TPanel;
    stbTester: TStatusBar;
    tmTester: TTimer;
    procedure btnPrivateSendClick(Sender: TObject);
    procedure btnPrivateWSClick(Sender: TObject);
    procedure btnPublicSendClick(Sender: TObject);
    procedure btnPublicWSClick(Sender: TObject);
    procedure btnStartStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ReceiveData(const Data: String);
    procedure ReceiveLog(const Data: String);
    procedure tmTesterTimer(Sender: TObject);
  end;

var
  frmTester: TfrmTester;
  HPX: TwpPoloniex=nil;

implementation

uses
  SysUtils, Dialogs;

{$R *.lfm}

{ TfrmTester }

procedure TfrmTester.btnPrivateSendClick(Sender: TObject);
var
  AllRight: Boolean=True;
begin
  with HPX do begin
    if not(FileExists(HyperTextClient))
      then with mmLog.Lines do begin
        Add('The "'+HyperTextClient+'" executable was not found!');
        Add('    You can get cURL at "https://curl.haxx.se/download.html".');
        AllRight:=False;
      end;
    if not(FileExists(CryptoUtil))
      then with mmLog.Lines do begin
        Add('The "'+CryptoUtil+'" executable was not found!');
        Add('    You can get OpenSSL at "https://wiki.openssl.org/index.php/Binaries".');
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
          then begin
            edtAPIKey.Enabled:=False;
            edtAPISecret.Enabled:=False;
            Key:=Trim(edtAPIKey.Text);
            Secret:=Trim(edtAPISecret.Text);
            with mmHT do begin
              Lines.Clear;
              Lines.AddText(ReturnCompleteBalances);
              CaretPos:=Point(0, Lines.Count-1);
              SelStart:=MaxInt;
            end;
            with mmLog do begin
              Lines.Add('Calling "ReturnCompleteBalances"...');
              CaretPos:=Point(0, Lines.Count-1);
              SelStart:=MaxInt;
            end;
          end;
  end;
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
        mmLog.Lines.Add('Subscribing to the "Account Notifications" channel...');
        PrivateSubscribe('1000');
      end;
end;

procedure TfrmTester.btnPublicSendClick(Sender: TObject);
begin
  with HPX do
    if not(FileExists(HyperTextClient))
      then with mmLog.Lines do begin
        Add('The "'+HyperTextClient+'" executable was not found!');
        Add('    You can get cURL at "https://curl.haxx.se/download.html".');
      end
      else begin
        with mmLog do begin
          Lines.AddText('Calling "ReturnTicker"...');
          CaretPos:=Point(0, Lines.Count-1);
          SelStart:=MaxInt;
        end;
        with mmHT do begin
          Lines.Clear;
          Lines.AddText(ReturnTicker);
          CaretPos:=Point(0, Lines.Count-1);
          SelStart:=MaxInt;
        end;
      end;
end;

procedure TfrmTester.btnPublicWSClick(Sender: TObject);
begin
  btnPublicWS.Enabled:=False;
  mmLog.Lines.Add('Subscribing to the "BTC/ETH" channel...');
  HPX.PublicSubscribe('BTC_ETH');
end;

procedure TfrmTester.btnStartStopClick(Sender: TObject);
begin
  with HPX do case (btnStartStop.Tag) of
    0: if not(FileExists(WebSocketClient))
      then with mmLog.Lines do begin
        Add('The "'+WebSocketClient+'" executable was not found!');
        Add('    You can get WebSocat at "https://github.com/vi/websocat/releases/latest".');
        btnStartStop.Enabled:=False;
      end
      else begin
        Start;
        mmLog.Lines.Add('WebSocket listening started!');
        btnStartStop.Tag:=1;
        btnStartStop.Caption:='Stop';
        btnPublicWS.Enabled:=True;
        btnPrivateWS.Enabled:=True;
      end;
    1: begin
      Terminate;
      mmLog.Lines.Add('WebSocket listening time: '+IntToStr(ElapsedTime div 1000)+'s.');
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
  HPX:=TwpPoloniex.Create;
end;

procedure TfrmTester.FormDestroy(Sender: TObject);
begin
  tmTester.Enabled:=False;
  FreeAndNil(HPX);
end;

procedure TfrmTester.FormShow(Sender: TObject);
begin
  OnShow:=nil;
  tmTester.Enabled:=True;
end;

procedure TfrmTester.ReceiveData(const Data: String);
begin
  with mmWS do begin
    Append(Data.Trim);
    if (Lines.Count>1000)
      then Lines.Clear;
    CaretPos:=Point(0, Lines.Count-1);
    SelStart:=MaxInt;
  end;
end;

procedure TfrmTester.ReceiveLog(const Data: String);
begin
  with mmLog do begin
    Append(Data.Trim);
    if (Lines.Count>1000)
      then Lines.Clear;
    CaretPos:=Point(0, Lines.Count-1);
    SelStart:=MaxInt;
  end;
end;

procedure TfrmTester.tmTesterTimer(Sender: TObject);
begin
  with HPX do begin
    with Received do if (Count>0)
      then repeat
        ReceiveData(Strings[0]);
        Delete(0);
      until (Count=0);
    with InternalLog do if (Count>0)
      then repeat
        ReceiveLog(Strings[0]);
        Delete(0);
      until (Count=0);
    with ExternalLog do if (cbExtMsgs.Checked) and (Count>0)
      then repeat
        ReceiveLog(Strings[0]);
        Delete(0);
      until (Count=0);
  end;
end;

end.
