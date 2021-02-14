program hxTester;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cmem, {$IFDEF UseCThreads}cthreads, {$ENDIF}{$ENDIF}
  Interfaces, {$IFDEF WINDOWS}Windows, {$ENDIF}Forms, SysUtils, LazFileUtils, _hxTester, Poloniex;

{$R *.res}

begin
  {$IFDEF WINDOWS}SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);{$ENDIF}
  {$IF DECLARED(UseHeapTrace)}
    GlobalSkipIfNoLeaks:=True;
    with Application do try
      SetHeapTraceOutput(Location+PathDelim+ExtractFileNameOnly(ExeName)+'-hptrc.log');
    except
      SetHeapTraceOutput(SysUtils.GetEnvironmentVariable('TEMP')+PathDelim+ExtractFileNameOnly(ExeName)+'-hptrc.log');
    end;
  {$ENDIF}
  RequireDerivedFormResource:=True;
  Application.Title:='Test Suite for HyperX';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmTester, frmTester);
  Application.Run;
  Application.Free;
end.