program hxTester;

{$mode objfpc}{$H+}{$codepage utf8}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}cthreads,{$ENDIF}{$ENDIF}
  Interfaces, Forms, SysUtils, LazFileUtils, _hxTester
  {$IFDEF WINDOWS}, Windows{$ENDIF};

{$R *.res}

begin
  {$IFDEF WINDOWS}SetPriorityClass(GetCurrentProcess,REALTIME_PRIORITY_CLASS);{$ENDIF}
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

