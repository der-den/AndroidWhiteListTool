unit ProcessFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, Dialogs;

  function GetProcessResult(filename, parameter:string; saveTo:string=''):TStringList;

implementation

function GetProcessResult(filename, parameter:string; saveTo:string=''):TStringList;
const
  READ_BYTES = 2048;
var
 P : TProcess;
 SL : TStringList;
 MS : TMemoryStream;
 n : Longint;
 BytesRead : LongInt;

begin

  SL := TStringList.Create;

  if (FileExists(filename) = false) then
  begin
    SL.Add('Run Process - file not exist: '+filename);
    GetProcessResult := SL;
    ShowMessage('exe-file for process not found - '+filename);
    exit;
  end;


  P := TProcess.Create(nil);
  SL := TStringList.Create;
  MS := TMemoryStream.Create;
  BytesRead := 0;

  P.CommandLine:=filename+' '+parameter;
  P.Options:=[poUsePipes,poStderrToOutPut];
  P.ShowWindow:=swoHIDE;
  P.Execute;

  // Lese vom Prozess
  while P.Running do
  begin
    MS.SetSize(BytesRead + READ_BYTES);
    n := P.Output.Read((MS.Memory + BytesRead)^, READ_BYTES);
    if n > 0 then
    begin
      Inc(BytesRead, n);
    end else
    begin
      Sleep(100);
    end;
  end;

  // Prozess hat sich beendet, die letzten Daten aus dem stream lesen

  repeat
    MS.SetSize(BytesRead + READ_BYTES);
    n := P.Output.Read((MS.Memory + BytesRead)^, READ_BYTES);
    if n > 0 then
    begin
      Inc(BytesRead, n);
    end

  until n<=0;

  if BytesRead > 0 then
  begin
       MS.SetSize(BytesRead);
       SL.LoadFromStream(MS);

  end else SL.Add('');



  P.Free;
  MS.Free;

  if Length(saveTo) > 0 then begin ShowMessage('save SL to'+saveTo); SL.SaveToFile(saveTo);  end;

  GetProcessResult := SL;
end;





end.

