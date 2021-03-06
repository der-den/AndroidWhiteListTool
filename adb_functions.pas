unit ADB_functions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, Dialogs, StdCtrls;

const
     llDebug = 1;
     llNotice = 2;
     llInformation = 4;
     llWarning = 8;
     llError = 16;
     llAll = 31;


type
  TFillLog = procedure(aText: string)  of object;
  { TADB }

  TADB = class(tObject)
    private
      fLogList : TStringList;
      logState : integer;
      extLogList : TStrings;
      ADBready : boolean;
      fFillLog : TFillLog;

      deviceActivated : boolean;
      transportID : String;
      ADBpathAndExe : String;

      function GetProcessResult(filename, parameter:string; saveTo:string=''):TStringList;
      procedure log(s:string; level:integer);

    public

      constructor Create(ll:integer = llDebug+llWarning+llError);
      destructor Destroy(); override;

      property logList : TStringList read fLogList;

      procedure assignLog(l:TStrings);
      procedure assignFillLog(func : TFillLog);
      procedure LogExtern(Text : String);

      procedure setTransportID(tID:String);
      function getTransportID():String;

      procedure setADBpathAndExe(pathAndExe:string);

      function getADBversion():String;

      function startServer():TStringList;
      function killServer() :TStringList;
      function getDevices() :TStringList;

      function extractIDfromDeviceLine(line:string):String;
      function extractStatusfromDeviceLine(line:string):String;
      function extractModelfromDeviceLine(line:string):String;
      function extractDevicefromDeviceLine(line:string):String;
      function extractTransportIDfromDeviceLine(line:string):String;

      function getProp():TStringlist;
      function getMountings():TStringList;
      function getDiskFree():TStringList;
      function checkForInstalledApp(app:string):string;
      function downloadApk(app:string;targetpath:string):String;
      function installApk(apkFile:string):TStringList;

      function ADB_callAndSave(id,workDirAndFile, adbParams: string; forceNoCache: boolean = false):TStringList;
      function ADB_call(command:string):TStringList;

  end;


implementation

constructor TADB.Create(ll:integer);
begin
   inherited Create();
   transportID:='';
   ADBpathAndExe:='';
   deviceActivated:=false;
   ADBready:=false;

   logState:=ll;
   fLogList := TStringlist.Create;
{*
   log('Init ADB class.. Test Debug',llDebug);
   log('Init ADB class.. Test Notice',llNotice);
   log('Init ADB class.. Test Information',llInformation);
   log('Init ADB class.. Test Warning',llWarning);
   log('Init ADB class.. Test Error',llError);
*}
end;

destructor TADB.Destroy();
begin
  freeandnil(fLogList);
  inherited Destroy();
end;

procedure TADB.assignLog(l:TStrings);
begin
   extLogList := l;
end;

procedure TADB.assignFillLog(func: TFillLog);
begin
  fFillLog := func;
end;

procedure TADB.LogExtern(Text: String);
begin
  if Assigned(fFillLog) then
    fFillLog(Text);
end;

procedure TADB.log(s:string; level:integer);
var logIt : boolean = false;
begin
  case level of
     llDebug:       begin if (level and llDebug and logState) <> 0        then logIt := true; end;
     llNotice:      begin if (level and llNotice and logState) <> 0       then logIt := true; end;
     llInformation: begin if (level and llInformation and logState) <> 0  then logIt := true; end;
     llWarning:     begin if (level and llWarning and logState) <> 0      then logIt := true; end;
     llError:       begin if (level and llError and logState) <> 0        then logIt := true; end;
  end;

  if (logIt) then
  begin
     fLogList.Add(s);
     LogExtern(s);

  end;
end;




procedure TADB.setTransportID(tID:String);
var
   t : integer;
begin
   if ( not deviceActivated ) then
   begin
     tid := trim(tID);
     if (TryStrToInt(tID,t)) then
     begin
          transportID:=tID;
          log('transportID set to: '+tID,llNotice);
     end else
     begin
          log('given transportID is not a number: '+tID,llError);
     end;
   end else
   begin
     // do nothing!
     log('Changing transportID with activated device is forbidden! Deactivate first',llWarning);
   end;
end;

function TADB.getTransportID():String;
begin
    Result := Self.TransportID;
end;



function TADB.ADB_call(command:string):TStringList;
var
  r : TStringList;
begin
   r := TStringList.Create();
   if not FileExists(Self.AdbPathAndExe) then
   begin
     r[0] := 'ERROR: adb bin not found';
     Result := r;
     Exit;
   end;
   r := GetProcessResult(adbPathAndExe,command);
   Result := r;
end;


procedure TADB.setADBpathAndExe(pathAndExe:string);
begin
  log('Check ADB: '+pathAndExe,llDebug);
  if( FileExists(pathAndExe) ) then
  begin
     adbPathAndExe:=pathAndExe;
     log('Using ADB: '+pathAndExe,llNotice);
     ADBready := true;
  end else
  begin
    log('Error: ADB binary not found: '+pathAndExe,llError);
    ADBready:=false;
  end;
end;




function TADB.startServer():TStringList;
var
  R : TStringList;
begin
  R := ADB_call('start-server');
 //  if (R.Count = 2) then
  Result := R;
end;

function TADB.killServer():TStringList;
begin
   Result:=ADB_call('kill-server');
end;


function TADB.getADBversion(): String;
var
  R : TStringList;
begin
  if (ADBready) then
  begin
   R := ADB_call('');                  // todo: PRÜFUNG!
   log(R[0]+' '+R[1],llDebug);
   Result := R[0]+' '+R[1];
  end else log('getADBversion: ADB not ready',llDebug);
end;

function TADB.getDevices():TStringList;
var
  R,DevList : TStringList;
  position : integer;
  i,c : integer;
begin
   DevList := TStringList.Create;
   if (ADBready) then
   begin
     R := ADB_call('devices -l');
     position := R.IndexOf('List of devices attached');
     position := position+1;
     c := R.Count - 1 - position;
     if c>0 then for i := position to position+c-1 do DevList.Add(R[i]);
     log('Check devices - Found: '+IntToStr(DevList.Count),llInformation);
   end else log('getDevices: ADB not ready',llDebug);
   Result := DevList;
end;


function TADB.getProp(): TStringlist;
begin
   Result := ADB_call('shell getprop');
end;

function TADB.getMountings():TStringList;
begin
   Result := ADB_call('shell mount');
end;

function TADB.getDiskFree():TStringList;
begin
   Result := ADB_call('shell df');
end;



function TADB.downloadApk(app: string; targetpath: string): String;
var
  f : file;
  a,b : TStringList;

begin
     a := TStringList.Create;
     b := TStringList.Create;
     // get download path
     a:=ADB_call('shell pm list packages -f  | grep "'+app+'"');
     a[0]:=copy(a[0],9,255);
     a[0]:=copy(a[0],1,length(a[0])-length(app)-1);
     b:=ADB_call('pull '+a[0]+' "'+targetPath+'/'+app+'.apk"');
     Result:=b[0];
end;


function TADB.installApk(apkFile:string):TStringList;
var
  a : TStringList;

begin
   if ( FileExists(apkFile) ) then
   begin
      a := TStringList.Create;
      a := ADB_call('install "'+apkFile+'"');
      a.Add(apkFile);
      Result := a;
   end else Result[0] := 'Error - file not found';

end;

function TADB.ADB_callAndSave(id,workDirAndFile, adbParams: string; forceNoCache: boolean = false):TStringList;
var
  SL: TStringList;
begin
   SL := TStringList.Create;
   if FileExists(workDirAndFile) then
   begin
     SL.LoadFromFile(workDirAndFile);
     // ShowMessage('LoadFromFile');
   end
   else
   begin
     SL := getProcessResult(adbPathAndExe,'-s '+id+' '+adbParams);
     SL.SaveToFile(workDirAndFile);
     // ShowMessage('LoadFromADB,savetoFile');
   end;
   Result := SL;
end;

function TADB.checkForInstalledApp(app:string):string;
var
  ar : TStringList;
   i : integer;
begin
   ar := TStringList.Create;
   ar := adb_call('shell dumpsys package '+app+' | grep versionName');
   if( ar.count>-1 ) then
   begin
     i := pos('=', ar[0]);
     Result := copy(ar[0], i+1,255);
   end else Result := '';

end;

function TADB.extractIDfromDeviceLine(line:string):String;
var List : TStrings;
begin
   List := TStringList.Create;
   ExtractStrings([' '], [], PChar(line), List);
   Result :='';
   if(List.Count>1) then Result:=List[0];
end;

function TADB.extractStatusfromDeviceLine(line:string):String;            // is case sensitive ! Un.. un..
var List : TStrings;
begin
   List := TStringList.Create;
   ExtractStrings([' '], [], PChar(line), List);
   Result:='';
   if(List.Count>1) then Result:=List[1];
end;

function TADB.extractModelfromDeviceLine(line:string):String;            // status = device !
var List : TStrings;
begin
   List := TStringList.Create;
   ExtractStrings([' '], [], PChar(line), List);
   Result :='';
   if(List.Count>1) then Result :=List[2];
end;

function TADB.extractDevicefromDeviceLine(line:string):String;              // status = device !
var List : TStrings;
begin
   List := TStringList.Create;
   ExtractStrings([' '], [], PChar(line), List);
   Result :='';
   if(List.Count>1) then Result :=List[3];
end;

function TADB.extractTransportIDfromDeviceLine(line:string):String;      // status = device/un = 5 or 2 !
var List : TStrings;
begin
   List := TStringList.Create;
   ExtractStrings([' '], [], PChar(line), List);
   Result :='';
   if(List.Count>1) then Result :=List[5];
end;

function TADB.GetProcessResult(filename, parameter:string; saveTo:string=''):TStringList;
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
{*     if( pos('nauthorized',R[1]) > 0 ) then
     begin
         DevList.Add('Unauthorized');
     end
     else
     begin *}


end.

