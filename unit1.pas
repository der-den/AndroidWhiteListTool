unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, CheckLst,
  ComCtrls, ValEdit, ADB_Functions;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button_ADB_Check4Devices: TButton;
    Button_Use_Device: TButton;
    CheckListBox1: TCheckListBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    Memo2: TMemo;
    PageControl1: TPageControl;
    TabSheet_Start: TTabSheet;
    TabSheet_Apps: TTabSheet;
    TabSheet_Ignore: TTabSheet;
    ValueListEditor1: TValueListEditor;
    procedure Button_ADB_Check4DevicesClick(Sender: TObject);
    procedure Button_Use_DeviceClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  search : TADB;

implementation

{$R *.lfm}

{ TForm1 }

// find ./backup -type f -exec md5sum {} \;>> /checksums_backup.md5

// find ./backup -type f -print0 | xargs -0 md5sum > /checksums_backup.md5

procedure TForm1.Button_ADB_Check4DevicesClick(Sender: TObject);
begin
  ComboBox1.Items.Clear;
  search.getDevices();
//  ComboBox1.Items.AddStrings( search.getDevices() );
//   ListBox1.Items.Add('Check devices - Found: '+IntToStr(ComboBox1.Items.Count)+' ');
  if ( ComboBox1.Items.Count = 1 ) then begin ComboBox1.Text := ComboBox1.Items[0]; end;
end;

procedure TForm1.Button_Use_DeviceClick(Sender: TObject);
begin
  // get device id
  if ( ComboBox1.Text <> '' ) then
  begin
    ListBox1.Items.Add(search.extractIDfromDeviceLine(ComboBox1.Text));
    ListBox1.Items.Add(search.extractStatusfromDeviceLine(ComboBox1.Text));
    ListBox1.Items.Add(search.extractDevicefromDeviceLine(ComboBox1.Text));
    ListBox1.Items.Add(search.extractModelfromDeviceLine(ComboBox1.Text));
    ListBox1.Items.Add(search.extractTransportIDfromDeviceLine(ComboBox1.Text));
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  search := TADB.Create(llDebug+llNotice+llInformation+llWarning+llError);
  search.assignLogStringList(ListBox1.Items);
  search.setADBpathAndExe('adb.exe');
  search.getADBversion();
  search.getDevices();
  PageControl1.ActivePage := TabSheet_Start;
end;

end.

