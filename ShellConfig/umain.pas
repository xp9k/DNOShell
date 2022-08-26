unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LCLType, ComCtrls;

type

  PMyProgram = ^TMyProgram;
  TMyProgram = record
    Filename: string;
    Icon: string;
    Name: string;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    btodIcon: TButton;
    btodProgram: TButton;
    btodBrowser: TButton;
    btodBoard: TButton;
    btodBrowserIcon: TButton;
    btodBoardIcon: TButton;
    btodBackground: TButton;
    Button1: TButton;
    btSaveCurrentProgram: TButton;
    btClose: TButton;
    btAdd: TButton;
    btRemove: TButton;
    btSave: TButton;
    btodExplorer: TButton;
    btodExplorerIcon: TButton;
    cd: TColorDialog;
    cbProgramsPos: TComboBox;
    edDomain: TEdit;
    edBrowser: TEdit;
    edBoard: TEdit;
    edBrowserIcon: TEdit;
    edBoardIcon: TEdit;
    edBackground: TEdit;
    edExplorer: TEdit;
    edExplorerIcon: TEdit;
    edFontSize: TEdit;
    edName: TEdit;
    edIcon: TEdit;
    edProgram: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbPrograms: TListBox;
    od: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlColor: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    UpDown1: TUpDown;
    procedure btAddClick(Sender: TObject);
    procedure btodBackgroundClick(Sender: TObject);
    procedure btodBoardClick(Sender: TObject);
    procedure btodBoardIconClick(Sender: TObject);
    procedure btodBrowserClick(Sender: TObject);
    procedure btodBrowserIconClick(Sender: TObject);
    procedure btodExplorerClick(Sender: TObject);
    procedure btodExplorerIconClick(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btodIconClick(Sender: TObject);
    procedure btodProgramClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btSaveCurrentProgramClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure edFontSizeKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure lbProgramsClick(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
  private
    ConfigFile: String;
    Config: TStringList;
    ProgramList: TList;
    function LoadPrograms: boolean;
    procedure ListPrograms;
    procedure ClearConfigPrograms;
  public

  end;

var
  frmMain: TfrmMain;

implementation



{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btodIconClick(Sender: TObject);
begin
  if od.Execute then
    edIcon.Text := od.FileName;

  Image1.Picture.Clear;
  try
    if FileExists(od.FileName) then Image1.Picture.LoadFromFile(od.FileName);
  except
    Application.MessageBox('Ошибка загрузки файла', 'Ошибка', MB_ICONERROR);
  end;
end;

procedure TfrmMain.btAddClick(Sender: TObject);
var
  prog: PMyProgram;
begin
  New(prog);
  prog^.Name:='Новая программа';
  prog^.Filename:='';
  prog^.Icon:='';
  ProgramList.Add(prog);
  ListPrograms;
end;

procedure TfrmMain.btodBackgroundClick(Sender: TObject);
begin
  if OD.Execute then edBackground.Text:=od.FileName;
end;

procedure TfrmMain.btodBoardClick(Sender: TObject);
begin
  if od.Execute then edBoard.Text:=od.FileName;
end;

procedure TfrmMain.btodBoardIconClick(Sender: TObject);
begin
  if OD.Execute then edBoardIcon.Text:=od.FileName;
end;

procedure TfrmMain.btodBrowserClick(Sender: TObject);
begin
  if od.Execute then edBrowser.Text:=od.FileName;
end;

procedure TfrmMain.btodBrowserIconClick(Sender: TObject);
begin
  if od.Execute then edBrowserIcon.Text:=od.FileName;
end;

procedure TfrmMain.btodExplorerClick(Sender: TObject);
begin
  if od.Execute then
    edExplorer.Text := od.FileName;
end;

procedure TfrmMain.btodExplorerIconClick(Sender: TObject);
begin
  if od.Execute then
    edExplorerIcon.Text := od.FileName;
end;

procedure TfrmMain.btRemoveClick(Sender: TObject);
begin
  if lbPrograms.ItemIndex < 0 then exit;
  ProgramList.Delete(lbPrograms.ItemIndex);
  ListPrograms;
  lbProgramsClick(self);
  edName.Text:='';
  edIcon.Text:='';
  edProgram.Text:='';
end;

procedure TfrmMain.btSaveClick(Sender: TObject);
var
  i: integer;
  prog: PMyProgram;
begin
  Config.Values['domain'] := edDomain.Text;
  Config.Values['bg'] := edBackground.Text;
  if pnlColor.Color <> clDefault then
    Config.Values['bgcolor'] := ColorToString(pnlColor.Color)
  else
    Config.Values['bgcolor'] := '';

  Config.Values['explorer'] := edExplorer.Text;
  Config.Values['explorericon'] := edExplorerIcon.Text;

  Config.Values['internet'] := edBrowser.Text;
  Config.Values['interneticon'] := edBrowserIcon.Text;

  Config.Values['board'] := edBoard.Text;
  Config.Values['boardicon'] := edBoardIcon.Text;

  case cbProgramsPos.ItemIndex of
    0: Config.Values['programspos'] := 'bottom';
    1: Config.Values['programspos'] := 'left';
    2: Config.Values['programspos'] := 'right';
  end;

  Config.Values['fontsize'] := edFontSize.Text;

  ClearConfigPrograms;
  for i := 1 to 20 do
    begin
      if i > ProgramList.Count then
        begin
         Config.Values['program' + IntToStr(i)] := '';
         Config.Values['icon' + IntToStr(i)] := '';
         Config.Values['exec' + IntToStr(i)] := '';
        end
      else
        begin
          prog := PMyProgram(ProgramList[i - 1]);
          Config.Values['program' + IntToStr(i)] := prog^.Name;
          Config.Values['icon' + IntToStr(i)] := prog^.Icon;
          Config.Values['exec' + IntToStr(i)] := prog^.Filename;
        end;
      //Config.Add('');
      //Config.AddPair('program' + IntToStr(i + 1), prog^.Name);
      //Config.AddPair('icon' + IntToStr(i + 1), prog^.Icon);
      //Config.AddPair('exec' + IntToStr(i + 1), prog^.Filename);
    end;
  Config.SaveToFile(ConfigFile);
end;

procedure TfrmMain.btodProgramClick(Sender: TObject);
begin
  if od.Execute then
    edProgram.Text := od.FileName;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  if cd.Execute then pnlColor.Color := cd.Color;
end;

procedure TfrmMain.btSaveCurrentProgramClick(Sender: TObject);
var
  prog: PMyProgram;
  index: integer;
begin
  if lbPrograms.ItemIndex < 0 then exit;

  prog := PMyProgram(ProgramList[lbPrograms.ItemIndex]);
  prog^.Filename :=  edProgram.Text;
  prog^.Icon := edIcon.Text;
  prog^.Name := edName.Text;

  Index := lbPrograms.ItemIndex;
  ListPrograms;
  lbPrograms.ItemIndex:=index;
end;

procedure TfrmMain.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.edFontSizeKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', #8]) then key := #0;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  {$IFDEF WINDOWS}
    ConfigFile := SysUtils.GetEnvironmentVariable('appdata') + '\' + 'dnoshell.conf';
  {$ENDIF}
  {$IFDEF LINUX}
    ConfigFile := './dnoshell.conf';
  {$ENDIF}

  Config := TStringList.Create;
  if FileExists(ConfigFile) then
    Config.LoadFromFile(ConfigFile);

  edDomain.Text:=Config.Values['domain'];
  edExplorer.Text := Config.Values['explorer'];
  edExplorerIcon.Text := Config.Values['explorericon'];
  edBrowser.Text:=Config.Values['internet'];
  edBrowserIcon.Text:=Config.Values['interneticon'];
  edBoard.Text:=Config.Values['board'];
  edBoardIcon.Text:=Config.Values['boardicon'];

  if (Config.Values['programspos'] = '') or (Config.Values['programspos'] = 'bottom') then
    cbProgramsPos.ItemIndex:=0
  else if (Config.Values['programspos'] = 'left') then
    cbProgramsPos.ItemIndex:=1
  else if (Config.Values['programspos'] = 'right') then
    cbProgramsPos.ItemIndex:=2;

  edFontSize.Text := Config.Values['fontsize'];

  edBackground.Text := Config.Values['bg'];
  if Config.Values['bgcolor'] <> '' then pnlColor.Color := StringToColor(Config.Values['bgcolor']);

  ProgramList := TList.Create;
  LoadPrograms;
  ListPrograms;
end;

procedure TfrmMain.lbProgramsClick(Sender: TObject);
var
  prog: PMyProgram;
begin
  if lbPrograms.ItemIndex < 0 then exit;

  prog := ProgramList[lbPrograms.ItemIndex];
  edProgram.Text := prog^.Filename;
  edIcon.Text := prog^.Icon;
  edName.Text := prog^.Name;
  Image1.Picture.Clear;
  if FileExists(prog^.Icon) then Image1.Picture.LoadFromFile(prog^.Icon);
end;

procedure TfrmMain.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin

end;

function TfrmMain.LoadPrograms: boolean;
var
  i: integer;
  Prg: PMyProgram;
begin
  for i := 1 to 10 do
    begin
      if Config.Values['program' + IntToStr(i)] <> '' then
        begin
         new(Prg);
         Prg^.Filename := Config.Values['exec' + IntToStr(i)];
         Prg^.Icon := Config.Values['icon' + IntToStr(i)];
         Prg^.Name := Config.Values['program' + IntToStr(i)];
         ProgramList.Add(Prg);
        end;
    end;
end;

procedure TfrmMain.ListPrograms;
var
  i: integer;
begin
  lbPrograms.Clear;
  for i := 0 to ProgramList.Count - 1 do
    begin
      lbPrograms.Items.Add(PMyProgram(ProgramList[i])^.Name);
    end;
end;

procedure TfrmMain.ClearConfigPrograms;
var
  i, j, index: integer;
begin
  for i := Config.Count - 1 downto 0 do;
    for j := 20 downto ProgramList.Count do
      begin
        index := Config.IndexOfName('program' + IntToStr(j));
        if index > 0 then Config.Delete(index);
        index := Config.IndexOfName('icon' + IntToStr(j));
        if index > 0 then Config.Delete(index);
        index := Config.IndexOfName('exec' + IntToStr(j));
        if index > 0 then Config.Delete(index);
      end;
end;

end.

