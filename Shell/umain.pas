unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Process, LCLType, ldapsend, StrUtils, fphttpclient
  {$IFDEF LINUX}, Unix
  {$ENDIF}
  ;

type

  PMyProgram = ^TMyProgram;
  TMyProgram = record
    Filename: string;
    Icon: string;
    Name: string;
  end;

  PMyProcess = ^TMyProcess;
  TMyProcess = record
    Process: TProcess;
    Icon: TBitmap;
  end;

  { TImageButton }

  TImageButton = class(TImage)
    private
      FLabel: Tlabel;
      FOldTop: integer;
      FOldLeft: integer;
      FOldHeight: integer;
      FOldWidth: integer;
      FOnResize: TNotifyEvent;
      FParent: TWinControl;
      FMouseLeft: boolean;
      function GetCaption: TCaption;
      procedure SetCaption(const AValue: TCaption);
      function GetFont: TFont;
      procedure SetFont(const AValue: TFont);
    protected
      procedure Resize; override;
      procedure MouseEnter(Sender: TObject);
      procedure MouseLeave(Sender: TObject);
      procedure MouseDown(Sender: TObject; Button: TMouseButton;
                Shift: TShiftState; X, Y: Integer);
      procedure MouseUp(Sender: TObject; Button: TMouseButton;
                Shift: TShiftState; X, Y: Integer);
      procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure SetParent(NewParent: TWinControl); override;
      procedure LabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure LabelMouseLeave(Sender: TObject);
    published
      property OnResize;
    public
      property Parent: TWinControl read FParent write SetParent;
      property Caption: TCaption read GetCaption write SetCaption;
      property Font: TFont read GetFont write SetFont;
      procedure OnLabelClick(Sender: TObject);
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    btPoweroff: TImage;
    edLogin: TEdit;
    edPassword: TEdit;
    Image1: TImage;
    Image0: TImage;
    btImage1: TImage;
    btImage2: TImage;
    btImage3: TImage;
    btReboot: TImage;
    btImagePrograms: TImage;
    btLogin: TImage;
    imgUser: TImage;
    imgPassword: TImage;
    ImageList1: TImageList;
    lbLogin: TLabel;
    lbTime: TLabel;
    lbTime1: TLabel;
    pnlPrograms: TPanel;
    pnlFiles: TPanel;
    pnlMainForm: TPanel;
    pnlLogin: TPanel;
    Timer1: TTimer;
    procedure bGuestClick(Sender: TObject);
    procedure bLoginClick(Sender: TObject);
    procedure btImage1Click(Sender: TObject);
    procedure btImage1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btImage1MouseEnter(Sender: TObject);
    procedure btImage1MouseLeave(Sender: TObject);
    procedure btImage1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure btImage1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btImage2Click(Sender: TObject);
    procedure btImage3Click(Sender: TObject);
    procedure btImageProgramsClick(Sender: TObject);
    procedure btLoginClick(Sender: TObject);
    procedure btLogoutClick(Sender: TObject);
    procedure btPoweroffClick(Sender: TObject);
    procedure btRebootClick(Sender: TObject);
    procedure edPasswordKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lbLoginMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbLoginMouseLeave(Sender: TObject);
    procedure lbLoginMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbLoginMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlLoginResize(Sender: TObject);
    procedure pnlMainFormResize(Sender: TObject);
    procedure pnlProgramsResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ProgramButtonClick(Sender: TObject);
    procedure ProgramCloseButtonClick(Sender: TObject);
  private
    CanClose: boolean;
    ProgramList: TList;
    ConfigFile: string;
    UserFolder: String ;
    Config: TStringList;
    tmpWidth: integer;
    tmpHeight: integer;
    tmpTop: integer;
    tmpLeft: integer;
    ProcessList: TList;
    procedure LoadDefaults;
    procedure ResizeLoginForm(pnlTop, pnlLeft, pnlWidth, pnlHeight: integer);
    function CreateUser(user: string): boolean;
    function LoadPrograms: boolean;
    procedure ArrangePrograms;
    procedure LocalizeDate;
    procedure CheckForProcessRun;
    function IsProcessExecuted(ProcessName: string): boolean;
    function GetProcessListIndexByName(ProcessName: string): Integer;
    procedure TerminateAndClearProcesses;
  public
    imgbtLogin: TImageButton;
    btExplorer: TImageButton;
    btBrowser: TImageButton;
    btBoard: TImageButton;
    btLogout: TImageButton;
    btGuest: TImageButton;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TImageButton }

procedure TImageButton.SetParent(NewParent: TWinControl);
begin
 inherited SetParent(NewParent);
 FLabel.Parent := NewParent;
end;

procedure TImageButton.LabelMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
 FMouseLeft := false;
 self.MouseMove(Sender, Shift, X, Y);
end;

procedure TImageButton.LabelMouseLeave(Sender: TObject);
begin
 FMouseLeft := false;
 Self.MouseLeave(Sender);
end;

function TImageButton.GetCaption: TCaption;
begin
  Result := FLabel.Caption;
end;

procedure TImageButton.SetCaption(const AValue: TCaption);
begin
  FLabel.Caption := AValue;
end;

function TImageButton.GetFont: TFont;
begin
  Result := FLabel.Font;
end;

procedure TImageButton.SetFont(const AValue: TFont);
begin
  FLabel.Font := AValue;
end;

procedure TImageButton.OnLabelClick(Sender: TObject);
begin
  self.Click;
end;

procedure TImageButton.Resize;
begin
  inherited Resize;
  FLabel.Left := Self.Left + ((Self.Width - FLabel.Width) div 2);
  FLabel.Top := Self.Top + ((Self.Height - FLabel.Height) div 2);
end;

procedure TImageButton.MouseEnter(Sender: TObject);
begin
  if FMouseLeft then
    begin
      FOldWidth := Self.Width;
      FOldHeight := Self.Height;
      FOldTop := Self.Top;
      FOldLeft := Self.Left;
    end;
   FMouseLeft := False;
end;

procedure TImageButton.MouseLeave(Sender: TObject);
begin
  if not FMouseLeft then
    begin
      Self.Width := FOldWidth;
      Self.Height := FOldHeight;
      Self.Top := FOldTop;
      Self.Left := FOldLeft;
    end;
  FMouseLeft := True;
end;

procedure TImageButton.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not FMouseLeft then
    begin
      Self.Width := FOldWidth;
      Self.Height := FOldHeight;
      Self.Top := FOldTop;
      Self.Left := FOldLeft;
    end;
end;

procedure TImageButton.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not FMouseLeft then
    begin
      Self.Width := FOldWidth + FOldWidth div 10;
      Self.Height := FOldHeight + FOldHeight div 10;
      Self.Top := FOldTop - ((FOldHeight div 10) div 2);
      Self.Left := FOldLeft - ((Self.Width div 10) div 2);
    end;
end;

procedure TImageButton.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FOldWidth = 0 then FOldWidth := Self.Width;
  if FOldHeight = 0 then FOldHeight := Self.Height;
  if FOldTop = 0 then FOldTop := Self.Top;
  if FOldLeft = 0 then FOldLeft := Self.Left;
  if not FMouseLeft then
    begin
      Self.Width := FOldWidth + FOldWidth div 10;
      Self.Height := FOldHeight + FOldHeight div 10;
      Self.Top := FOldTop - ((FOldHeight div 10) div 2);
      Self.Left := FOldLeft - ((Self.Width div 10) div 2);
    end;
end;

constructor TImageButton.Create(AOwner: TComponent);
begin
  FLabel := TLabel.Create(AOwner);
  FLabel.BringToFront;
  inherited Create(AOwner);
  OnMouseEnter := @MouseEnter;
  OnMouseLeave := @MouseLeave;
  OnMouseMove := @MouseMove;
  OnMouseDown := @MouseDown;
  OnMouseUp := @MouseUp;

  FLabel.OnMouseMove := @LabelMouseMove;
  FLabel.OnMouseLeave := @LabelMouseLeave;
  FLabel.OnMouseDown := @MouseDown;
  FLabel.OnMouseUp := @MouseUp;
  FLabel.OnClick := @OnLabelClick;

  FMouseLeft := True;

  Stretch := True;
  Transparent := True;
end;

destructor TImageButton.Destroy;
begin
//  FLabel.Destroy;
  inherited Destroy;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  FontSize: Byte;
begin
  CanClose := false;

  {$IFDEF WINDOWS}
  ConfigFile := SysUtils.GetEnvironmentVariable('appdata') + '\' + 'dnoshell.conf';
  {$ENDIF}

  {$IFDEF LINUX}
  ConfigFile := './dnoshell.conf';
  {$ENDIF}

  Config := TStringList.Create;
  if FileExists(ConfigFile) then
    Config.LoadFromFile(ConfigFile)
  else
    begin
      LoadDefaults;
      Config.SaveToFile(ConfigFile);
    end;

  Width := Screen.Width;
  Height := Screen.Height;
  Top := 0;
  Left := 0;

  //Кнопка Файлового менеджера

  btExplorer := TImageButton.Create(pnlMainForm);
  btExplorer.Parent := pnlMainForm;
  btExplorer.OnClick := @btImage1Click;
  btExplorer.Name := 'imageBtn1';
  if FileExists(Config.Values['explorericon']) then
    btExplorer.Picture.LoadFromFile(Config.Values['explorericon'])
  else
    ImageList1.GetBitmap(1, btExplorer.Picture.Bitmap);

  //Кнопка Интернет браузера

  btBrowser:= TImageButton.Create(pnlMainForm);
  btBrowser.Parent := pnlMainForm;
  btBrowser.OnClick := @btImage2Click;
  btBrowser.Name := 'imageBtn2';
  if FileExists(Config.Values['explorericon']) then
    btBrowser.Picture.LoadFromFile(Config.Values['interneticon'])
  else
    ImageList1.GetBitmap(2, btBrowser.Picture.Bitmap);

  //Кнопка Доски

  btBoard:= TImageButton.Create(pnlMainForm);
  btBoard.Parent := pnlMainForm;
  btBoard.OnClick := @btImage3Click;
  btBoard.Name := 'imageBtn3';
  if FileExists(Config.Values['explorericon']) then
    btBoard.Picture.LoadFromFile(Config.Values['boardicon'])
  else
    ImageList1.GetBitmap(3, btBoard.Picture.Bitmap);

  // Кнопка логина

  imgbtLogin := TImageButton.Create(pnlLogin);
  imgbtLogin.Parent := pnlLogin;
  imgbtLogin.Name := 'imgbtnLogin';
  imgbtLogin.Caption := 'Войти';
  imgbtLogin.Picture.Bitmap := btLogin.Picture.Bitmap;
  imgbtLogin.Font.Color:= clWhite;
  imgbtLogin.Font.Style:= [fsBold];
  imgbtLogin.Font.Size:=20;

  imgbtLogin.OnClick := @btLoginClick;

  //Кнопка Входа Гостя

  btGuest:= TImageButton.Create(pnlLogin);
  btGuest.Parent := pnlLogin;
  btGuest.OnClick := @bGuestClick;
  btGuest.Name := 'btGuest';
  btGuest.Picture.Bitmap := btLogin.Picture.Bitmap;
  btGuest.Caption := 'Гость';
  btGuest.Font.Color:= clWhite;

  //Кнопка Выхода из учетной записи

  btLogout:= TImageButton.Create(pnlMainForm);
  btLogout.Parent := pnlMainForm;
  btLogout.OnClick := @btLogoutClick;
  btLogout.Name := 'btLogout';
  btLogout.Picture.Bitmap := btLogin.Picture.Bitmap;
  btLogout.Caption := 'Выход';
  btLogout.Font.Color:= clWhite;

  LocalizeDate;

  if FileExists(Config.Values['bg']) then
    begin
      image0.Picture.LoadFromFile(Config.Values['bg']);
      image1.Picture.LoadFromFile(Config.Values['bg']);
    end;

//  if Config.Values['bgcolor'] <> '' then pnlLoginForm.Color := StringToColor(Config.Values['bgcolor']);

  //if FileExists(Config.Values['explorericon']) then
  //  btImage1.Picture.LoadFromFile(Config.Values['explorericon'])
  //else
  //  ImageList1.GetBitmap(1, btImage1.Picture.Bitmap);
  //
  //if FileExists(Config.Values['interneticon']) then
  //  btImage2.Picture.LoadFromFile(Config.Values['interneticon'])
  //else
  //  ImageList1.GetBitmap(2, btImage2.Picture.Bitmap);
  //
  //if FileExists(Config.Values['boardicon']) then
  //  btImage3.Picture.LoadFromFile(Config.Values['boardicon'])
  //else
  //  ImageList1.GetBitmap(3, btImage3.Picture.Bitmap);

  ImageList1.GetBitmap(6, btReboot.Picture.Bitmap);
  ImageList1.GetBitmap(7, btPoweroff.Picture.Bitmap);

  ImageList1.GetBitmap(4, imgUser.Picture.Bitmap);
  ImageList1.GetBitmap(5, imgPassword.Picture.Bitmap);

  ProcessList := TList.Create;
  ProgramList := TList.Create;

  LoadPrograms;
  ArrangePrograms;

  pnlPrograms.Height:=0;
  pnlPrograms.Visible:=false;
  edLogin.Text:='';
  edPassword.Text:='';

  FontSize := 0;

  if Config.Values['fontsize'] <> '' then
    FontSize := StrToIntDef(Config.Values['fontsize'], 0)
  else
  if Height >= 2160 then
    FontSize := 20
  else
  if Height >= 1440 then
    FontSize := 18
  else
  if Height >= 1080 then
    FontSize := 14;

  edLogin.Font.Size := FontSize;
  edPassword.Font.Size := FontSize;
  imgbtLogin.Font.Size := FontSize;

  pnlLogin.BringToFront;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  p: TProcess;
begin
  if (ssCtrl in Shift) and (ssShift in Shift) then
    begin
      if (Key = VK_F2) then
       begin
         CanClose:=true;
         frmMain.Close;
       end;
      if (Key = VK_F3) then
       begin
        try
          p := TProcess.Create(nil);
          p.InheritHandles := False;
          p.Options := [];
          p.ShowWindow := swoShow;
          p.Executable := Application.ExeName;
          p.Parameters.Add('-c');
          p.Parameters.Add(Application.ExeName);
          p.Execute;
        finally
          p.free;
        end;
         CanClose:=true;
         frmMain.Close;
       end;
    end;
end;

procedure TfrmMain.lbLoginMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  btImage1MouseDown(btLogin, Button, Shift, X, Y);
end;

procedure TfrmMain.lbLoginMouseLeave(Sender: TObject);
begin
  btImage1MouseLeave(btLogin);
end;

procedure TfrmMain.lbLoginMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  btImage1MouseMove(btLogin, Shift, X, Y);
end;

procedure TfrmMain.lbLoginMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  btImage1MouseUp(btLogin, Button, Shift, X, Y);
end;

procedure TfrmMain.pnlLoginResize(Sender: TObject);
var
  bleft, bwidth, bHeight, bTop, allwidth: integer;
begin
  bwidth := self.Width div 6;
  bHeight := self.Height div 5;
  bTop := self.Height div 3;
  allwidth := bwidth;
  bleft := (pnlMainForm.Width div 2) - (allwidth div 2);

  lbTime.Width := pnlLogin.Width;

  btPoweroff.Width := self.Width div 50;
  btPoweroff.Height := btPoweroff.Width;
  btPoweroff.Top := Self.Height - btPoweroff.Height - btPoweroff.Height div 2;
  btPoweroff.Left := btPoweroff.Width div 2;

  btReboot.Width := self.Width div 50;
  btReboot.Height := btReboot.Width;
  btReboot.Top := btPoweroff.Top - btReboot.Height - btReboot.Height div 2;
  btReboot.Left := btReboot.Width div 2;

  ResizeLoginForm(bTop, bleft, bwidth, bHeight);
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if not CanClose then
     CloseAction := caNone
  else
    begin
      TerminateAndClearProcesses;
      Config.Free;
      ProgramList.Free;
      ProcessList.Free;
      CloseAction := caFree;
    end;
end;

procedure TfrmMain.bGuestClick(Sender: TObject);
begin
  pnlMainForm.BringToFront;
  {$IFDEF WINDOWS}
  UserFolder := SysUtils.GetEnvironmentVariable('appdata') + '\DnoShell\Users\Guest';
  {$ENDIF}
  {$IFDEF LINUX}
  UserFolder := './DnoShell/Users/Guest';
  {$ENDIF}

  if not DirectoryExists(UserFolder) then
  begin
   CreateUser('Guest');
  end;
end;

procedure TfrmMain.bLoginClick(Sender: TObject);
begin
end;

procedure TfrmMain.btImage1Click(Sender: TObject);
var
  p: TProcess;
  proc: PMyProcess;
begin
  {$IFDEF WINDOWS}
  ExecuteProcess(Config.Values['explorer'], UserFolder + '\files');
  {$ENDIF}
  {$IFDEF LINUX}
  try
   p := TProcess.Create(nil);
   p.ShowWindow := swoShow;
   p.Executable := '/bin/bash';
   p.Parameters.Add('-c');
   p.Parameters.Add(Config.Values['explorer'] + ' ' + UserFolder + '/files');
   p.Execute;

   New(proc);
   proc^.Process := p;

   ProcessList.Add(proc);
  finally
//    p.free;
  end;
//  RunCommand('/bin/bash',['-c', 'dolphin ' + UserFolder + '/files'], s);
  {$ENDIF}
end;

procedure TfrmMain.btImage1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  with Sender as TImage do
    begin
      Width := tmpWidth;
      Height := tmpHeight;
      Top := tmpTop;
      Left := tmpLeft;
    end;
end;

procedure TfrmMain.btImage1MouseEnter(Sender: TObject);
begin
  with Sender as TImage do
    begin
      tmpWidth := Width;
      tmpHeight := Height;
      tmpTop := Top;
      tmpLeft := Left;
    end;
end;

procedure TfrmMain.btImage1MouseLeave(Sender: TObject);
begin
  with Sender as TImage do
    begin
      Width := tmpWidth;
      Height := tmpHeight;
      Top := tmpTop;
      Left := tmpLeft;
    end;
end;

procedure TfrmMain.btImage1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  with Sender as TImage do
    begin
      Width := tmpWidth + tmpWidth div 10;
      Height := tmpHeight + tmpHeight div 10;
      Top := tmpTop - ((tmpHeight div 10) div 2);
      Left := tmpLeft - ((tmpWidth div 10) div 2);
    end;
end;

procedure TfrmMain.btImage1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  with Sender as TImage do
    begin
      Width := tmpWidth + tmpWidth div 10;
      Height := tmpHeight + tmpHeight div 10;
      Top := tmpTop - ((tmpHeight div 10) div 2);
      Left := tmpLeft - ((tmpWidth div 10) div 2);
    end;
end;


procedure TfrmMain.btImage2Click(Sender: TObject);
var
  profile, s: string;
  p: TProcess;
  proc: PMyProcess;
begin
  {$IFDEF WINDOWS}
    profile := '--user-data-dir="' + UserFolder + '\.profile"';
    p := TProcess.Create(nil);
    p.ShowWindow := swoShow;
    p.Executable := Config.Values['internet'];
    p.Parameters.Add(profile);
    p.Execute;

    New(proc);
    proc^.Process := p;

    ProcessList.Add(proc);
  {$ENDIF}
  {$IFDEF LINUX}
  try
   profile := '--user-data-dir=' + UserFolder + '/.profile';
   p := TProcess.Create(nil);
   p.ShowWindow := swoShow;
   p.Executable := '/bin/bash';
   p.Parameters.Add('-c');
   p.Parameters.Add(Config.Values['internet'] + ' ' + profile);
   p.Parameters.Add(profile);
   p.Execute;

   New(proc);
   proc^.Process := p;

   ProcessList.Add(proc);
  finally
 //   p.free;
  end;
  //RunCommand('/bin/bash',['-c', Config.Values['internet'] + ' ' + profile], s);
  {$ENDIF}
end;

procedure TfrmMain.btImage3Click(Sender: TObject);
var
  p: TProcess;
  proc: PMyProcess;
begin
  {$IFDEF WINDOWS}
    p := TProcess.Create(nil);
    p.ShowWindow := swoShow;
    p.Executable := Config.Values['board'];
    p.Execute;

    New(proc);
    proc^.Process := p;

    ProcessList.Add(proc);
  {$ENDIF}
  {$IFDEF LINUX}
  try
   p := TProcess.Create(nil);
   p.ShowWindow := swoShow;
   p.Executable := '/bin/bash';
   p.Parameters.Add('-c');
   p.Parameters.Add(Config.Values['board']);
   p.Execute;

   New(proc);
   proc^.Process := p;

   ProcessList.Add(proc);
  finally
//    p.free;
  end;
  {$ENDIF}
end;

procedure TfrmMain.btImageProgramsClick(Sender: TObject);
begin
  CheckForProcessRun;
  pnlProgramsResize(self);
  pnlPrograms.Visible:= not pnlPrograms.Visible
end;

procedure TfrmMain.btLoginClick(Sender: TObject);
var
  ldap: TLDAPsend;
  SearchAttributes: TStringList;
begin
  if (edLogin.Text = '') or (edPassword.Text = '') then exit;

  SearchAttributes := TStringList.Create;
  ldap:= TLDAPsend.Create;
  SearchAttributes.Add('*');
  ldap.TargetHost := Config.Values['domain'];
  ldap.UserName := edLogin.Text + '@' + Config.Values['domain'];
  ldap.Password := edPassword.Text;
  ldap.SearchSizeLimit:=0;
  ldap.SearchPageSize:=0;

  ldap.Login;
  if not ldap.Bind then
    begin
      Application.MessageBox(PChar('Ошибка входа для пользователя ' + ldap.UserName), 'Ошибка', MB_ICONERROR);
    end
  else
    begin
      pnlMainForm.BringToFront;
      {$IFDEF WINDOWS}
      UserFolder := SysUtils.GetEnvironmentVariable('appdata') + '\DnoShell\Users\' + edLogin.Text;
      {$ENDIF}
      {$IFDEF LINUX}
      UserFolder := './DnoShell/Users/' + edLogin.Text;
      {$ENDIF}
      if not DirectoryExists(UserFolder) then
      begin
       CreateUser(edLogin.Text);
      end;
    end;
  ldap.Logout;
  ldap.Free;
  SearchAttributes.Free;
end;

procedure TfrmMain.btLogoutClick(Sender: TObject);
var
  i: integer;
begin
  edLogin.Text:='';
  edPassword.Text:='';
  pnlLogin.BringToFront;

  TerminateAndClearProcesses;

  for i := 0 to ProgramList.Count - 1 do
    begin
      TImage(pnlPrograms.FindSubComponent('btProgramClose' + IntToStr(i + 1))).Visible := false;
    end;
  pnlMainFormResize(self);
  pnlPrograms.Visible := False;
end;

procedure TfrmMain.btPoweroffClick(Sender: TObject);
var
  s: string;
begin
  if Application.MessageBox('Вы действительно хотите выключить доску?', 'Внимание', MB_ICONQUESTION + MB_YESNO) = ID_YES then
     RunCommand('/bin/bash',['-c', '/sbin/shutdown now'], s);
end;

procedure TfrmMain.btRebootClick(Sender: TObject);
var
  s: string;
begin
  if Application.MessageBox('Вы действительно хотите перезагрузить доску?', 'Внимание', MB_ICONQUESTION + MB_YESNO) = ID_YES then
     RunCommand('/bin/bash',['-c', '/sbin/shutdown -r now'], s);
end;

procedure TfrmMain.edPasswordKeyPress(Sender: TObject; var Key: char);
begin
  if key = #13 then btLoginClick(self);
end;

procedure TfrmMain.pnlMainFormResize(Sender: TObject);
var
  i, bleft, bwidth, bTop, allwidth, gap: integer;
  img: TImage;
begin
  bwidth := self.Width div 8;
  bTop := self.Height div 5;
  gap := bwidth div 4;
  allwidth := bwidth * 3 + gap * 2;
  for i := 0 to 2 do
    begin
      img := TImage(pnlMainForm.FindChildControl('imageBtn' + IntToStr(i + 1)));
      bleft := (pnlMainForm.Width div 2) - (allwidth div 2) + i * bwidth + i * gap;
      img.Left := bleft;
      img.Height := bwidth;
      img.Width := bwidth;
      img.Top := bTop;
    end;

  btImagePrograms.Width := self.Width div 30;
  btImagePrograms.Height := btImagePrograms.Width;
  btImagePrograms.Left := self.Width div 2 - btImagePrograms.Width div 2;
  btImagePrograms.Top := bTop + bwidth + gap;

  btLogout.Width := self.Width div 30;
  btLogout.Height := self.Height div 30;
  btLogout.Top := self.Height - btLogout.Height - btLogout.Height div 2;
  btLogout.Left := self.Width div 2 - btLogout.Width div 2;

  if (Config.Values['programspos'] = '') or (Config.Values['programspos'] = 'bottom') then
    begin
     pnlPrograms.top := btImagePrograms.Top + btImagePrograms.Height + self.Height div 50;
     pnlPrograms.Height := Self.Height div 15;
     pnlPrograms.Width := Self.Width;
    end
  else if (Config.Values['programspos'] = 'left') then
    begin
      pnlPrograms.Width := self.Width div 25;
      pnlPrograms.Top := 0;
      pnlPrograms.Height := Self.Height;
      pnlPrograms.Left := pnlPrograms.Width;
    end
  else
    begin
      pnlPrograms.Width := self.Width div 25;
      pnlPrograms.Top := 0;
      pnlPrograms.Height := Self.Height;
      pnlPrograms.Left := self.Width - pnlPrograms.Width*2;
    end;

  lbTime1.Width:=self.width;
end;

procedure TfrmMain.pnlProgramsResize(Sender: TObject);
var
  i, bleft, bwidth, bTop, allwidth, allTop, gap: integer;
  img, imgClose: TImage;
  lab: TLabel;
begin

  if (Config.Values['programspos'] = '') or (Config.Values['programspos'] = 'bottom') then
    begin
      bwidth := pnlPrograms.Height div 5 * 3;
      bTop := pnlPrograms.Height div 5;
      gap := bwidth div 2;
      allwidth := bwidth * ProgramList.Count + gap * (ProgramList.Count - 1);

      for i := 0 to ProgramList.Count - 1 do
        begin
          img := TImage(pnlPrograms.FindSubComponent('btProgram' + IntToStr(i + 1)));
          bleft := (pnlMainForm.Width div 2) - (allwidth div 2) + i * bwidth + i * gap;
          img.Left := bleft;
          img.Height := bwidth;
          img.Width := bwidth;
          img.Top := bTop;

          imgClose := TImage(pnlPrograms.FindSubComponent('btProgramClose' + IntToStr(i + 1)));
          imgClose.Width := img.Width div 2;
          imgClose.Height := imgClose.Width;
          imgClose.Left := (img.Left + img.Width) - (imgClose.Width div 3) * 2;
          imgClose.Top := img.Top - (imgClose.Height div 2);

          lab := TLabel(pnlPrograms.FindSubComponent('lbProgram' + IntToStr(i + 1)));
          lab.Left := bleft;
          lab.Top := img.Top + img.Height;
          lab.left := (img.Left + img.Width div 2) - lab.Width div 2;
        end;
    end
  else
    begin
      bwidth := pnlPrograms.Width div 5 * 3;
      gap := bwidth div 2;
      bleft := (pnlPrograms.Width div 2) - (bwidth div 2);
      allTop := bwidth * ProgramList.Count + gap * (ProgramList.Count - 1);

      for i := 0 to ProgramList.Count - 1 do
        begin
          img := TImage(pnlPrograms.FindSubComponent('btProgram' + IntToStr(i + 1)));
          bleft := (pnlPrograms.Width div 2) - (bwidth div 2);
          bTop := (pnlMainForm.Height div 2) - (allTop div 2) + i * bwidth + i * gap;
          img.Left := bleft;
          img.Height := bwidth;
          img.Width := bwidth;
          img.Top := bTop;

          imgClose := TImage(pnlPrograms.FindSubComponent('btProgramClose' + IntToStr(i + 1)));
          imgClose.Width := img.Width div 2;
          imgClose.Height := imgClose.Width;
          imgClose.Left := (img.Left + img.Width) - (imgClose.Width div 3) * 2;
          imgClose.Top := img.Top - (imgClose.Height div 2);

          lab := TLabel(pnlPrograms.FindSubComponent('lbProgram' + IntToStr(i + 1)));
          lab.Left := bleft;
          lab.Top := img.Top + img.Height;
          lab.left := (img.Left + img.Width div 2) - lab.Width div 2;
        end;
    end;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  lbTime.Caption := FormatDateTime('d mmmm yyyyy hh:nn:ss (dddd)', Now);
  lbTime1.Caption := lbTime.Caption;
end;

procedure TfrmMain.ProgramButtonClick(Sender: TObject);
var
  s: string;
  p: TProcess;
  proc: PMyProcess;
  ProgramName: string;
  imgClose: TImage;
begin
  with sender as TImage do
    begin

       ProgramName := PMyProgram(ProgramList[Tag])^.Filename;
       if IsProcessExecuted(ProgramName) then
         begin

         end
       else
         begin
           try
            p := TProcess.Create(nil);
            p.ShowWindow := swoShow;
            {$IFDEF LINUX}
            p.Executable := '/bin/bash';
            p.Parameters.Add('-c');
            p.Parameters.Add(ProgramName);
            {$ENDIF}
            {$IFDEF WINDOWS}
            p.Executable := ProgramName;
            {$ENDIF}
            p.Execute;

            New(proc);
            proc^.Icon := Picture.Bitmap;
            proc^.Process := p;

            ProcessList.Add(proc);

            imgClose := TImage(pnlPrograms.FindComponent('btProgramClose' + IntToStr(Tag + 1)));
            imgClose.Visible := True;
//            CheckForProcessRun;
           finally
           end;
         end;
    end;
end;

procedure TfrmMain.ProgramCloseButtonClick(Sender: TObject);
var
  ProcIndex: integer;
begin
  with Sender as TImage do
    begin
      ProcIndex := GetProcessListIndexByName(PMyProgram(ProgramList[Tag])^.Filename);
      if ProcIndex >= 0 then
        begin;
          PMyProcess(ProcessList[ProcIndex])^.Process.Terminate(0);
          ProcessList.Delete(ProcIndex);
        end;
      Visible := false;
    end;
end;

procedure TfrmMain.LoadDefaults;
begin
  Config.Add('domain=uvao.obr.mos.ru');
  {$IFDEF WINDOWS}
  Config.Add('explorer=D:\Windows\explorer.exe');
  Config.Add('internet=D:\Program Files (x86)\Google\Chrome\Application\chrome.exe');
  Config.Add('board=C:\Windows\system32\mspaint.exe');
  {$ENDIF}
  {$IFDEF LINUX}
  Config.Add('explorer=dolphin');
  Config.Add('internet=/usr/bin/chromium-browser');
  Config.Add('board=/usr/bin/openboard');
  Config.add('');
  Config.Add('program1=Kumir 2');
  Config.Add('icon1=/usr/share/icons/hicolor/64x64/apps/kumir2-classic.png');
  Config.Add('exec1=/usr/bin/kumir2-classic');
  Config.add('');
  Config.Add('program2=r7-office');
  Config.Add('icon2=/usr/share/icons/hicolor/64x64/apps/r7-office.png');
  Config.Add('exec2=/usr/bin/r7-office');
  Config.add('');
  Config.Add('program3=libreoffice7.3');
  Config.Add('icon3=/usr/share/icons/hicolor/64x64/apps/libreoffice-main.png');
  Config.Add('exec3=/usr/bin/libreoffice7.3');
  Config.add('');
  Config.Add('program4=Scratch');
  Config.Add('icon4=/usr/share/icons/hicolor/128x128/apps/scratch.png');
  Config.Add('exec4=/usr/bin/scratch');
  {$ENDIF}

end;

procedure TfrmMain.ResizeLoginForm(pnlTop, pnlLeft, pnlWidth, pnlHeight: integer);
var
  bleft, bwidth, bHeight, bTop, i, gap: integer;
begin
  gap := pnlHeight div 15;
  bwidth := pnlWidth div 7 * 5;
  bHeight := pnlHeight div 8;

  imgUser.Width := bHeight;
  imgUser.Height := bHeight;

  imgPassword.Width := bHeight;
  imgPassword.Height := bHeight;

  edLogin.Width := bwidth - imgPassword.Width;
  edLogin.Height := bHeight;
  edLogin.Top := pnlTop;
  edLogin.Left := (self.Width div 2) - (edLogin.Width div 2);

  edPassword.Width := edLogin.Width;
  edPassword.Height := edLogin.Height;
  edPassword.Top := edLogin.Top + edLogin.Height + gap;
  edPassword.Left := (self.Width div 2) - (edPassword.Width div 2);

  imgUser.Left := edLogin.Left - imgUser.Width - gap;
  imgUser.Top := pnlTop;

  imgPassword.Left := imgUser.Left;
  imgPassword.Top := edPassword.Top;

  //btLogin.Width := pnlWidth div 3;
  //btLogin.Height := pnlHeight div 5;
  //btLogin.Left := (self.Width div 2) - (btLogin.Width div 2);
  //btLogin.Top := edPassword.Top + edPassword.Height + gap;

  imgbtLogin.Width := edLogin.Width div 2;
  imgbtLogin.Height := pnlHeight div 5;
  imgbtLogin.Left := (self.Width div 2) - (imgbtLogin.Width div 2);
  imgbtLogin.Top := edPassword.Top + edPassword.Height + gap;

  //lbLogin.Left := (self.Width div 2) - (lbLogin.Width div 2);
  //lbLogin.Top := btLogin.Top + (btLogin.Height div 2 - lbLogin.Height div 2);

  btGuest.Width := imgbtLogin.Width div 2;
  btGuest.Height := imgbtLogin.Height div 2;;
  btGuest.Left := (self.Width div 2) - (btGuest.Width div 2);
  btGuest.Top := imgbtLogin.Top + imgbtLogin.Height + Gap * 2;
end;

function TfrmMain.CreateUser(user: string): boolean;
begin
  {$IFDEF WINDOWS}
  CreateDir(SysUtils.GetEnvironmentVariable('appdata') + '\DnoShell\');
  CreateDir(SysUtils.GetEnvironmentVariable('appdata') + '\DnoShell\Users');
  CreateDir(SysUtils.GetEnvironmentVariable('appdata') + '\DnoShell\Users\' + user);
  CreateDir(SysUtils.GetEnvironmentVariable('appdata') + '\DnoShell\Users\' + user + '\files');
  CreateDir(SysUtils.GetEnvironmentVariable('appdata') + '\DnoShell\Users\' + user + '\.profile');
  {$ENDIF}

  {$IFDEF LINUX}
  CreateDir('./DnoShell/');
  CreateDir('./DnoShell/Users');
  CreateDir('./DnoShell/Users/' + user);
  CreateDir('./DnoShell/Users/' + user + '/files');
  CreateDir('./DnoShell/Users/' + user + '/.profile');
  {$ENDIF}
end;

function TfrmMain.LoadPrograms: boolean;
var
  i: integer;
  Prg: PMyProgram;
begin
  for i := 1 to 20 do
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

procedure TfrmMain.ArrangePrograms;
var
  i, bleft: integer;
  img, imgClose: TImage;
  lab: TLabel;
begin
  for i := 0 to ProgramList.Count - 1 do
    begin
      //Панель прграмм внизу
      img := TImage.Create(pnlPrograms);
      img.Parent := pnlPrograms;
      bleft := (pnlPrograms.Width div 2) - (ProgramList.Count * 30 div 2) + i * 30;
      img.Left := bleft;
      img.Height := 30;
      img.Width := 30;
      img.Top := 0;
      img.Name := 'btProgram' + IntToStr(i + 1);
      img.Caption := PMyProgram(ProgramList[i])^.Filename;
      img.Tag := i;
      img.Stretch:=true;
      img.Transparent:=true;
      img.OnClick := @ProgramButtonClick;
      img.OnMouseMove := @btImage1MouseMove;
      img.OnMouseLeave := @btImage1MouseLeave;
      img.OnMouseEnter := @btImage1MouseEnter;
      img.OnMouseDown := @btImage1MouseDown;
      img.OnMouseUp := @btImage1MouseUp;
      if FileExists(PMyProgram(ProgramList[i])^.Icon) then
        begin
          img.Picture.LoadFromFile(PMyProgram(ProgramList[i])^.Icon);
        end;

      imgClose := TImage.Create(pnlPrograms);
      imgClose.Stretch:=true;
      imgClose.Transparent:=true;
      ImageList1.GetBitmap(0, imgClose.Picture.Bitmap);
      imgClose.Name := 'btProgramClose' + IntToStr(i + 1);
      imgClose.Parent := pnlPrograms;

      imgClose.OnClick := @ProgramCloseButtonClick;
      imgClose.OnMouseMove := @btImage1MouseMove;
      imgClose.OnMouseLeave := @btImage1MouseLeave;
      imgClose.OnMouseEnter := @btImage1MouseEnter;
      imgClose.OnMouseDown := @btImage1MouseDown;
      imgClose.OnMouseUp := @btImage1MouseUp;
      imgClose.Tag := i;
      imgClose.BringToFront;
      imgClose.Visible := false;

      lab := TLabel.Create(pnlPrograms);
      lab.Parent := pnlPrograms;
      lab.Caption := PMyProgram(ProgramList[i])^.Name;
      lab.Name := 'lbProgram' + IntToStr(i + 1);
    end;
end;

procedure TfrmMain.LocalizeDate;
begin
  DefaultFormatSettings.LongDayNames[1]:='Воскресенье';
  DefaultFormatSettings.LongDayNames[2]:='Понедельник';
  DefaultFormatSettings.LongDayNames[3]:='Вторник';
  DefaultFormatSettings.LongDayNames[4]:='Среда';
  DefaultFormatSettings.LongDayNames[5]:='Четверг';
  DefaultFormatSettings.LongDayNames[6]:='Пятница';
  DefaultFormatSettings.LongDayNames[7]:='Суббота';

  DefaultFormatSettings.LongMonthNames[1]:='Января';
  DefaultFormatSettings.LongMonthNames[2]:='Февраля';
  DefaultFormatSettings.LongMonthNames[3]:='Марта';
  DefaultFormatSettings.LongMonthNames[4]:='Апреля';
  DefaultFormatSettings.LongMonthNames[5]:='Мая';
  DefaultFormatSettings.LongMonthNames[6]:='Июня';
  DefaultFormatSettings.LongMonthNames[7]:='Июля';
  DefaultFormatSettings.LongMonthNames[8]:='Августа';
  DefaultFormatSettings.LongMonthNames[9]:='Сентября';
  DefaultFormatSettings.LongMonthNames[10]:='Октября';
  DefaultFormatSettings.LongMonthNames[11]:='Ноября';
  DefaultFormatSettings.LongMonthNames[12]:='Декабря';
end;

procedure TfrmMain.CheckForProcessRun;
var
  i, j: integer;
  img: TImage;
  imgClose: TImage;
begin
  for i := 0 to ProgramList.Count - 1 do
    begin
      imgClose := TImage(pnlPrograms.FindComponent('btProgramClose' + IntToStr(i + 1)));
      if IsProcessExecuted(PMyProgram(ProgramList[i])^.Filename) then
         imgClose.Visible := true
      else
         imgClose.Visible := false;
    end;
end;

function TfrmMain.IsProcessExecuted(ProcessName: string): boolean;
var
  i: integer;
begin
  Result := false;
  for i := 0 to ProcessList.Count - 1 do
    begin
      {$IFDEF LINUX}
      if (PMyProcess(ProcessList[i])^.Process.Parameters[1] = ProcessName) and (PMyProcess(ProcessList[i])^.Process.Running) then
      {$ENDIF}
      {$IFDEF WINDOWS}
      if (PMyProcess(ProcessList[i])^.Process.Executable = ProcessName) and (PMyProcess(ProcessList[i])^.Process.Running) then
      {$ENDIF}
        begin
         Result := True;
         Break;
        end;
    end;
end;

function TfrmMain.GetProcessListIndexByName(ProcessName: string): Integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to ProcessList.Count - 1 do
    begin
      {$IFDEF LINUX}
      if PMyProcess(ProcessList[i])^.Process.Parameters[1] = ProcessName then
      {$ENDIF}
      {$IFDEF WINDOWS}
      if PMyProcess(ProcessList[i])^.Process.Executable = ProcessName then
      {$ENDIF}
        begin
         Result := i;
         Break;
        end;
    end;
end;

procedure TfrmMain.TerminateAndClearProcesses;
var
  i: Integer;
begin
  for i := ProcessList.Count - 1 downto 0 do
    begin
      PMyProcess(ProcessList[i])^.Process.Terminate(0);
      PMyProcess(ProcessList[i])^.Process.Free;
      ProcessList.Delete(i);
    end;
end;


end.

