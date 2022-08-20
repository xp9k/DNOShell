unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Process, LCLType, ldapsend,
  {$IFDEF LINUX} Unix
  {$ENDIF}
  ;

type

  PMyProgram = ^TMyProgram;
  TMyProgram = record
    Filename: string;
    Icon: string;
    Name: string;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    bGuest: TButton;
    bLogin: TButton;
    btLogout: TButton;
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
    lbTime: TLabel;
    lbTime1: TLabel;
    pnlLoginForm: TPanel;
    pnlPrograms2: TPanel;
    pnlPrograms: TPanel;
    pnlFiles: TPanel;
    pnlMainForm: TPanel;
    pnlLogin: TPanel;
    Timer1: TTimer;
    procedure bGuestClick(Sender: TObject);
    procedure bLoginClick(Sender: TObject);
    procedure btImage1Click(Sender: TObject);
    procedure btImage1MouseEnter(Sender: TObject);
    procedure btImage1MouseLeave(Sender: TObject);
    procedure btImage1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure btImage2Click(Sender: TObject);
    procedure btImage3Click(Sender: TObject);
    procedure btImageProgramsClick(Sender: TObject);
    procedure btLogoutClick(Sender: TObject);
    procedure btPoweroffClick(Sender: TObject);
    procedure btRebootClick(Sender: TObject);
    procedure edPasswordKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure pnlLoginFormResize(Sender: TObject);
    procedure pnlLoginResize(Sender: TObject);
    procedure pnlMainFormResize(Sender: TObject);
    procedure pnlPrograms2Resize(Sender: TObject);
    procedure pnlProgramsResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ProgramButtonClick(Sender: TObject);
  private
    CanClose: boolean;
    ProgramList: TList;
    ConfigFile: string;
    UserFolder: String ;
    Config: TStringList;
    imgBtnWidthHeight: integer;
    imgBtnTop: integer;
    tmpWidth: integer;
    tmpHeight: integer;
    tmpTop: integer;
    tmpLeft: integer;
    procedure LoadDefaults;
    function CreateUser(user: string): boolean;
    function LoadPrograms: boolean;
    procedure ArrangePrograms;
    procedure btImgMouseMove(Sender: TObject; Shift: TShiftState; X,
              Y: Integer);
    procedure btImgMouseLeave(Sender: TObject);
    procedure LocalizeDate;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  CanClose := false;

  LocalizeDate;

  Width:=Screen.Width;
  Height:=Screen.Height;
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

  if FileExists(Config.Values['bg']) then
    begin
      image0.Picture.LoadFromFile(Config.Values['bg']);
      image1.Picture.LoadFromFile(Config.Values['bg']);
    end;

  if Config.Values['bgcolor'] <> '' then pnlLoginForm.Color := StringToColor(Config.Values['bgcolor']);

  if FileExists(Config.Values['explorericon']) then
    btImage1.Picture.LoadFromFile(Config.Values['explorericon']);

  if FileExists(Config.Values['interneticon']) then
    btImage2.Picture.LoadFromFile(Config.Values['interneticon']);

  if FileExists(Config.Values['boardicon']) then
    btImage3.Picture.LoadFromFile(Config.Values['boardicon']);

  ProgramList := TList.Create;
  LoadPrograms;
  ArrangePrograms;

  pnlPrograms.Height:=0;
  pnlPrograms.Visible:=false;
  edLogin.Text:='';
  edPassword.Text:='';
  pnlLogin.BringToFront;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (ssShift in Shift) and (Key = VK_F2) then
   begin
     CanClose:=true;
     frmMain.Close;
   end;
end;


procedure TfrmMain.pnlLoginFormResize(Sender: TObject);
var
  bleft, bwidth, bHeight, bTop, i, gap: integer;
begin
  gap := pnlLoginForm.Height div 20;
  bwidth := pnlLoginForm.Width div 5 * 4;
  bHeight := pnlLoginForm.Height div 8;
  edLogin.Width := bwidth;
  edLogin.Height := bHeight;
  edPassword.Width := bwidth;
  edPassword.Height := bHeight;

  bleft := (pnlLoginForm.Width div 2) - (bwidth div 2);

  edLogin.Left := bleft;
  edPassword.Left := bleft;

  bLogin.Width := pnlLoginForm.Width div 3;
  bLogin.Height := pnlLoginForm.Height div 3;
  bLogin.Left := (pnlLoginForm.Width div 2) - (bLogin.Width div 2);

  bGuest.Left := (pnlLoginForm.Width div 2) - (bGuest.Width div 2);

  bTop := pnlLoginForm.Height div 8;
  for i := 0 to pnlLoginForm.ControlCount - 1 do
    begin
      pnlLoginForm.Controls[i].Top := btop;
      pnlLoginForm.Controls[i].BorderSpacing.Top := gap;
    end;
end;

procedure TfrmMain.pnlLoginResize(Sender: TObject);
var
  bleft, bwidth, bHeight, bTop, allwidth: integer;
begin
  bwidth := self.Width div 5;
  bHeight := self.Height div 5;
  bTop := self.Height div 3;
  allwidth := bwidth;
  bleft := (pnlMainForm.Width div 2) - (allwidth div 2);
  pnlLoginForm.Left := bleft;
  pnlLoginForm.Height := bHeight;
  pnlLoginForm.Width := bwidth;
  pnlLoginForm.Top := bTop;

  lbTime.Width := pnlLogin.Width;

  btPoweroff.Width := self.Width div 50;
  btPoweroff.Height := btPoweroff.Width;
  btPoweroff.Top := Self.Height - btPoweroff.Height - btPoweroff.Height div 2;
  btPoweroff.Left := btPoweroff.Width div 2;

  btReboot.Width := self.Width div 50;
  btReboot.Height := btReboot.Width;
  btReboot.Top := btPoweroff.Top - btReboot.Height - btReboot.Height div 2;
  btReboot.Left := btReboot.Width div 2;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if not CanClose then
     CloseAction := caNone
  else
    begin
      Config.Free;
      ProgramList.Free;
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
var
  ldap: TLDAPsend;
  SearchAttributes: TStringList;
begin
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

procedure TfrmMain.btImage1Click(Sender: TObject);
var
  s: string;
  p: TProcess;
begin
  {$IFDEF WINDOWS}
  ExecuteProcess(Config.Values['explorer'], profile);
  {$ENDIF}
  {$IFDEF LINUX}
  try
   p := TProcess.Create(nil);
   p.ShowWindow := swoShow;
   p.Executable := '/bin/bash';
   p.Parameters.Add('-c');
   p.Parameters.Add(Config.Values['explorer'] + ' ' + UserFolder + '/files');
   p.Execute;
  finally
    p.free;
  end;
//  RunCommand('/bin/bash',['-c', 'dolphin ' + UserFolder + '/files'], s);
  {$ENDIF}
end;

procedure TfrmMain.btImage1MouseEnter(Sender: TObject);
begin
  with Sender as TImage do
    begin
      tmpWidth:= Width;
      tmpHeight:= Height;
      tmpTop:= Top;
      tmpLeft:= Left;
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

procedure TfrmMain.btImage2Click(Sender: TObject);
var
  profile, s: string;
  p: TProcess;
begin
  {$IFDEF WINDOWS}
  profile := '--user-data-dir="' + UserFolder + '\.profile"';
  ExecuteProcess(Config.Values['internet'], profile);
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
  finally
    p.free;
  end;
  //RunCommand('/bin/bash',['-c', Config.Values['internet'] + ' ' + profile], s);
  {$ENDIF}
end;

procedure TfrmMain.btImage3Click(Sender: TObject);
var
  p: TProcess;
begin
  {$IFDEF LINUX}
  try
   p := TProcess.Create(nil);
   p.ShowWindow := swoShow;
   p.Executable := '/bin/bash';
   p.Parameters.Add('-c');
   p.Parameters.Add(Config.Values['board']);
   p.Execute;
  finally
    p.free;
  end;
  {$ENDIF}
end;

procedure TfrmMain.btImageProgramsClick(Sender: TObject);
begin
  if (Config.Values['programspos'] = '') or ((Config.Values['programspos'] = 'bottom')) then
     pnlPrograms.Visible:= not pnlPrograms.Visible
  else
    if (Config.Values['programspos'] = 'left') or ((Config.Values['programspos'] = 'right')) then
      pnlPrograms2.Visible:= not pnlPrograms2.Visible;
end;

procedure TfrmMain.btLogoutClick(Sender: TObject);
begin
  edLogin.Text:='';
  edPassword.Text:='';
  pnlLogin.BringToFront;
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
  if key = #13 then bLoginClick(self);
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
      img := TImage(pnlMainForm.FindChildControl('btImage' + IntToStr(i + 1)));
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

  pnlPrograms.top := btImagePrograms.Top + btImagePrograms.Height + self.Height div 50;
  pnlPrograms.Height := Self.Height div 15;
  pnlPrograms.Width := Self.Width;

  if (Config.Values['programspos'] = 'left') then
    begin
      pnlPrograms2.Width := self.Width div 20;
      pnlPrograms2.Top := 0;
      pnlPrograms2.Height := Self.Height;
      pnlPrograms2.Left := self.Width div 20;
    end
  else
    begin
      pnlPrograms2.Width := self.Width div 20;
      pnlPrograms2.Top := 0;
      pnlPrograms2.Height := Self.Height;
      pnlPrograms2.Left := self.Width - pnlPrograms2.Width*2;
    end;

  lbTime1.Width:=self.width;
end;

procedure TfrmMain.pnlPrograms2Resize(Sender: TObject);
var
  i, bleft, bwidth, bTop, allTop, gap: integer;
  img: TImage;
  lab: TLabel;
begin
  bwidth := pnlPrograms2.Width div 5 * 3;
  gap := bwidth div 2;
  bleft := (pnlPrograms2.Width div 2) - (bwidth div 2);
  allTop := bwidth * ProgramList.Count + gap * (ProgramList.Count - 1);

  for i := 0 to ProgramList.Count - 1 do
    begin
      img := TImage(pnlPrograms2.FindSubComponent('btProgram_2' + IntToStr(i + 1)));
      bleft := (pnlPrograms2.Width div 2) - (bwidth div 2);
      bTop := (pnlMainForm.Height div 2) - (allTop div 2) + i * bwidth + i * gap;
      img.Left := bleft;
      img.Height := bwidth;
      img.Width := bwidth;
      img.Top := bTop;

      imgBtnWidthHeight := bwidth;
      imgBtnTop := bTop;

      lab := TLabel(pnlPrograms2.FindSubComponent('lbProgram_2' + IntToStr(i + 1)));
      lab.Left := bleft;
      lab.Top := img.Top + img.Height;
      lab.left := (img.Left + img.Width div 2) - lab.Width div 2;
    end;
end;

procedure TfrmMain.pnlProgramsResize(Sender: TObject);
var
  i, bleft, bwidth, bTop, allwidth, gap: integer;
  img: TImage;
  lab: TLabel;
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

      imgBtnWidthHeight := bwidth;
      imgBtnTop := bTop;

      lab := TLabel(pnlPrograms.FindSubComponent('lbProgram' + IntToStr(i + 1)));
      lab.Left := bleft;
      lab.Top := img.Top + img.Height;
      lab.left := (img.Left + img.Width div 2) - lab.Width div 2;
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
begin
  with sender as TImage do
    begin
      {$IFDEF LINUX}
       try
        p := TProcess.Create(nil);
        p.ShowWindow := swoShow;
        p.Executable := '/bin/bash';
        p.Parameters.Add('-c');
        p.Parameters.Add(PMyProgram(ProgramList[Tag])^.Filename);
        p.Execute;
       finally
         p.free;
       end;
//        RunCommand('/bin/bash',['-c', PMyProgram(ProgramList[Tag])^.Filename], s, [poDetached]);
      {$ENDIF}
    end;
end;

procedure TfrmMain.LoadDefaults;
begin
  Config.Add('domain=uvao.obr.mos.ru');
  {$IFDEF WINDOWS}
  Config.Add('browser=C:\');
  Config.Add('board=C:\');
  {$ENDIF}
  {$IFDEF LINUX}
  Config.Add('explorer=dolphin');
  Config.Add('internet=/usr/bin/chromium-browser');
  Config.Add('board=/usr/bin/openboard');
  {$ENDIF}

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
  img: TImage;
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
      if FileExists(PMyProgram(ProgramList[i])^.Icon) then
        begin
          img.Picture.LoadFromFile(PMyProgram(ProgramList[i])^.Icon);
        end;

      lab := TLabel.Create(pnlPrograms);
      lab.Parent := pnlPrograms;
      lab.Caption := PMyProgram(ProgramList[i])^.Name;
      lab.Name := 'lbProgram' + IntToStr(i + 1);

      // Панель программ слева

      img := TImage.Create(pnlPrograms2);
      img.Parent := pnlPrograms2;
      bleft := 0;
      img.Left := bleft;
      img.Height := 30;
      img.Width := 30;
      img.Top := (pnlPrograms2.Height div 2) - (ProgramList.Count * 30 div 2) + i * 30;
      img.Name := 'btProgram_2' + IntToStr(i + 1);
      img.Caption := PMyProgram(ProgramList[i])^.Filename;
      img.Tag := i;
      img.Stretch:=true;
      img.Transparent:=true;
      img.OnClick := @ProgramButtonClick;
      img.OnMouseMove := @btImage1MouseMove;
      img.OnMouseLeave := @btImage1MouseLeave;
      img.OnMouseEnter := @btImage1MouseEnter;
      if FileExists(PMyProgram(ProgramList[i])^.Icon) then
        begin
          img.Picture.LoadFromFile(PMyProgram(ProgramList[i])^.Icon);
        end;

      lab := TLabel.Create(pnlPrograms2);
      lab.Parent := pnlPrograms2;
      lab.Caption := PMyProgram(ProgramList[i])^.Name;
      lab.Name := 'lbProgram_2' + IntToStr(i + 1);
    end;
end;

procedure TfrmMain.btImgMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  with sender as TImage do
    begin
      Width := imgBtnWidthHeight + 10;
      Height := imgBtnWidthHeight + 10;
      top := imgBtnTop - 5;
    end;
end;

procedure TfrmMain.btImgMouseLeave(Sender: TObject);
begin
  with sender as TImage do
    begin
      Width := imgBtnWidthHeight;
      Height := imgBtnWidthHeight;
      top := imgBtnTop;
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

end.

