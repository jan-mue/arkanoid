unit menu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, StdCtrls, Dialogs,
  {$IFDEF MSWINDOWS}
  MMSystem,
  {$ENDIF}
  loadscreen;

type

  { TMenu }

  TMenu = class
    private
      HName: TEdit;
      HScore, about: TLabel;
      save:TButton;
      logo, bg, gm, reset, back: TImage;
      scores: Integer;
      levels: array[0..4] of TImage;
      buttons: array[0..5] of TImage;
      highscores: array[0..11] of TImage;
      HighNames: array[0..11] of TLabel;
      load: TLoadScreen;
      FVisible:boolean;
      HScores: array of Integer;
      HNames: array of String;
      procedure SetVisibility(value: boolean);
      procedure showAbout(Sender: TObject);
      procedure showHighscores(Sender: TObject);
      procedure showLevels(Sender: TObject);
      procedure goBack(Sender: TObject);
      procedure saveScore(Sender: TObject);
      procedure resetScores(Sender: TObject);
      procedure updateScores;
    public
      property Visible: boolean read FVisible write SetVisibility;
      procedure BringToFront;
      procedure Resize;
      procedure GameOver;
      Constructor Create(TheOwner: TComponent);
      Destructor Destroy; override;
  end;


implementation
uses main;

{ TMenu }

procedure TMenu.SetVisibility(value: boolean);
var i: integer;
begin
  for i:=0 to 5 do buttons[i].Visible:=value;
  logo.Visible:=value;
  bg.Visible:=value;
  FVisible:=value;
  for i:=0 to 4 do if levels[i].Visible then levels[i].Visible:=value;
  if back.Visible then back.Visible:=value;
end;

procedure TMenu.showAbout(Sender: TObject);
begin
  Visible:=false;
  bg.Show;
  about.Show;
  back.Show;
  Resize;
end;

procedure TMenu.showHighscores(Sender: TObject);
var i:integer;
begin
  if length(HScores)=0 then begin
    ShowMessage('Noch keine Highscores vorhanden!');
    Exit;
  end;
  Visible:=false;
  bg.Show;
  for i:=0 to scores do begin
    highscores[i].Show;
    HighNames[i].Show;
  end;
  back.Show;
  reset.Show;
  Resize;
end;

procedure TMenu.showLevels(Sender: TObject);
var i: integer;
begin
  Visible:=false;
  bg.Show;
  for i:=0 to 4 do levels[i].Show;
  back.Show;
  Resize;
end;

procedure TMenu.goBack(Sender: TObject);
var i:integer;
begin
  for i:=0 to scores do begin
    highscores[i].Hide;
    HighNames[i].Hide;
  end;
  for i:=0 to 4 do levels[i].Hide;
  about.Hide;
  back.Hide;
  reset.Hide;
  Visible:=true;
end;

procedure TMenu.saveScore(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
    if sound then sndPlaySound('../config/sounds/title.wav', SND_ASYNC or SND_LOOP);
  {$ENDIF}
  setLength(HScores, length(HScores)+1);
  setLength(HNames, length(HNames)+1);
  HNames[length(HNames)-1]:=HName.Text;
  HScores[length(HScores)-1]:=Form1.score.CurrentValue;
  updateScores;
  HScore.Hide;
  HName.Hide;
  gm.Hide;
  save.Hide;
  Visible:=true;
  BringToFront;
end;

procedure TMenu.resetScores(Sender: TObject);
var i: integer;
begin
  for i:=0 to scores do begin
    highscores[i].Hide;
    HighNames[i].Hide;
  end;
  HScores:=nil;
  HNames:=nil;
  updateScores;
  goBack(self);
end;

procedure TMenu.updateScores;
var
  i, j, IItem:integer;
  SItem: String;
begin
  for i:=0 to Length(HScores)-2 do            //Sortieren
    for j:=i downto 0 do
      if (HScores[j+1] > HScores[j]) then begin
        IItem:=HScores[j];
        HScores[j]:=HScores[j+1];
        HScores[j+1]:=IItem;
        SItem:=HNames[j];
        HNames[j]:=HNames[j+1];
        HNames[j+1]:=SItem;
      end;
  if length(HScores)>length(highscores) then scores:=length(highscores)-1
  else scores:=length(HScores)-1;
  for i:=0 to scores do
  HighNames[i].Caption:= IntToStr(i+1)+'   '+HNames[i]+' '+IntToStr(HScores[i]);
end;

procedure TMenu.BringToFront;
var i: integer;
begin
  bg.BringToFront;
  back.BringToFront;
  reset.BringToFront;
  about.BringToFront;
  gm.BringToFront;
  HScore.BringToFront;
  HName.BringToFront;
  for i:=0 to 5 do buttons[i].BringToFront;
  for i:=0 to 4 do levels[i].BringToFront;
  for i:=0 to length(highscores)-1 do begin
    highscores[i].BringToFront;
    HighNames[i].BringToFront;
  end;
  logo.BringToFront;
end;

procedure TMenu.Resize;
var i, w, h: integer;
begin
  w:=TControl(bg.Parent).Width;
  h:=TControl(bg.Parent).Height;

  bg.SetBounds(0, 0, w, h);

  if (load<>nil) and load.Visible then load.Resize(w,h);

  if about.Visible then with about do begin
     Font.Size:=round(3/64*w);
     Left:=(w-Width) div 2;
     Top:=(h-Height) div 2;
  end;

  with logo do begin
    Width:= round(w*739/1280);
    Height:= round(h*235/800);
    Left:= (w-Width) div 2;
    Top:= round(h/2-h*65/800-Height);
  end;

  if back.Visible then with back do begin
    Width:=round(w*300/1920);
    Height:=round(h*87/1200);
    Top:=round(5/6*h);
    Left:=(w-Width+600) div 2;
  end;
  if reset.Visible then with reset do begin
    Width:=round(w*300/1920);
    Height:=round(h*87/1200);
    Top:=round(6/8*h);
    Left:=(w-Width+600) div 2;
  end;

  for i:=0 to 5 do with buttons[i] do begin
    Width:=round(w*500/1920);
    Height:=round(h*145/1200);
    Top:=round(h/2-h/40+1.1*Height*(i-(i mod 2))/2);
    Left:=round(w/2-w*35/128+1.1*Width*(i mod 2));
  end;

  for i:=0 to 4 do if levels[i].Visible then with levels[i] do begin
    Width:=round(w*300/1920);
    Height:=round(h*87/1200);
    Top:=round(h/2-h/6+1.2*Height*(i-(i mod 3))/3);
    Left:=round(w/2-w*51/192+1.2*Width*(i mod 3));
  end;

  for i:=0 to length(highscores)-1 do if highscores[i].Visible then begin
    with highscores[i] do begin
      Width:=round(w*500/1920);
      Height:=round(h*87/1200);
      Top:=round(h/2-Height*6.045+i*1.01*Height);
      Left:=(w-Width-400) div 2;
    end;
    with HighNames[i] do begin
      Font.Size:=round(w*1/64);
      Top:=highscores[i].Top + (highscores[i].Height-Height) div 2;
      Left:=highscores[i].Left + round((highscores[i].Width-1.2*Width)/2);
    end;
  end;

  if gm.Visible then gm.SetBounds(0, 0, w, h);
  if HScore.Visible then with HScore do begin
    Font.Size:=round(1/64*w);
    Top:=round(4/6*h);
    Left:=(w-Width) div 2;
  end;
  if HName.Visible then with HName do begin
    Width:=round(w*200/1920);
    Top:=round(5/7*h);
    Left:=(w-Width) div 2;
  end;
  if save.Visible then with save do begin
    Top:=round(6/8*h);
    Left:=(w-Width) div 2;
  end;
end;

constructor TMenu.Create(TheOwner: TComponent);
var i: integer;
begin
  FVisible:=true;

  bg:= TImage.Create(TheOwner);
  with bg do
  begin
    Parent:= TWinControl(TheOwner);
    Stretch:=true;
    Left:=0;
    Top:=0;
    Picture.LoadFromFile('../config/images/stars.png');
    {$IFDEF MSWINDOWS}
    OnMouseDown:=@Form1.ComponentMouseDown;
    {$ENDIF}
  end;

  back:=TImage.Create(TheOwner);
  with back do begin
    Parent:=TWinControl(TheOwner);
    Stretch:=true;
    Picture.LoadFromFile('../config/images/back.png');
    Visible:=false;
    OnClick:=@goBack;
  end;

  reset:=TImage.Create(TheOwner);
  with reset do begin
    Parent:=TWinControl(TheOwner);
    Stretch:=true;
    Picture.LoadFromFile('../config/images/reset.png');
    Visible:=false;
    OnClick:=@resetScores;
  end;

  for i:=0 to 5 do  begin
    buttons[i]:=TImage.Create(TheOwner);
    with buttons[i] do begin
      Parent:=TWinControl(TheOwner);
      Stretch:=true;
    end;
  end;
  buttons[0].Picture.LoadFromFile('../config/images/play.png');
  buttons[1].Picture.LoadFromFile('../config/images/levels.png');
  buttons[2].Picture.LoadFromFile('../config/images/highscores.png');
  buttons[3].Picture.LoadFromFile('../config/images/options.png');
  buttons[4].Picture.LoadFromFile('../config/images/info.png');
  buttons[5].Picture.LoadFromFile('../config/images/quit.png');

  buttons[0].OnClick:=@Form1.play;
  buttons[1].OnClick:=@showLevels;
  buttons[2].OnClick:=@showHighscores;
  buttons[3].OnClick:=@Form2.FormShow;
  buttons[4].OnClick:=@showAbout;
  buttons[5].OnClick:=@Form1.FormClose;

  for i:=0 to 4 do  begin
    levels[i]:=TImage.Create(TheOwner);
    with levels[i] do begin
      Parent:=TWinControl(TheOwner);
      Stretch:=true;
      Visible:=false;
    end;
  end;
  levels[0].Picture.LoadFromFile('../config/images/level1.png');
  levels[1].Picture.LoadFromFile('../config/images/level2.png');
  levels[2].Picture.LoadFromFile('../config/images/level3.png');
  levels[3].Picture.LoadFromFile('../config/images/level4.png');
  levels[4].Picture.LoadFromFile('../config/images/level5.png');

  levels[0].OnClick:=@Form1.play;
  levels[1].OnClick:=@Form1.load1;
  levels[2].OnClick:=@Form1.load2;
  levels[3].OnClick:=@Form1.load3;
  levels[4].OnClick:=@Form1.load4;

  logo:= TImage.Create(TheOwner);
  with logo do
  begin
    Parent:= TWinControl(TheOwner);
    Stretch:=true;
    Picture.LoadFromFile('../config/images/logo.png');
  end;
  //load:=TLoadScreen.Create(TheOwner);

  about:= TLabel.Create(TheOwner);
  with about do
  begin
    Parent:= TWinControl(TheOwner);
    Caption:= 'A Game by'+#10+'Jan Müller';
    Font.Name:='Star Jedi Outline';
    Font.Color:=$00D7FF;
    Visible:=false;
  end;

  for i:=0 to length(highscores)-1 do begin
    highscores[i]:=TImage.Create(TheOwner);
    with highscores[i] do begin
      Parent:= TWinControl(TheOwner);
      Stretch:=true;
      Picture.LoadFromFile('../config/images/block.png');
      Visible:=false;
    end;
    HighNames[i]:=TLabel.Create(TheOwner);
    with HighNames[i] do begin
      Parent:= TWinControl(TheOwner);
      Font.Name:='Roboto Thin';
      Font.Color:=$FFFFFF;
      Visible:=false;
    end;
  end;

  with TStringList.Create do
    try
      LoadFromFile('../config/resources/scores');
      SetLength(HNames, Count div 2);
      SetLength(HScores, Count div 2);
      for i:=0 to (Count-2) div 2 do begin
        HNames[i]:=Strings[2*i];
        HScores[i]:=StrToInt(Strings[2*i+1]);
      end;
    finally
      Free;
    end;

  //SetLength(HNames, 10);
  //SetLength(HScores, 10);
  //randomize;
  //for i:=0 to 9 do begin
  //  HNames[i]:='Computer';
  //  HScores[i]:=random(100)*100;
  //end;
  updateScores;

  gm:= TImage.Create(TheOwner);
    with gm do begin
      Parent:=TWinControl(TheOwner);
      Stretch:=true;
      Picture.LoadFromFile('../config/images/game over.png');
      Visible:=false;
    end;

  HScore:= TLabel.Create(TheOwner);
    with HScore do
    begin
      Parent:= TWinControl(TheOwner);
      Font.Name:='Roboto';
      Font.Color:=$000000;
      Visible:=false;
    end;

    HName:= TEdit.Create(TheOwner);
    with HName do
    begin
      Parent:= TWinControl(TheOwner);
      Text:='Spieler';
      Visible:=false;
    end;

    save:= TButton.Create(TheOwner);
    with save do
    begin
      Parent:= TWinControl(TheOwner);
      Caption:='Speichern';
      OnClick:=@saveScore;
      Visible:=false;
    end;
end;

procedure TMenu.GameOver;
begin
  Form1.OpenGLControl1.Visible:=false;
  Form1.OpenGLControl1.SetBounds(0,0,0,0);

  if (HScores=nil) or (Form1.score.CurrentValue>HScores[0]) then
  HScore.Caption:='Neuer Highscore! '
  else HScore.Caption:='Ihre Punktzahl: ';
  HScore.Caption:=HScore.Caption + IntToStr(Form1.score.CurrentValue);

  gm.Show;
  HScore.Show;
  HName.Show;
  HName.SetFocus;
  save.Show;
  {$IFDEF UNIX}
  Form1.Height:=Form1.Height+2;
  Form1.Height:=Form1.Height-2;
  {$ENDIF}
  Resize;
end;

destructor TMenu.Destroy;
var i: integer;
begin
  with TStringList.Create do
    try
      for i:=0 to length(HScores)-1 do begin
        Add(HNames[i]);
        Add(IntToStr(HScores[i]));
      end;
      SaveToFile('../config/resources/scores');
    finally
      Free;
    end;
  load.Free;
  logo.Free;
  bg.Free;
  back.Free;
  about.Free;
  gm.Free;
  HScore.Free;
  HName.Free;
  save.Free;
  for i:=0 to 5 do buttons[i].Free;
  for i:=0 to 4 do levels[i].Free;
  for i:=0 to length(highscores)-1 do begin
    highscores[i].Free;
    HighNames[i].Free;
  end;
  inherited Destroy;
end;

end.

