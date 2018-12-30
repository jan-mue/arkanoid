unit options;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, LResources, ExtCtrls, SysUtils, ComCtrls, StdCtrls, LCLIntf,
  {$IFDEF MSWINDOWS}
    MMSystem,
  {$ENDIF}
  Controls;

type

  { TOption }

  TOption = class
    private
      Fvalue: boolean;
      icon, button: TImage;
      proc: procedure of object;
      procedure OnRowClick(Sender: TObject);
      procedure loadImage;
      procedure setValue(val: boolean);
      procedure setSwitch(val: boolean);
      function getSwitch: boolean;
    public
      bg: TImage;
      cap: TLabel;
      property value: boolean read Fvalue write setValue;
      property switch: boolean read getSwitch write setSwitch;
      Constructor Create(TheOwner: TComponent; ico, txt: String; y: Integer);
      Destructor Destroy; override;
  end;

{ TForm2 }

  TForm2 = class(TForm)
    private
      rows: array[0..6] of TOption;
      speed: TTrackBar;
      header, exit: TImage;
      procedure ShowManual;
      procedure SwitchSound;
      procedure SwitchControls;
      procedure SetSpeed(Sender: TObject);
      procedure SetFPS(s: String);
      procedure FormClose(Sender: TObject);
    public
      property fps: string write SetFPS;
      procedure FormShow(Sender: TObject);
      constructor Create(TheOwner: TComponent); override;
      destructor Destroy; override;
  end;

implementation
uses main;

  { TOption }

  procedure TOption.OnRowClick(Sender: TObject);
  begin
    Fvalue:=not Fvalue;
    loadImage;
    if proc<>nil then proc;
  end;

  procedure TOption.loadImage;
  begin
    if Fvalue then button.Picture.LoadFromFile('../config/images/on.png')
    else button.Picture.LoadFromFile('../config/images/off.png');
  end;

  procedure TOption.setValue(val: boolean);
  begin
    if val<>FValue then begin
      FValue:=val;
      loadImage;
    end;
  end;

  procedure TOption.setSwitch(val: boolean);
  begin
    if button<>nil then button.Visible:=val;
  end;

  function TOption.getSwitch: boolean;
  begin
    if button<>nil then Result:=button.Visible
    else Result:=false;
  end;

  constructor TOption.Create(TheOwner: TComponent; ico, txt: String; y: Integer);
  begin
    Fvalue:=false;
    proc:=nil;

    bg:= TImage.Create(TheOwner);
    with bg do begin
      Parent:=TWinControl(TheOwner);
      Stretch:=true;
      Top:=y;
      Left:=0;
      Width:=TControl(TheOwner).Width;
      Height:=49;
      Picture.LoadFromFile('../config/images/row.png');
      OnClick:=@OnRowClick;
    end;
    icon:=TImage.Create(TheOwner);
    with icon do begin
      Parent:=TWinControl(TheOwner);
      Stretch:=true;
      Width:=31;
      Height:=31;
      Top:=y+9;
      Left:=17;
      Picture.LoadFromFile(ico);
      OnClick:=@OnRowClick;
    end;
    cap:= TLabel.Create(TheOwner);
    with cap do begin
      Parent:=TWinControl(TheOwner);
      Caption:=txt;
      Font.Name:='Roboto Bold';
      Font.Size:=10;
      Left:=56;
      Top:=y + (49-Height) div 2;
      OnClick:=@OnRowClick;
    end;
    button:= TImage.Create(TheOwner);
    with button do begin
      Parent:=TWinControl(TheOwner);
      Stretch:=true;
      Width:=35;
      Height:=22;
      Top:=y+14;
      Left:=bg.Width-57;
      OnClick:=@OnRowClick;
    end;
    loadImage;
  end;

destructor TOption.Destroy;
begin
  bg.Free;
  icon.Free;
  cap.Free;
  button.Free;
  inherited Destroy;
end;

{ TForm2 }

procedure TForm2.ShowManual;
begin
  OpenDocument('../config/resources/manual.pdf');
end;

procedure TForm2.SwitchSound;
  begin
    sound:=not sound;
    {$IFDEF MSWINDOWS}
    if not sound then sndPlaySound(nil, 0)
    else if Form1.menu1.Visible then
    sndPlaySound('../config/sounds/title.wav', SND_ASYNC or SND_LOOP)
    else sndPlaySound('../config/sounds/cave.wav', SND_ASYNC or SND_LOOP);
    {$ENDIF}
  end;

procedure TForm2.SwitchControls;
begin
  mcontrol:= not mcontrol;
end;

  procedure TForm2.SetSpeed(Sender: TObject);
  begin
    animation:=speed.Position/10;
    rows[3].cap.Caption:='Animationen: '+FloatToStr(round(animation*10)/10)+'X';
    if Form1.level<>nil then Form1.level.Ballv:=2*animation;
  end;

  procedure TForm2.SetFPS(s: String);
  begin
    rows[5].cap.Caption:=s;
  end;

  procedure TForm2.FormClose(Sender: TObject);
  begin
    Close;
  end;

  procedure TForm2.FormShow(Sender: TObject);
  begin
    show;
  end;

  constructor TForm2.Create(TheOwner: TComponent);
  begin
    inherited CreateNew(TheOwner);
    if LazarusResources.Find(ClassName)=nil then begin
      SetBounds((Screen.Width-250) div 2, (Screen.Height-491) div 2, 250, 491);
      Caption:='';
      BorderStyle := bsNone;

      lighting:=true;
      sound:=true;
      fullscreen:=false;
      mcontrol:=true;

      header:=TImage.Create(self);
      header.Parent:=self;
      with header do begin
      SetBounds(0, 0, 250, 100);
      Picture.LoadFromFile('../config/images/header.png');
      end;

      exit:=TImage.Create(self);
      exit.Parent:=self;
      with exit do begin
      SetBounds(228, 2, 16, 16);
      Picture.LoadFromFile('../config/images/close.png');
      OnClick:=@FormClose;
      end;

      rows[0]:=TOption.Create(self, '../config/images/fullscreen.png', 'Fullscreen', 100);
      rows[1]:=TOption.Create(self, '../config/images/sound.png', 'Sound', 149);
      rows[2]:=TOption.Create(self, '../config/images/light.png', 'Beleuchtung', 198);
      rows[3]:=TOption.Create(self, '../config/images/animation.png', 'Animationen', 247);
      with rows[3] do begin
        switch:=false;
        bg.Picture.LoadFromFile('../config/images/row2.png');
        bg.Height:=97;
      end;
      rows[4]:=TOption.Create(self, '../config/images/control.png', 'Maussteuerung', 344);
      rows[4].value:=true;
      rows[5]:=TOption.Create(self, '../config/images/speed.png', 'Bitte Spiel starten', 393);
      rows[5].switch:=false;

      rows[6]:=TOption.Create(self, '../config/images/help.png', 'Hilfe', 442);
      rows[6].switch:=false;
      rows[6].proc:=@ShowManual;

      rows[0].proc:=@Form1.SwitchFullScreen;
      rows[1].proc:=@SwitchSound;
      rows[1].value:=true;
      rows[2].proc:=@Form1.SwitchLight;
      rows[2].value:=true;
      rows[4].proc:=@SwitchControls;

      speed:=TTrackBar.Create(self);
      speed.Parent:=self;
      with speed do begin
        Width:=200;
        Left:=25;
        Top:=300;
        Min:=1;
        Max:=45;
        OnChange:=@SetSpeed;
        Position:=40;
        {$IFDEF MSWINDOWS}
        Position:=10;
        {$ENDIF}
      end;
    end;
  end;

destructor TForm2.Destroy;
var i: integer;
begin
  for i:=0 to 6 do rows[i].Free;
  speed.Free;
  header.Free;
  exit.Free;
  inherited Destroy;
end;

end.

