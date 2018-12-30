unit loadscreen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Controls, StdCtrls;

type
  { TLoadScreen }

  TLoadScreen = class
    private
      text: TLabel;
      m: integer;
      timer: TTimer;
      bg: TImage;
      FVisible:boolean;
      const status: array [0..15] of String = ('Destilliere Schönheit',
                                               'Filtere Moral',
                                               'Überlaste CPU',
                                               'Lösche ungespeicherten Fortschritt',
                                               'Blitting aller Zweierpotenzen',
                                               'Lokalisiere Bugs',
                                               'Ändere alles',
                                               'Baumloade Avatar 2',
                                               'Extrahiere',
                                               'Erhalte Unabhängigkeit',
                                               'Ionisiere',
                                               'Erfahre Macht der 42',
                                               'Revolutioniere Spielindustrie',
                                               'Missioniere Windows-Nutzer',
                                               'Hacke Pentagon',
                                               'Initialisiere Weltuntergang');
      var
      procedure message(Sender: TObject);
      procedure SetVisibility(value: boolean);
     public
      property Visible: boolean read FVisible write SetVisibility;
      procedure Resize(w,h: integer);
      Constructor Create(TheOwner: TComponent);
      Destructor Destroy; override;
  end;

implementation

{ TLoadScreen }

procedure TLoadScreen.message(Sender: TObject);
begin
  inc(m);
  if m=17 then begin
    timer.Interval:=1000;
    Exit;
  end
  else if m=18 then begin
    Visible:=false;
    timer.Enabled:=false;
    Exit;
  end;
  text.Caption:=text.Caption+#10+status[m];
  text.Top:= text.Top-13;
  timer.Interval:=round(timer.Interval*0.8);
end;

procedure TLoadScreen.SetVisibility(value: boolean);
begin
  bg.Visible:=value;
  text.Visible:=value;
  FVisible:=value;
end;

procedure TLoadScreen.Resize(w, h: integer);
begin
  bg.SetBounds(0, 0, w, h);
  with text do Top:= h-Height-5;
end;

constructor TLoadScreen.Create(TheOwner: TComponent);
begin
  bg:= TImage.Create(TheOwner);
  with bg do
  begin
    Parent:= TWinControl(TheOwner);
    Left:=0;
    Top:=0;
    Stretch:=true;
    with Canvas do rect(0,0,Width,Height);
  end;

  text:= TLabel.Create(TheOwner);
  with text do
  begin
    Parent:= TWinControl(TheOwner);
    Left:= 20;
    m:=0;
    Caption:= status[m];
    Font.Name:='Courier';
    Font.Color:=$00FF00;
  end;

  timer:= TTimer.Create(TheOwner);
  with timer do begin
    Interval:= 500;
    OnTimer:=@message;
    Enabled:= true;
  end;
  Visible:=true;
end;

destructor TLoadScreen.Destroy;
begin
  bg.Free;
  text.Free;
  timer.Free;
  inherited Destroy;
end;

end.

