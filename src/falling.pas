unit falling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, movers;

type

  { TFallingObj }

  TFallingObj = class(T2DMover)
    private
      t:single;
      t0:TDateTime;
      v0, x0, y0:double;
    protected
      procedure checkPos; override;
      procedure OnCollusion; virtual;
    public
      procedure reset;
      procedure draw; override;
      Constructor Create(x, y, w, h: single);
  end;

implementation
uses main;

procedure TFallingObj.checkPos;
begin
  t:=(Time-t0)*100000;
  v:=(t*0.0981+v0)*animation;
  if collide(Form1.level.bat) then begin
     if Form1.level.bat.Width<>200 then Form1.level.bat.Width:=200
     else Form1.level.bat.Width:=100;
     OnCollusion;
  end;
  if Top+Height<-400 then OnCollusion;
end;

procedure TFallingObj.OnCollusion;
begin
  hide;
  stop;
end;

procedure TFallingObj.reset;
begin
  stop;
  Left:=x0;
  Top:=y0;
  v:=v0;
  t0:=Time;
  Visible:=true;
end;

procedure TFallingObj.draw;
begin
  move;
end;

constructor TFallingObj.Create(x, y, w, h: single);
begin
  inherited Create;
  x0:=x;
  y0:=y;
  Width:=w;
  Height:=h;
  v0:=0.5;
  a:=270;
  reset;
  resume;
end;
end.

