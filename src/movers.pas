unit movers;

{$mode objfpc}{$H+}

interface

uses
  Classes, math, root;

type
  { TMover }

  TMover = class(TGLObj)
    private
      Frunning:boolean;
    protected
      procedure move; virtual;
    public
      property running:boolean read Frunning;
      Constructor Create;
      procedure stop;
      procedure resume;
  end;

  { T2DMover }

  T2DMover = class(TMover)
    private
      Fv, Fa: double;
      procedure setSpeed(v1:double);
      procedure setAngle(alpha:double);
      procedure setSpeedX(v1:double);
      procedure setSpeedY(v1:double);
      function getSpeedX: double;
      function getSpeedY: double;
    protected
      procedure checkPos; virtual; abstract;
      procedure move; override;
      procedure OnCollusion(Sender:TGLObj);
    public
      property v: double read Fv write setSpeed;
      property a: double read Fa write setAngle;
      property vx: double read getSpeedX write setSpeedX;
      property vy: double read getSpeedY write setSpeedY;
      function collide(obj:TGLObj): boolean; virtual;
      Constructor Create;
    end;

implementation

{ TMover }

procedure TMover.move;
begin

end;

constructor TMover.Create;
begin
  inherited Create;
  Frunning:=false;
end;

procedure TMover.stop;
begin
  Frunning:=false;
end;

procedure TMover.resume;
begin
  Frunning:=true;
end;

function T2DMover.collide(obj: TGLObj): boolean;
begin
  if (obj.Visible) and (Left+Width+vx>=obj.Left) and (Left+vx<=obj.Left+obj.Width)
  and (Top+Height+vy>=obj.Top) and (Top+vy<=obj.Top+obj.Height) then Result:=true
  else Result:=false;
end;

constructor T2DMover.Create;
begin
  inherited Create;
  Fa:=0.0;
  Fv:=0.0;
end;

procedure T2DMover.OnCollusion(Sender: TGLObj);
begin
  if vx<>0 then
  begin
    if (Left-Width/2>Sender.Left+Sender.Width) then //von rechts
    begin
      if (((Top-Height/2+(Sender.Left+Sender.Width-Left+Width/2)/vx*vy)>=Sender.Top+Sender.Height) or  //oben
      ((Top+Height/2+(Sender.Left+Sender.Width-Left+Width/2)/vx*vy)<=Sender.Top)) then vy:=-vy          //unten
      else vx:=-vx;
    end
    else if (Left+Width/2<Sender.Left) then //von links
    begin
      if (((Top-Height/2+(Sender.Left-Left-Width/2)/vx*vy)>=Sender.Top+Sender.Height) or              //oben
      ((Top+Height/2+(Sender.Left-Left-Width/2)/vx*vy)<=Sender.Top)) then vy:=-vy                      //unten
      else vx:=-vx;
    end
    else vy:=-vy;
  end
  else vy:=-vy;
end;

procedure T2DMover.move;
begin
  if Frunning then
  begin
  checkPos;
  Left:=Left+vx;
  Top:=Top+vy;
  checkPos;
  end;
end;

procedure T2DMover.setSpeed(v1: double);
begin
  if (v1>=0) and (v1<=sqrt(200)) then Fv:=v1;
end;

procedure T2DMover.setAngle(alpha: double);
begin
  if (alpha>=0) and (alpha<=360) then Fa:=alpha;
end;

procedure T2DMover.setSpeedX(v1: double);
begin
  if abs(v1)<10 then
  begin
    if (v1<>0) or (vy<>0) then
    begin
      if vy>0 then Fa:=arccos(v1/sqrt(power(v1,2)+power(vy,2)))/2/pi*360  //Arkuskosinus austricksen :P
      else Fa:=180+arccos(-v1/sqrt(power(v1,2)+power(vy,2)))/2/pi*360;
    end;

    if (Fa=90) or (Fa=270) then Fv:=vy
    else Fv:=v1/cos(Fa/360*2*pi);
    if (v1=0) and (vy=0) then Fv:=0;
  end;
end;

procedure T2DMover.setSpeedY(v1: double);
begin
  if abs(v1)<10 then
  begin
    if (vx<>0) or (v1<>0) then
    begin
      if vx>0 then Fa:=arcsin(v1/sqrt(power(v1,2)+power(vx,2)))/2/pi*360 //360° here we go
      else Fa:=180+arcsin(-v1/sqrt(power(v1,2)+power(vx,2)))/2/pi*360;
    end;

    if (Fa=0) or (Fa=180) then Fv:=vx
    else Fv:=v1/sin(Fa/360*2*pi);
    if (vx=0) and (v1=0) then Fv:=0;
  end;
end;

function T2DMover.getSpeedX: double;
begin
  Result:=Fv*cos(Fa/360*2*pi);
end;

function T2DMover.getSpeedY: double;
begin
  Result:=Fv*sin(Fa/360*2*pi);
end;

end.

