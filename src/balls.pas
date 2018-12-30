unit balls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, movers, Math, root;

type

  { TBall }

  TBall = class(T2DMover)
    public
      constructor Create(x, y:single; v0, a0: double);
      constructor Create(v0, a0: double); overload;
      procedure move; override;
      procedure reset;
      procedure draw; override;
      function collide(obj: TGLObj): boolean; override;
    protected
      procedure checkPos; override;
  end;

implementation
uses main, GL;

{ TBall }

constructor TBall.Create(x, y: single; v0, a0: double);
begin
  Create(v0, a0);
  Left:=x;
  Top:=y;
end;

constructor TBall.Create(v0, a0: double);
begin
  inherited Create;
  Width:=24;
  Height:=Width;
  v:=v0;
  a:=a0;
  resume;
end;

procedure TBall.checkPos;
var
  i, c:integer;
  dx, dy, da, alpha: double;
begin
  if (Left-Width/2+vx<=-640) or (Left+Width/2+vx>=640) then vx:=-vx;
  if (Top+Height/2+vy>=400) then vy:=-vy;      //or (Top-Height/2+vy<=-400)

  if collide(Form1.level.bat) then //Ball gefangen
  begin
    if (Left+Width/2+vx-Form1.level.bat.Left>0) and
    (Left-Width/2+vx-Form1.level.bat.Left<Form1.level.bat.Width) then
    a:= 140-(Left+vx-Form1.level.bat.Left)/Form1.level.bat.Width*100
    else OnCollusion(Form1.level.bat);
  end;

  if Top+Height/2<-400 then if length(Form1.level.balls)=1 then //Ball verloren
  begin
      Form1.lives.decrement;
      stop;
  end
  else hide;

  c:=-1;
  for i:=0 to Length(Form1.level.stack)-1 do      //Block getroffen
  begin
    if (collide(Form1.level.stack[i])) then
    begin
         Form1.level.stack[i].decRes;
         if c=-1 then c:=i
         else break;
    end;
  end;
  if c>=0 then OnCollusion(Form1.level.stack[c]);

  //anderen Ball getroffen
  for i:=0 to length(Form1.level.balls)-1 do begin
    dx:=Left+vx-Form1.level.balls[i].Left-Form1.level.balls[i].vx;
    dy:=Top+vy-Form1.level.balls[i].Top-Form1.level.balls[i].vy;
    if (Form1.level.balls[i]<>self) and (sqrt(power(dx,2)+power(dy,2))<=Width) then begin
      if dy>0 then da:=arccos(dx/sqrt(power(dx,2)+power(dy,2)))/2/pi*360
      else da:=180+arccos(-dx/sqrt(power(dx,2)+power(dy,2)))/2/pi*360;
      alpha:=2*da-a-540;
      if alpha<0 then alpha:=alpha+720;
      if alpha>=360 then alpha:=alpha-360;
      a:=alpha;
      alpha:=2*da-Form1.level.balls[i].a-540;
      if alpha<0 then alpha:=alpha+720;
      if alpha>=360 then alpha:=alpha-360;
      Form1.level.balls[i].a:=alpha;
    end;
  end;

end;

procedure TBall.move;
begin
  inherited move;
  if not running and Form1.level.bat.running then reset;
end;

procedure TBall.reset;
begin
  a:=90;
  Left:=Form1.level.bat.Left+Form1.level.bat.Width/2;
  Top:=Form1.level.bat.Top+Form1.level.bat.Height+Height/2;
end;

procedure TBall.draw;
begin
  move;
  if not Visible then Exit;
  glPushMatrix;
    glTranslatef(Left, Top, 15.0);
    glCallList(BallList);
  glPopMatrix;
end;

function TBall.collide(obj: TGLObj): boolean;
begin
  if (obj.Visible) and (Left+Width/2+vx>=obj.Left) and (Left-Width/2+vx<=obj.Left+obj.Width)
  and (Top+Height/2+vy>=obj.Top) and (Top-Height/2+vy<=obj.Top+obj.Height) then Result:=true
  else Result:=false;
end;

end.

