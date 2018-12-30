unit blocks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, root, falling;

type

  { TBlock }

  TBlock = class(TGLObj)
    private
      Fres, Fr: integer;
      Fz, x0, y0: single;
      Fhit, decr: boolean;
      fall: TFallingObj;
    public
      property hit: boolean read Fhit;
      property z: single read Fz;
      property res: integer read Fres;
      procedure reset;
      procedure decRes;
      procedure animate;
      procedure draw; override;
      Constructor Create(x, y: single; r:integer);
      Destructor Destroy; override;
  end;

implementation
uses main, GL;

{ TBlock }

constructor TBlock.Create(x, y: single; r: integer);
begin
  inherited Create;
  Left:=x;
  Top:=y;
  x0:=x;
  y0:=y;
  Height:=30;
  Width:=90;
  Fz:=0;
  Fhit:=false;
  if r>0 then Fres:=r;
  Fr:=Fres;
  fall:= TFallingObj.Create(Left, Top, Width, Height);
  fall.stop;
end;

destructor TBlock.Destroy;
begin
  fall.Free;
  inherited Destroy;
end;

procedure TBlock.reset;
begin
  Fres:=Fr;
  Fz:=0;
  decr:=false;
  if fall<>nil then fall.reset;
  Left:=x0;
  Top:=y0;
  show;
end;

procedure TBlock.decRes;
begin
  Fhit:=true;
  dec(Fres);
end;

procedure TBlock.animate;
begin
  if Fz>=20 then decr:=true;
  if not decr then Fz:= Fz+animation
  else if (Fz<=0) and (Fz>-32) and (Fr = 2) and (Fres<=0) then begin
    Fhit:=false;
    decr:=false;
    if not fall.running then begin
      Form1.score.add(100);
      fall.reset;
    end;
    fall.resume;
  end
  else if (Fz>0) or ((Fres<=0) and (Fz>-32)) then Fz:= Fz-animation
  else if Fres<=0 then begin
    hide;
    Fhit:=false;
    Form1.score.add(100);
    if Fr = 3 then Form1.level.addBall(Left+45, Top+15, 270)
  end
  else begin
    decr:=false;
    Fhit:=false;
  end;
end;

procedure TBlock.draw;
begin
  if not Visible then Exit;
  if hit then animate;
  if fall.running then begin
    fall.draw;
    Left:=fall.Left;
    Top:=fall.Top;
    Visible:=fall.Visible;
  end;
  glPushMatrix;
      glCullFace(GL_BACK);

      if res<=1 then glColor3f(0.3, 0.0, 0.0)
      else if res=2 then glColor3f(0.0, 0.2, 0.0)
      else if res=3 then glColor3f(0.2, 0.15, 0.0);

      glTranslatef(Left, Top, z);
      glCallList(BlockList);
  glPopMatrix;
end;

end.

