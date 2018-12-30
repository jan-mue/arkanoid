unit counters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, root;

type

  { TCounter }

  TCounter = class(TGLObj)
    private
      value:integer;
      procedure drawNum(num: integer);
    public
      property CurrentValue: integer read value;
      procedure reset; virtual;
      procedure draw; override;
      Constructor Create;
  end;

  { TPoints }

  TScore = class(TCounter)
    private
      FMulti:integer;
      procedure setMultiplicator(m:integer);
    public
      property Multiplicator:Integer read FMulti write setMultiplicator;
      procedure add(points:integer);
      Constructor Create;
  end;

  { TLives }

  TLives = class(TCounter)
    private
      FStart:Integer;
    public
      procedure reset; override;
      procedure increment;
      procedure decrement;
      Constructor Create(StartLifes:Integer);
  end;

implementation
uses main, GL;

{ TLives }

procedure TLives.reset;
begin
  value:=FStart;
end;

procedure TLives.increment;
begin
  inc(value);
end;

procedure TLives.decrement;
begin
  if value>0 then dec(value);
end;

constructor TLives.Create(StartLifes: Integer);
begin
  FStart:=StartLifes;
  inherited Create;
end;

{ TScore }

procedure TScore.setMultiplicator(m: integer);
begin
  if (m=2) or (m=5) or (m=10) or (m=20) or (m=42) then FMulti:=m;
end;

procedure TScore.add(points: integer);
begin
  if points>0 then value:=value+points*FMulti;
end;

constructor TScore.Create;
begin
  inherited Create;
  FMulti:=1;
end;

{ TCounter }

procedure TCounter.reset;
begin
  value:=0;
end;

procedure TCounter.drawNum(num: integer);
var
  i:integer;
  s:string;
begin
  s:=IntToStr(num);
  glEnable(GL_BLEND);
  for i:=1 to length(s) do begin
    glPushMatrix;
    glTranslatef((i-1)*20,0,0);
    glCullFace(GL_BACK);

  if StrToInt(s[i])=0 then begin
     glRotatef(90, 1, 0, 0);
     glCallList(n90List);
  end
  else if StrToInt(s[i])<4 then begin
     glRotatef(StrToInt(s[i])*90, 1, 0, 0);
     glCallList(n03List);
  end
  else if StrToInt(s[i])<7 then begin
     glRotatef((StrToInt(s[i])-3)*90, 1, 0, 0);
     glCallList(n36List);
  end
  else if StrToInt(s[i])<10 then begin
     glRotatef((StrToInt(s[i])-6)*90, 1, 0, 0);
     glCallList(n69List);
  end;
  glPopMatrix;
  end;
  glDisable(GL_BLEND);
end;

procedure TCounter.draw;
begin
  if not Visible then Exit;
  glPushMatrix;
    glTranslatef(Left, Top, 50.0);
    drawNum(CurrentValue);
  glPopMatrix;
end;

constructor TCounter.Create;
begin
  inherited Create;
  reset;
end;

end.

