unit levelObj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, bats, balls, blocks, GL;

type
  { TLevel }

  TLevel = class
    private
      FSpeed: double;
      initialized: boolean;
      procedure setSpeed(v: double);
      function countBalls: integer;
    public
      bat:TBat;
      ran: boolean;
      stack : array of TBlock;
      balls : array of TBall;

      rows: array of array [0..11] of integer;
      property Ballv: double read FSpeed write SetSpeed;
      procedure fillRow(row, a,b,c,d,e,f,g,h,i,j,k,l: integer);
      procedure addBall(x, y: single; a: double);
      procedure drawBlocks;
      procedure reset;
      procedure build;
      destructor Destroy; override;
  end;

implementation
uses main;

{ TLevel }

  procedure TLevel.fillRow(row, a, b, c, d, e, f, g, h, i, j, k, l: integer);
  begin
    rows[row][0]:=a;
    rows[row][1]:=b;
    rows[row][2]:=c;
    rows[row][3]:=d;
    rows[row][4]:=e;
    rows[row][5]:=f;
    rows[row][6]:=g;
    rows[row][7]:=h;
    rows[row][8]:=i;
    rows[row][9]:=j;
    rows[row][10]:=k;
    rows[row][11]:=l;
  end;

  procedure TLevel.build;
  var i,j: integer;
  begin
    if not initialized then begin
    for j:=0 to length(rows)-1 do
    begin
    for i:=0 to length(rows[j])-1 do if rows[j][i]<>0 then
    begin
      SetLength(stack, Length(stack)+1);
      stack[Length(stack)-1]:=TBlock.Create(i*95-570, -33*j+33/2*length(rows)+150, rows[j][i]);
    end;
    end;

    bat:=TBat.Create(-100);

    FSpeed:=2*animation;
    setLength(balls,1);
    balls[0]:=TBall.Create(FSpeed, 90);
    balls[0].stop;

    //addBall(-200,-205,1,0);       //Test
    //addBall(200,-200,1,180);

    initialized:=true;
    end
    else reset;
  end;

  procedure TLevel.setSpeed(v: double);
  var i: integer;
  begin
    for i:=0 to length(balls)-1 do begin
      balls[i].v:=v;
    end;
    FSpeed:=v;
  end;

  function TLevel.countBalls: integer;
  begin
    Result:=Length(Balls);
  end;

  procedure TLevel.addBall(x, y: single; a: double);
  begin
    setLength(balls, Length(balls)+1);
    balls[Length(balls)-1]:=TBall.Create(x, y, FSpeed, a);
  end;

  procedure TLevel.drawBlocks;
  var i, visible:integer;
  begin
    visible:=0;
    for i:=0 to length(stack)-1 do if stack[i].Visible then begin
      inc(visible);
      stack[i].draw;
    end;

    if visible=0 then with Form1 do begin
      for i:=0 to length(levels)-1 do if level=levels[i] then break;
      if i<4 then begin
        level:=levels[i+1];
        play(self);
      end
      else begin
        Form1.menu1.GameOver;
        level:=levels[0];
      end;
    end;
  end;

  procedure TLevel.reset;
  var i:integer;
  begin
    if Length(balls)>1 then for i:=1 to Length(balls)-1 do balls[i].Free;
    setLength(balls, 1);
    balls[0].stop;
    balls[0].reset;
    ran:=false;
    for i:=0 to Length(stack)-1 do stack[i].reset;
    bat.Width:=100;
  end;

  destructor TLevel.Destroy;
  var i:integer;
  begin
    for i:=0 to Length(balls)-1 do balls[i].Free;
    bat.Free;
    for i:=0 to Length(stack)-1 do stack[i].Free;

    inherited destroy;
  end;

end.

