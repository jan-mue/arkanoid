unit bats;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls,
  {$IFDEF MSWINDOWS}
  windows,
  {$ENDIF}
  movers;
type

  { TBat }

  TBat = class(TMover)
    public
      constructor Create(x:single);
      procedure move; override;
      procedure draw; override;
  end;

implementation
uses main, GL;

{ TBat }

constructor TBat.Create(x: single);
begin
  inherited Create;
  Width:=100;
  Height:=15;
  Left:=x;
  Top:=-350;
  resume;
end;

procedure TBat.move;
var xcoor: single;
begin
  if not mcontrol then begin
   {$IFDEF MSWINDOWS}
   if getasynckeystate(VK_LEFT)<>0 then xcoor:=Left-2*animation
   else if getasynckeystate(VK_RIGHT)<>0 then xcoor:=Left+2*animation
   else xcoor:=Left;
   {$ENDIF}
  end
  else begin
  xcoor:=(Mouse.CursorPos.X-Form1.Left-Form1.OpenGLControl1.Left)
  /Form1.OpenGLControl1.Width*1280-640-Width/2;
  end;
  if running and (xcoor+Width<=640) and (xcoor>=-640) then Left:=xcoor;
end;

procedure TBat.draw;
begin
  move;
  if not Visible then Exit;
  glPushMatrix;
    glTranslatef(Left+Width/2, Top, 15);
    glScalef(Width, 2*Height, 1.02);
    glCallList(BatList);
  glPopMatrix;
end;

end.

