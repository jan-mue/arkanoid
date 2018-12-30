unit root;

{$mode objfpc}{$H+}

interface

uses
  Classes, GL;

type

  { TGLObj }

  TGLObj = class
    private
      Fx, Fy, Fw, Fh: single;
      Fvisible: boolean;
    public
      property Left: single read Fx write Fx;
      property Top: single read Fy write Fy;
      property Width: single read Fw write Fw;
      property Height: single read Fh write Fh;
      property Visible: boolean read Fvisible write FVisible;
      procedure draw; virtual; abstract;
      procedure show;
      procedure hide;
      Constructor Create;
  end;

implementation

{ TGLObj }

procedure TGLObj.show;
begin
  FVisible:=true;
end;

procedure TGLObj.hide;
begin
  FVisible:=false;
end;

constructor TGLObj.Create;
begin
   Fx:=0;
   Fy:=0;
   Fw:=0;
   Fh:=0;
   Fvisible:= true;
end;

end.

