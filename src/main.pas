unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LCLProc, Forms, LResources, ExtCtrls, Dialogs,
  Graphics, IntfGraphics, GL, FPimage, OpenGLContext, glu,
  {$IFDEF MSWINDOWS}
  windows, MMSystem,
  {$ENDIF}
  Controls, counters, menu, options, levelobj;

const
  GL_CLAMP_TO_EDGE = $812F;

type
  TglTexture = class
  public
    Width,Height: longint;
    Data        : pointer;
    destructor Destroy; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
  public
    OpenGLControl1: TOpenGLControl;

    score:TScore;
    lives:TLives;

    menu1:TMenu;

    level:TLevel;
    levels: array[0..4] of TLevel;

    procedure SwitchFullScreen;
    procedure SwitchLight;
    procedure FormClose(Sender: TObject);
    procedure play(Sender: TObject);
    procedure load1(Sender: TObject);
    procedure load2(Sender: TObject);
    procedure load3(Sender: TObject);
    procedure load4(Sender: TObject);

    {$IFDEF MSWINDOWS}
    procedure ComponentMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer) ;
    {$ENDIF}

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  private
    OriginalBounds: TRect;
    OriginalWindowState: TWindowState;
    ScreenBounds: TRect;

    AreaInitialized: boolean;
    FrameCount: integer;
    LastFrameTicks: integer;
    Frames: String;

    timer: single;
    LastMsecs: integer;

    rry, dist, viewx: single;
    smenu, mmove: boolean;
    textures       : array [0..4] of GLuint;    // Storage For 5 Textures
    MyglTextures   : array [0..4] of TglTexture;
    lightamb, lightdif, lightpos, matspecular, matambient, matdiffuse,
      ballspecular, ballambient, balldiffuse, batspecular, batambient,
      batdiffuse: array [0..3] of GLfloat;
    matshininess, ballshininess, batshininess: array [0..0] of GLfloat;
    HeartList, MenuList, BackList: GLuint;

    procedure LoadTextures;
    procedure IdleFunc(Sender: TObject; var Done: Boolean);
    procedure FormResize(Sender: TObject);
    procedure OpenGLControl1Paint(Sender: TObject);
    procedure OpenGLControl1Resize(Sender: TObject);
    procedure OpenGLControl1Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    {$IFDEF MSWINDOWS}
    procedure WMNCHitTest(var Msg: TWMNCHitTest) ; message WM_NCHitTest;
    {$ENDIF}
  end;

var Form1: TForm1;
    Form2: TForm2;

    BallList, BlockList, BatList, n03List, n36List, n69List, n90List: GLuint;

    animation: single;
    fullscreen, lighting, sound, mcontrol: boolean;

    function LoadFileToMemStream(const Filename: string): TMemoryStream;
    function LoadglTexImage2DFromPNG(PNGFilename:string;
      Image: TglTexture): boolean;

    implementation


    function LoadFileToMemStream(const Filename: string): TMemoryStream;
    var FileStream: TFileStream;
    begin
      Result:=TMemoryStream.Create;
      try
        FileStream:=TFileStream.Create(UTF8ToSys(Filename), fmOpenRead);
        try
          Result.CopyFrom(FileStream,FileStream.Size);
          Result.Position:=0;
        finally
          FileStream.Free;
        end;
      except
        Result.Free;
        Result:=nil;
      end;
    end;

    function LoadglTexImage2DFromPNG(PNGFilename: string; Image: TglTexture
      ): boolean;
    var
      png: TPortableNetworkGraphic;
      IntfImg: TLazIntfImage;
      y: Integer;
      x: Integer;
      c: TFPColor;
      p: PByte;
    begin
      Result:=false;
      png:=TPortableNetworkGraphic.Create;
      IntfImg:=nil;
      try
        png.LoadFromFile(PNGFilename);
        IntfImg:=png.CreateIntfImage;
        Image.Width:=IntfImg.Width;
        Image.Height:=IntfImg.Height;
        GetMem(Image.Data,Image.Width*Image.Height * 3);
        p:=PByte(Image.Data);
        for y:=0 to IntfImg.Height-1 do begin
          for x:=0 to IntfImg.Width-1 do begin
            c:=IntfImg.Colors[x,y];
            p^:=c.red shr 8;
            inc(p);
            p^:=c.green shr 8;
            inc(p);
            p^:=c.blue shr 8;
            inc(p);
          end;
        end;
      finally
        png.Free;
        IntfImg.Free;
      end;
      Result:=true;
    end;

{ TForm1 }

constructor TForm1.Create(TheOwner: TComponent);
var
  i:integer;
begin
  inherited CreateNew(TheOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    SetBounds((Screen.Width-1280) div 2,(Screen.Height-800) div 2,1280,800);
    Caption:='';
    BorderStyle:=bsNone;

    Application.OnIdle:=@IdleFunc;
    OnResize:=@FormResize;
    OnKeyPress:=@FormKeyPress;

    {$IFDEF MSWINDOWS}
    AddFontResource('../config/resources/Roboto-Thin.ttf');
    AddFontResource('../config/resources/Roboto-Bold.ttf');
    AddFontResource('../config/resources/Starjout.ttf');
    sndPlaySound('../config/sounds/title.wav', SND_ASYNC or SND_LOOP);
    {$ENDIF}

    Application.CreateForm(TForm2, Form2);

    menu1:=TMenu.Create(self);

    //load levels:
    for i:=0 to 4 do Levels[i]:=TLevel.Create;

    with Levels[0] do begin
    setLength(rows,7);
    fillRow(0, 0,0,0,0,0,1,1,0,0,0,0,0);
    fillRow(1, 0,0,0,1,1,2,2,1,1,0,0,0);
    fillRow(2, 0,0,1,2,2,3,3,2,2,1,0,0);
    fillRow(3, 0,1,2,2,3,3,3,3,2,2,1,0);
    fillRow(4, 0,0,1,2,2,3,3,2,2,1,0,0);
    fillRow(5, 0,0,0,1,1,2,2,1,1,0,0,0);
    fillRow(6, 0,0,0,0,0,1,1,0,0,0,0,0);
    end;

     with Levels[1] do begin
    setLength(rows,12);
    fillRow(0, 0,0,0,0,2,0,0,0,2,0,0,0);
    fillRow(1, 0,0,0,0,0,2,2,2,0,0,0,0);
    fillRow(2, 0,0,0,0,2,1,2,1,2,0,0,0);
    fillRow(3, 0,0,0,0,2,2,2,2,2,0,0,0);
    fillRow(4, 0,0,0,2,2,2,2,2,2,2,0,0);
    fillRow(5, 0,0,0,0,2,2,2,2,2,0,0,0);
    fillRow(6, 0,0,0,0,2,2,2,2,2,0,0,0);
    fillRow(7, 0,0,0,0,2,2,2,2,2,0,0,0);
    fillRow(8, 0,0,0,0,2,2,2,2,2,0,0,0);
    fillRow(9, 0,0,0,0,2,2,2,2,2,0,0,0);
    fillRow(10,0,0,0,0,0,2,0,2,0,0,0,0);
    fillRow(11,0,0,0,0,0,2,0,2,0,0,0,0);
    end;

    with Levels[2] do begin
    setLength(rows,12);
    fillRow(0, 0,1,1,1,1,1,1,1,1,1,1,0);
    fillRow(1, 0,1,1,1,3,1,1,3,3,1,1,0);
    fillRow(2, 0,1,1,3,3,1,3,1,1,3,1,0);
    fillRow(3, 0,1,1,3,3,1,1,1,1,3,1,0);
    fillRow(4, 0,1,3,1,3,1,1,1,3,1,1,0);
    fillRow(5, 0,1,3,1,3,1,1,1,3,1,1,0);
    fillRow(6, 0,1,3,3,3,3,1,3,1,1,1,0);
    fillRow(7, 0,1,3,3,3,3,1,3,1,1,1,0);
    fillRow(8, 0,1,1,1,3,1,3,1,1,1,1,0);
    fillRow(9, 0,1,1,1,3,1,3,1,1,1,1,0);
    fillRow(10,0,1,1,1,3,1,3,3,3,3,1,0);
    fillRow(11,0,1,1,1,1,1,1,1,1,1,1,0);
    end;

    with Levels[3] do begin
    setLength(rows,12);
    fillRow(0, 1,1,1,0,0,0,0,0,0,1,1,1);
    fillRow(1, 1,1,0,0,0,0,0,0,0,0,1,1);
    fillRow(2, 1,0,0,3,3,0,0,3,3,0,0,1);
    fillRow(3, 0,0,0,3,3,0,0,3,3,0,0,0);
    fillRow(4, 0,0,0,3,3,0,0,3,3,0,0,0);
    fillRow(5, 0,0,0,0,0,0,0,0,0,0,0,0);
    fillRow(6, 0,0,0,0,0,0,0,0,0,0,0,0);
    fillRow(7, 0,0,2,2,2,2,2,2,2,2,0,0);
    fillRow(8, 0,0,2,2,2,2,2,2,2,2,0,0);
    fillRow(9, 1,0,0,0,0,0,0,0,0,0,0,1);
    fillRow(10,1,1,0,0,0,0,0,0,0,0,1,1);
    fillRow(11,1,1,1,0,0,0,0,0,0,1,1,1);
    end;

    with Levels[4] do begin
    setLength(rows,12);
    fillRow(0, 0,0,2,2,2,2,2,2,2,2,0,0);
    fillRow(1, 0,0,2,1,1,2,2,1,1,2,0,0);
    fillRow(2, 0,0,2,1,1,2,2,1,1,2,0,0);
    fillRow(3, 0,0,2,1,1,2,2,1,1,2,0,0);
    fillRow(4, 0,0,2,2,2,1,1,2,2,2,0,0);
    fillRow(5, 0,0,2,2,2,1,1,2,2,2,0,0);
    fillRow(6, 0,0,2,2,1,1,1,1,2,2,0,0);
    fillRow(7, 0,0,2,2,1,1,1,1,2,2,0,0);
    fillRow(8, 0,0,2,2,1,1,1,1,2,2,0,0);
    fillRow(9, 0,0,2,2,1,2,2,1,2,2,0,0);
    fillRow(10,0,0,2,2,1,2,2,1,2,2,0,0);
    fillRow(11,0,0,2,2,2,2,2,2,2,2,0,0);
    end;

    //load frame:
    score:=TScore.Create;
    score.Left:=-600;
    score.Top:=390;

    lives:=TLives.Create(3);
    lives.Left:=580;
    lives.Top:=390;

// resize the components first, because the opengl context needs some time to setup
    FormResize(Self);

    OpenGLControl1:=TOpenGLControl.Create(Self);
    with OpenGLControl1 do begin
      Name:='OpenGLControl1';
      SetBounds(0, 0, Width, Height);
      OnPaint:=@OpenGLControl1Paint;
      OnResize:=@OpenGLControl1Resize;
      OnClick:=@OpenGLControl1Click;
      OnKeyPress:=@FormKeyPress;

      Parent:=Self;
      Visible:=false;
    end;

  end;

  LoadTextures;
end;

procedure TForm1.play(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
    if sound then sndPlaySound('../config/sounds/cave.wav', SND_ASYNC or SND_LOOP);
  {$ENDIF}
  rry:=0;
  dist:=0;
  viewx:=0;
  smenu:=false;
  mmove:=false;
  menu1.Visible:=false;
  if level=nil then level:=Levels[0];
  level.build;
  if not OpenGLControl1.Visible then begin
    score.reset;
    lives.reset;
  end;
  score.show;
  OpenGLControl1.Visible:=true;
  level.bat.resume;
  FormResize(Self);
end;

procedure TForm1.load1(Sender: TObject);
begin
  level:=levels[1];
  play(Sender);
end;

procedure TForm1.load2(Sender: TObject);
begin
  level:=levels[2];
  play(Sender);
end;

procedure TForm1.load3(Sender: TObject);
begin
  level:=levels[3];
  play(Sender);
end;

procedure TForm1.load4(Sender: TObject);
begin
  level:=levels[4];
  play(Sender);
end;

destructor TForm1.Destroy;
var i: integer;
begin
  for i:=0 to 4 do begin
    Textures[i]:=0;
    FreeAndNil(MyglTextures[i]);
  end;

  score.Free;
  lives.Free;
  for i:=0 to 4 do Levels[i].Free;
  menu1.Free;

  inherited Destroy;
end;

procedure TForm1.SwitchFullScreen;
begin
  if not fullscreen then begin
    // To full screen
    OriginalWindowState := WindowState;
    OriginalBounds := BoundsRect;
    BorderStyle:=bsNone;

    ScreenBounds := Screen.MonitorFromWindow(Handle).BoundsRect;
    with ScreenBounds do
      SetBounds(Left, Top, Right - Left, Bottom - Top) ;
  end else begin
    // From full screen
    if OriginalWindowState = wsMaximized then
      WindowState := wsMaximized
    else
      with OriginalBounds do
        SetBounds(Left, Top, Right - Left, Bottom - Top) ;
  end;
  fullscreen:= not fullscreen;
end;

procedure TForm1.SwitchLight;
begin
  if OpenGLControl1.Visible then begin
    if lighting then glDisable(GL_LIGHTING)
    else glEnable(GL_LIGHTING);
  end;
  lighting:= not lighting;
end;

procedure TForm1.LoadTextures;

  procedure LoadglTexture(Filename:string; Image:TglTexture);
  begin
    Filename:=ExpandFileNameUTF8(Filename);
    if not LoadglTexImage2DFromPNG(Filename,Image) then begin
      MessageDlg('File not found',
        'Image file not found: '+Filename,
        mtError,[mbOk],0);
      raise Exception.Create('Image file not found: '+Filename);
    end;
  end;

var
  i: Integer;
begin
  for i:=0 to 4 do begin
    Textures[i]:=0;
    MyglTextures[i]:=TglTexture.Create;
  end;
  {loading the texture and setting its parameters}

  LoadglTexture('../config/images/stars.png',MyglTextures[0]);
  LoadglTexture('../config/images/metal.png',MyglTextures[1]);
  LoadglTexture('../config/images/menu.png',MyglTextures[2]);
  LoadglTexture('../config/images/heart.png',MyglTextures[3]);
  LoadglTexture('../config/images/numbers.png',MyglTextures[4]);
end;

procedure TForm1.IdleFunc(Sender: TObject; var Done: Boolean);
begin
  OpenGLControl1.Invalidate;
  //OpenGLControl1Paint(Self);
  Done:=false; // tell lcl to handle messages and return immediatly
end;

procedure TForm1.FormResize(Sender: TObject);
var w, h: integer;
begin
  h:=Height;
  w:=round(h*16/10);
  if w>Width then begin
    w:=Width;
    h:=round(w*10/16);
  end;

  if (OpenGLControl1<>nil) and OpenGLControl1.Visible then
    OpenGLControl1.SetBounds((Width-w) div 2, (Height-h) div 2, w, h);

  if menu1<>nil then menu1.Resize;
end;

procedure TForm1.FormClose(Sender: TObject);
begin
  Close;
end;

{$IFDEF MSWINDOWS}
procedure TForm1.WMNCHitTest(var Msg: TWMNCHitTest) ;
 begin
    inherited;
    if not fullscreen and (Msg.Result = htClient) then Msg.Result := htCaption;
 end;

procedure TForm1.ComponentMouseDown
    (Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer) ;
 begin
   if not fullscreen then begin
      ReleaseCapture;
      SendMessage(Form1.Handle, WM_SYSCOMMAND, 61458, 0);
    end;
 end;
{$ENDIF}

procedure TForm1.OpenGLControl1Paint(Sender: TObject);

  procedure myInit;
  begin
    {init lighting variables}
    {ambient color}
    lightamb[0]:=0.8;
    lightamb[1]:=0.8;
    lightamb[2]:=0.8;
    lightamb[3]:=1.0;
    {diffuse color}
    lightdif[0]:=1.0;
    lightdif[1]:=1.0;
    lightdif[2]:=1.0;
    lightdif[3]:=1.0;
    {diffuse position}
    lightpos[0]:=500.0;
    lightpos[1]:=-300.0;
    lightpos[2]:=500.0;
    lightpos[3]:=0.0;

    {block reflection}
    matspecular[0]:=0.73;
    matspecular[1]:=0.63;
    matspecular[2]:=0.63;
    matspecular[3]:=0.5;
    {block shininess}
    matshininess[0]:=76.8;
    {block ambient}
    matambient[0]:=0.17;
    matambient[1]:=0.01;
    matambient[2]:=0.01;
    matambient[3]:=0.5;
    {block diffuse}
    matdiffuse[0]:=0.61;
    matdiffuse[1]:=0.04;
    matdiffuse[2]:=0.04;
    matdiffuse[3]:=0.5;

    {ball reflection}
    ballspecular[0]:=0.774597;
    ballspecular[1]:=0.774597;
    ballspecular[2]:=0.774597;
    ballspecular[3]:=1.0;
    {ball shininess}
    ballshininess[0]:=76.8;
    {ball ambient}
    ballambient[0]:=0.25;
    ballambient[1]:=0.25;
    ballambient[2]:=0.25;
    ballambient[3]:=1.0;
    {ball diffuse}
    balldiffuse[0]:=0.4;
    balldiffuse[1]:=0.4;
    balldiffuse[2]:=0.4;
    balldiffuse[3]:=1.0;

    {bat reflection}
    batspecular[0]:=0.774597;
    batspecular[1]:=0.774597;
    batspecular[2]:=0.774597;
    batspecular[3]:=1.0;
    {bat shininess}
    batshininess[0]:=89.6;
    {bat ambient}
    batambient[0]:=0.23;
    batambient[1]:=0.23;
    batambient[2]:=0.23;
    batambient[3]:=1.0;
    {ball diffuse}
    batdiffuse[0]:=0.28;
    batdiffuse[1]:=0.28;
    batdiffuse[2]:=0.28;
    batdiffuse[3]:=1.0;

  end;

const GLInitialized: boolean = false;

procedure InitGL;
var
  i: Integer;
  w,h,d: single;
  sphere:PGLUquadric;
begin
  if GLInitialized then exit;
  GLInitialized:=true;

  {setting lighting conditions}
  glLightfv(GL_LIGHT0,GL_AMBIENT,lightamb);
  glLightfv(GL_LIGHT1,GL_AMBIENT,lightamb);
  glLightfv(GL_LIGHT2,GL_DIFFUSE,lightdif);
  glLightfv(GL_LIGHT2,GL_POSITION,lightpos);
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHT1);
  glEnable(GL_LIGHT2);

  if lighting then glEnable(GL_LIGHTING);
  glEnable(GL_COLOR_MATERIAL);

  glGenTextures(5, @textures[0]);
  for i:=0 to 4 do begin
    glBindTexture(GL_TEXTURE_2D, Textures[i]);
    glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
    glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D,0,3,MyglTextures[i].Width,MyglTextures[i].Height,0
        ,GL_RGB,GL_UNSIGNED_BYTE,MyglTextures[i].Data);
  end;
  glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
  {instead of GL_MODULATE you can try GL_DECAL or GL_BLEND}
  glEnable(GL_TEXTURE_2D);          // enables 2d textures
  glClearColor(0.0,0.0,0.0,1.0);    // sets background color
  glClearDepth(1.0);
  glDepthFunc(GL_LEQUAL);           // the type of depth test to do
  glEnable(GL_DEPTH_TEST);          // enables depth testing
  glShadeModel(GL_SMOOTH);          // enables smooth color shading
  {blending}
  glColor4f(1.0,1.0,1.0,0.5);       // Full Brightness, 50% Alpha
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE);
  glHint(GL_LINE_SMOOTH_HINT,GL_NICEST);
  glHint(GL_POLYGON_SMOOTH_HINT,GL_NICEST);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);

  // creating display lists

  BackList:=glGenLists(9);
  glNewList(BackList, GL_COMPILE);
    glBindTexture(GL_TEXTURE_2D, textures[0]);
    glBegin(GL_QUADS);
      {Back Face}
      glNormal3f( 0.0, 0.0,-1.0);
      glTexCoord2f( 0.0, 1.0);     glVertex3f( 1280, 800,0.0);
      glTexCoord2f( 0.0, 0.0);     glVertex3f( 1280,0.0,0.0);
      glTexCoord2f( 1.0, 0.0);     glVertex3f(0.0,0.0,0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f(0.0, 800,0.0);
    glEnd;

    glBindTexture(GL_TEXTURE_2D, textures[1]);
    glBegin(GL_QUADS);
      {Left Face}
      glNormal3f(-1.0, 0.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f(0.0, 800, 1067);
      glTexCoord2f( 0.0, 1.0);     glVertex3f(0.0, 800 , 0.0);
      glTexCoord2f( 0.0, 0.0);     glVertex3f(0.0, 0.0, 0.0);
      glTexCoord2f( 1.0, 0.0);     glVertex3f(0.0, 0.0, 1067);
      {Right Face}
      glNormal3f( 1.0, 0.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f( 1280, 800,0.0);
      glTexCoord2f( 0.0, 1.0);     glVertex3f( 1280, 800, 1067);
      glTexCoord2f( 0.0, 0.0);     glVertex3f( 1280,0.0, 1067);
      glTexCoord2f( 1.0, 0.0);     glVertex3f( 1280,0.0,0.0);
      {Top Face}
      glNormal3f( 0.0, 1.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f( 1280, 800,0.0);
      glTexCoord2f( 0.0, 1.0);     glVertex3f(0.0, 800,0.0);
      glTexCoord2f( 0.0, 0.0);     glVertex3f(0.0, 800, 960);
      glTexCoord2f( 1.0, 0.0);     glVertex3f( 1280, 800, 960);
    glEnd;
  glEndList;

  MenuList:=BackList+1;
  glNewList(MenuList, GL_COMPILE);
    glBindTexture(GL_TEXTURE_2D, textures[2]);
    glBegin(GL_QUADS);
      glNormal3f( -1.0, 0.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f( 0.0, 600, 400);
      glTexCoord2f( 0.0, 1.0);     glVertex3f( 0.0, 600, 0.0);
      glTexCoord2f( 0.0, 0.0);     glVertex3f( 0.0, 0.0, 0.0);
      glTexCoord2f( 1.0, 0.0);     glVertex3f( 0.0, 0.0, 400);
    glEnd;
  glEndList;

  w:=level.stack[0].Width;
  h:=level.stack[0].Height;
  d:=h;
  BlockList:=MenuList+1;
  glNewList(BlockList, GL_COMPILE);
    glMaterialfv(GL_FRONT, GL_SPECULAR,  matspecular);
    glMaterialfv(GL_FRONT, GL_SHININESS, matshininess);
    glMaterialfv(GL_FRONT, GL_AMBIENT,   matambient);
    glMaterialfv(GL_FRONT, GL_DIFFUSE,   matdiffuse);

    glBegin(GL_QUADS);
      {Front Face}
      glNormal3f( 0.0, 0.0, 1.0);
      glVertex3f( w, h, d);
      glVertex3f(0.0, h, d);
      glVertex3f(0.0,0.0, d);
      glVertex3f( w,0.0, d);
      {Back Face}
      glNormal3f( 0.0, 0.0,-1.0);
      glVertex3f( w, h,0.0);
      glVertex3f( w,0.0,0.0);
      glVertex3f(0.0,0.0,0.0);
      glVertex3f(0.0, h,0.0);
      {Left Face}
      glNormal3f(-1.0, 0.0, 0.0);
      glVertex3f(0.0, h, d);
      glVertex3f(0.0, h,0.0);
      glVertex3f(0.0,0.0,0.0);
      glVertex3f(0.0,0.0, d);
      {Right Face}
      glNormal3f( 1.0, 0.0, 0.0);
      glVertex3f( w, h,0.0);
      glVertex3f( w, h, d);
      glVertex3f( w,0.0, d);
      glVertex3f( w,0.0,0.0);
      {Top Face}
      glNormal3f( 0.0, 1.0, 0.0);
      glVertex3f( w, h,0.0);
      glVertex3f(0.0, h,0.0);
      glVertex3f(0.0, h, d);
      glVertex3f( w, h, d);
      {Bottom Face}
      glNormal3f( 0.0,-1.0, 0.0);
      glVertex3f(0.0,0.0,0.0);
      glVertex3f( w,0.0,0.0);
      glVertex3f( w,0.0, d);
      glVertex3f(0.0,0.0, d);
    glEnd;
  glEndList;

  BatList:=BlockList+1;
  glNewList(BatList, GL_COMPILE);
    glMaterialfv(GL_FRONT, GL_SPECULAR,  batspecular);
    glMaterialfv(GL_FRONT, GL_SHININESS, batshininess);
    glMaterialfv(GL_FRONT, GL_AMBIENT,   batambient);
    glMaterialfv(GL_FRONT, GL_DIFFUSE,   batdiffuse);

    glColor3f(0.12,0.12,0.12);
    sphere:=gluNewQuadric();
    gluQuadricDrawStyle(sphere, GLU_FILL);
    gluQuadricNormals(sphere, GLU_SMOOTH);
    gluSphere(sphere, 0.5, 100, 100);
  glEndList;

  HeartList:=BatList+1;
  glNewList(HeartList, GL_COMPILE);
    glBindTexture(GL_TEXTURE_2D, textures[3]);
    glBegin(GL_POLYGON);
      glNormal3f( 0.0, 0.0, 1.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f( 25.0, 23, 0.0);
      glTexCoord2f( 0.0, 1.0);     glVertex3f(0.0, 23, 0.0);
      glTexCoord2f( 0.0, 0.5);     glVertex3f(0.0, 11.5, 0.0);
      glTexCoord2f( 0.5, 0.0);     glVertex3f(12.5, 0.0, 0.0);
      glTexCoord2f( 1.0, 0.5);     glVertex3f(25.0, 11.5, 0.0);
    glEnd;
  glEndList;

  BallList:=HeartList+1;
  glNewList(BallList, GL_COMPILE);
    glMaterialfv(GL_FRONT, GL_SPECULAR,  ballspecular);
    glMaterialfv(GL_FRONT, GL_SHININESS, ballshininess);
    glMaterialfv(GL_FRONT, GL_AMBIENT,   ballambient);
    glMaterialfv(GL_FRONT, GL_DIFFUSE,   balldiffuse);

    glColor3f(0.16,0.14,0.12);
    sphere:=gluNewQuadric();
    gluQuadricDrawStyle(sphere, GLU_FILL);
    gluQuadricNormals(sphere, GLU_SMOOTH);
    gluSphere(sphere, level.balls[0].Width/2, 50, 50);
  glEndList;

  w:=20;
  h:=w;
  d:=h;

  n03List:=BallList+1;
  glNewList(n03List, GL_COMPILE);
  glBindTexture(GL_TEXTURE_2D, textures[4]);
    glBegin(GL_QUADS);
      {Front Face}
      glNormal3f( 0.0, 0.0, 1.0);
      glTexCoord2f( 1.0, 1/11);   glVertex3f( w/2, h/2, d/2);
      glTexCoord2f( 0.0, 1/11);   glVertex3f(-w/2, h/2, d/2);
      glTexCoord2f( 0.0, 0.0);    glVertex3f(-w/2,-h/2, d/2);
      glTexCoord2f( 1.0, 0.0);    glVertex3f( w/2,-h/2, d/2);
      {Back Face}
      glNormal3f( 0.0, 0.0,-1.0);
      glTexCoord2f( 1.0, 2/11);   glVertex3f( w/2, h/2,-d/2);
      glTexCoord2f( 1.0, 3/11);   glVertex3f( w/2,-h/2,-d/2);
      glTexCoord2f( 0.0, 3/11);   glVertex3f(-w/2,-h/2,-d/2);
      glTexCoord2f( 0.0, 2/11);   glVertex3f(-w/2, h/2,-d/2);
      {Top Face}
      glNormal3f( 0.0, 1.0, 0.0);
      glTexCoord2f( 1.0, 2/11);   glVertex3f( w/2, h/2,-d/2);
      glTexCoord2f( 0.0, 2/11);   glVertex3f(-w/2, h/2,-d/2);
      glTexCoord2f( 0.0, 1/11);   glVertex3f(-w/2, h/2, d/2);
      glTexCoord2f( 1.0, 1/11);   glVertex3f( w/2, h/2, d/2);
      {Bottom Face}
      glNormal3f( 0.0,-1.0, 0.0);
      glTexCoord2f( 0.0, 3/11);   glVertex3f(-w/2,-h/2,-d/2);
      glTexCoord2f( 1.0, 3/11);   glVertex3f( w/2,-h/2,-d/2);
      glTexCoord2f( 1.0, 4/11);   glVertex3f( w/2,-h/2, d/2);
      glTexCoord2f( 0.0, 4/11);   glVertex3f(-w/2,-h/2, d/2);
    glEnd;
  glEndList;

  n36List:=n03List+1;
  glNewList(n36List, GL_COMPILE);
  glBindTexture(GL_TEXTURE_2D, textures[4]);
    glBegin(GL_QUADS);
      {Front Face}
      glNormal3f( 0.0, 0.0, 1.0);
      glTexCoord2f( 1.0, 4/11);   glVertex3f( w/2, h/2, d/2);
      glTexCoord2f( 0.0, 4/11);   glVertex3f(-w/2, h/2, d/2);
      glTexCoord2f( 0.0, 3/11);   glVertex3f(-w/2,-h/2, d/2);
      glTexCoord2f( 1.0, 3/11);   glVertex3f( w/2,-h/2, d/2);
      {Back Face}
      glNormal3f( 0.0, 0.0,-1.0);
      glTexCoord2f( 1.0, 5/11);   glVertex3f( w/2, h/2,-d/2);
      glTexCoord2f( 1.0, 6/11);   glVertex3f( w/2,-h/2,-d/2);
      glTexCoord2f( 0.0, 6/11);   glVertex3f(-w/2,-h/2,-d/2);
      glTexCoord2f( 0.0, 5/11);   glVertex3f(-w/2, h/2,-d/2);
      {Top Face}
      glNormal3f( 0.0, 1.0, 0.0);
      glTexCoord2f( 1.0, 5/11);   glVertex3f( w/2, h/2,-d/2);
      glTexCoord2f( 0.0, 5/11);   glVertex3f(-w/2, h/2,-d/2);
      glTexCoord2f( 0.0, 4/11);   glVertex3f(-w/2, h/2, d/2);
      glTexCoord2f( 1.0, 4/11);   glVertex3f( w/2, h/2, d/2);
      {Bottom Face}
      glNormal3f( 0.0,-1.0, 0.0);
      glTexCoord2f( 0.0, 6/11);   glVertex3f(-w/2,-h/2,-d/2);
      glTexCoord2f( 1.0, 6/11);   glVertex3f( w/2,-h/2,-d/2);
      glTexCoord2f( 1.0, 7/11);   glVertex3f( w/2,-h/2, d/2);
      glTexCoord2f( 0.0, 7/11);   glVertex3f(-w/2,-h/2, d/2);
    glEnd;
  glEndList;

  n69List:=n36List+1;
  glNewList(n69List, GL_COMPILE);
  glBindTexture(GL_TEXTURE_2D, textures[4]);
    glBegin(GL_QUADS);
      {Front Face}
      glNormal3f( 0.0, 0.0, 1.0);
      glTexCoord2f( 1.0, 7/11);   glVertex3f( w/2, h/2, d/2);
      glTexCoord2f( 0.0, 7/11);   glVertex3f(-w/2, h/2, d/2);
      glTexCoord2f( 0.0, 6/11);   glVertex3f(-w/2,-h/2, d/2);
      glTexCoord2f( 1.0, 6/11);   glVertex3f( w/2,-h/2, d/2);
      {Back Face}
      glNormal3f( 0.0, 0.0,-1.0);
      glTexCoord2f( 1.0, 8/11);   glVertex3f( w/2, h/2,-d/2);
      glTexCoord2f( 1.0, 9/11);   glVertex3f( w/2,-h/2,-d/2);
      glTexCoord2f( 0.0, 9/11);   glVertex3f(-w/2,-h/2,-d/2);
      glTexCoord2f( 0.0, 8/11);   glVertex3f(-w/2, h/2,-d/2);
      {Top Face}
      glNormal3f( 0.0, 1.0, 0.0);
      glTexCoord2f( 1.0, 8/11);   glVertex3f( w/2, h/2,-d/2);
      glTexCoord2f( 0.0, 8/11);   glVertex3f(-w/2, h/2,-d/2);
      glTexCoord2f( 0.0, 7/11);   glVertex3f(-w/2, h/2, d/2);
      glTexCoord2f( 1.0, 7/11);   glVertex3f( w/2, h/2, d/2);
      {Bottom Face}
      glNormal3f( 0.0,-1.0, 0.0);
      glTexCoord2f( 0.0, 9/11);   glVertex3f(-w/2,-h/2,-d/2);
      glTexCoord2f( 1.0, 9/11);   glVertex3f( w/2,-h/2,-d/2);
      glTexCoord2f( 1.0, 10/11);  glVertex3f( w/2,-h/2, d/2);
      glTexCoord2f( 0.0, 10/11);  glVertex3f(-w/2,-h/2, d/2);
    glEnd;
  glEndList;

  n90List:=n69List+1;
  glNewList(n90List, GL_COMPILE);
  glBindTexture(GL_TEXTURE_2D, textures[4]);
    glBegin(GL_QUADS);
      {Front Face}
      glNormal3f( 0.0, 0.0, 1.0);
      glTexCoord2f( 1.0, 10/11);   glVertex3f( w/2, h/2, d/2);
      glTexCoord2f( 0.0, 10/11);   glVertex3f(-w/2, h/2, d/2);
      glTexCoord2f( 0.0, 9/11);    glVertex3f(-w/2,-h/2, d/2);
      glTexCoord2f( 1.0, 9/11);    glVertex3f( w/2,-h/2, d/2);
      {Top Face}
      glNormal3f( 0.0, 1.0, 0.0);
      glTexCoord2f( 1.0, 1.0);     glVertex3f( w/2, h/2,-d/2);
      glTexCoord2f( 0.0, 1.0);     glVertex3f(-w/2, h/2,-d/2);
      glTexCoord2f( 0.0, 10/11);   glVertex3f(-w/2, h/2, d/2);
      glTexCoord2f( 1.0, 10/11);   glVertex3f( w/2, h/2, d/2);
    glEnd;
  glEndList;

end;

var
  CurTime: TDateTime;
  MSecs, i, j: integer;
begin
  inc(FrameCount);
  inc(LastFrameTicks,OpenGLControl1.FrameDiffTimeInMSecs);
  if (LastFrameTicks>=1000) then begin
    DebugLn(['TForm1.OpenGLControl1Paint Frames per second: ',FrameCount]);

    Frames:=IntToStr(FrameCount)+' FPS';

    dec(LastFrameTicks,1000);
    FrameCount:=0;
  end;

  if OpenGLControl1.MakeCurrent then
  begin
    if not AreaInitialized then begin
      myInit;
      InitGL;
      glMatrixMode (GL_PROJECTION);    { prepare for and then }
      glLoadIdentity ();               { define the projection }
      glFrustum (-640.0, 640.0, 0.0, 800.0, 1280.0, 2500.0); { transformation }
      glMatrixMode (GL_MODELVIEW);  { back to modelview matrix }
      glViewport (0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
                                    { define the viewport }
      AreaInitialized:=true;
    end;

    Form2.fps:='Framerate: '+Frames;

    CurTime:=Now;
    MSecs:=round(CurTime*86400*1000) mod 1000;
    if MSecs<0 then MSecs:=1000+MSecs;
    timer:=msecs-LastMsecs;
    if timer<0 then timer:=1000+timer;
    LastMsecs:=MSecs;

    if lives.CurrentValue=0 then begin
      menu1.GameOver;
      level:=levels[0];
      Exit;
    end;

    if smenu and mmove then begin
      if rry<20 then begin
        rry:=rry+0.25*animation;
        viewx:=viewx-1.25*animation;
        dist:=dist-2.5*animation;
      end
      else mmove:=false;
    end
    else if not smenu and mmove then begin
      if rry>0 then begin
        rry:=rry-0.25*animation;
        viewx:=viewx+1.25*animation;
        dist:=dist+2.5*animation;
      end
      else mmove:=false;
    end;

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    glLoadIdentity;             { clear the matrix }
    glTranslatef (viewx, 400.0,-1350.0+dist);  // -2.5); { viewing transformation }
    glRotatef(rry, 0, 1, 0); // menu rotation

    glPushMatrix;
    glTranslatef(-640,-400,0.0);
    glCallList(BackList);   // draw background
    glPopMatrix;

    glPushMatrix;
    glTranslatef(639, -300 , 70);
    glCallList(MenuList);   // draw menu
    glPopMatrix;

    glPushMatrix;
    glTranslatef(595,375,60);
    glCallList(HeartList);  // draw heart
    glPopMatrix;

    score.draw;
    lives.draw;

    glDisable(GL_TEXTURE_2D);
    level.bat.draw;

    for i:=0 to Length(level.balls)-1 do level.balls[i].draw;

    for i:=0 to Length(level.balls)-1 do if not level.balls[i].Visible then begin
      level.balls[i].Free;
      for j:=i to Length(level.balls)-2 do level.balls[j]:=level.balls[j+1];
      setLength(level.balls, Length(level.balls)-1);
    end;

    level.drawBlocks;

    glColor4f(1.0,1.0,1.0,0.5);
    glEnable(GL_TEXTURE_2D);

    //glFlush;
    //glFinish;
    // Swap backbuffer to front
    OpenGLControl1.SwapBuffers;
  end;
end;

procedure TForm1.OpenGLControl1Resize(Sender: TObject);
begin
  if (AreaInitialized)
  and OpenGLControl1.MakeCurrent then
    glViewport (0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
end;

procedure TForm1.OpenGLControl1Click(Sender: TObject);
begin
  if not level.balls[0].running and not menu1.Visible and not smenu then level.balls[0].resume;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
var i:integer;
begin
  if not menu1.Visible then
  begin
  if (Key=chr(27)) and (level<>nil) then
  begin
    if  level.bat.running then
    begin
      if level.balls[0].running then level.ran:=true
      else level.ran:=false;
      smenu:=true;
      mmove:=true;
      score.hide;
      level.bat.stop;
      for i:=0 to Length(level.balls)-1 do level.balls[i].stop;
    end
    else
    begin
      smenu:=false;
      mmove:=true;
      score.show;
      level.bat.resume;
      if level.ran then for i:=0 to Length(level.balls)-1 do level.balls[i].resume;
    end;
  end
  else if (Key='1') and smenu then begin
    level.reset;
    score.reset;
    lives.reset;
  end
  else if (Key='2') and smenu then Form2.Show
  else if (Key='3') and smenu then begin
    if MessageDlg('Fortschritt geht verloren',mtWarning, mbOKCancel, 0)=mrCancel
    then Exit;
    {$IFDEF MSWINDOWS}
      if sound then sndPlaySound('../config/sounds/title.wav', SND_ASYNC or SND_LOOP);
    {$ENDIF}
    OpenGLControl1.Visible:=false;
    OpenGLControl1.SetBounds(0,0,0,0);
    menu1.Visible:=true;
    menu1.BringToFront;
    level:=levels[0];
  end;
  end;
end;


{ TglTexture }

destructor TglTexture.Destroy;
begin
  if Data<>nil then FreeMem(Data);
  inherited Destroy;
end;

end.
