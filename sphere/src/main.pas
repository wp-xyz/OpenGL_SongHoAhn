unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Types,
  gl, glu, GLWin32WGLContext,
  oglUtils, oglSphere;


const
  CAMERA_DISTANCE = 6.0;
  DEFAULT_ROTANGLE_X = 20;
  DEFAULT_ROTANGLE_Y = 30;
  VIEW_ANGLE = 30.0;
  NEAR_CLIPDIST = 0.1;
  FAR_CLIPDIST = 100;

type
  TDrawMode = (dmFill, dmWireFrame, dmPoint);


  { TForm1 }

  TForm1 = class(TForm)
    cmbSectorCount: TComboBox;
    cmbStackCount: TComboBox;
    Label1: TLabel;
    lblSectorCount: TLabel;
    lblStackCount: TLabel;
    OpenGLControl: TOpenGLControl;
    Panel1: TPanel;
    procedure cmbSectorCountChange(Sender: TObject);
    procedure cmbStackCountChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure OpenGLControlKeyPress(Sender: TObject; var Key: char);
    procedure OpenGLControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControlMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure OpenGLControlMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OpenGLControlPaint(Sender: TObject);
    procedure OpenGLControlResize(Sender: TObject);
  private
    { private declarations }
    FInitDone: Boolean;
    FCameraDistance: Double;
    FCameraAngleX, FCameraAngleY: GLFloat;
    FMouseX, FMouseY: GLFloat;
    FDrawMode: TDrawMode;
    FTexID: GLuint;
    FFont: ToglFont;
    FFixedFont: ToglFont;
    FSphere1: ToglSphere;
    FSphere2: ToglSphere;

    procedure DrawInfo;
    procedure DrawScene;
    procedure InitGL;
    procedure InitLights;
    function LoadTexture(const AFileName: String; Wrap: Boolean): GLuint;
    procedure ToOrtho;
    procedure ToPerspective;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Clipbrd, oglTypes;

const
  GL_BGRA = $80E1;


function IfThen(b: Boolean; i, j: Integer): Integer;
begin
  if b then Result := i else result := j;
end;


{ TForm1 }

procedure TForm1.cmbSectorCountChange(Sender: TObject);
begin
  FSphere1.SectorCount := StrToInt(cmbSectorCount.Items[cmbSectorCount.ItemIndex]);
  FSphere2.SectorCount := FSphere1.SectorCount;
  OpenGLControl.Invalidate;
end;


procedure TForm1.cmbStackCountChange(Sender: TObject);
begin
  FSphere1.StackCount := StrToInt(cmbStackCount.Items[cmbStackCount.ItemIndex]);
  FSphere2.StackCount := FSphere1.StackCount;
  OpenGLControl.Invalidate;
end;


procedure TForm1.DrawScene;
const
  AMBIENT: ToglVector4f = (0.5, 0.5, 0.5, 1);
  DIFFUSE: ToglVector4f = (0.7, 0.7, 0.7, 1);
  SPECULAR: ToglVector4f = (1.0, 1.0, 1.0, 1);
  SHININESS: GLFloat = 128;
  LINE_COLOR: ToglVector4f = (0.2, 0.2, 0.2, 1);
begin
  glMaterialfv(GL_FRONT, GL_AMBIENT,   @AMBIENT);
  glMaterialfv(GL_FRONT, GL_DIFFUSE,   @DIFFUSE);
  glMaterialfv(GL_FRONT, GL_SPECULAR,  @SPECULAR);
  glMaterialf(GL_FRONT,  GL_SHININESS, SHININESS);

  // draw left flat sphere with lines
  glPushMatrix();
  glTranslatef(-2.5, 0, 0);
  glRotatef(FCameraAngleX, 1, 0, 0);   // pitch
  glRotatef(FCameraAngleY, 0, 1, 0);   // heading
  glRotatef(-90, 1, 0, 0);
  glBindTexture(GL_TEXTURE_2D, 0);
  FSphere1.DrawWithLines(LINE_COLOR);
  //FSphere1.DrawLines(LINE_COLOR);
  glPopMatrix();

  // draw centre smooth sphere with line
  glPushMatrix();
  glRotatef(FCameraAngleX, 1, 0, 0);
  glRotatef(FCameraAngleY, 0, 1, 0);
  glRotatef(-90, 1, 0, 0);
  glBindTexture(GL_TEXTURE_2D, 0);
  FSphere2.DrawWithLines(LINE_COLOR);
  glPopMatrix();

  // draw right sphere with texture
  glPushMatrix();
  glTranslatef(2.5, 0, 0);
  glRotatef(FCameraAngleX, 1, 0, 0);
  glRotatef(FCameraAngleY, 0, 1, 0);
  glRotatef(-90, 1, 0, 0);
  glBindTexture(GL_TEXTURE_2D, FTexID);
  FSphere2.Draw();
  glPopMatrix();
  glBindTexture(GL_TEXTURE_2D, 0);
end;



(*
{-------------------------------------------------------------------------------
  Write 2d text using GLUT
  The projection matrix must be set to orthogonal before calling this function.
-------------------------------------------------------------------------------}
procedure TForm1.DrawString(const Str: String; x, y: Integer;
  AColor: ToglVector4f; AFont: Pointer);
var
  p: PChar;
begin
  if Str = '' then
    exit;

  glPushAttrib(GL_LIGHTING_BIT | GL_CURRENT_BIT); // lighting and color mask
  glDisable(GL_LIGHTING);     // need to disable lighting for proper text color
  glDisable(GL_TEXTURE_2D);

  glColor4fv(color);          // set text color
  glRasterPos2i(x, y);        // place text position

  // loop all characters in the string
  P := @Str[1];
  while P^ <> #0 do begin
    glutBitmapCharacter(font, P);
    inc(P);
  end;

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_LIGHTING);
  glPopAttrib();
end;
*)

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCameraDistance := CAMERA_DISTANCE;
  FCameraAngleX := 0.0;
  FCameraAngleY := 0.0;
  FDrawMode := dmFill;

  // sphere: min sector = 3, min stack = 2
  FSphere1 := ToglSphere.Create(1.0, 36, 18, false);    // radius, sectors, stacks, non-smooth (flat) shading
  FSphere2 := ToglSphere.Create(1.0, 36, 18);           // radius, sectors, stacks, smooth(default)end;

  cmbSectorCount.ItemIndex := cmbSectorCount.Items.IndexOf('36');
  cmbStackCount.ItemIndex := cmbStackCount.Items.IndexOf('18');
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  oglDestroyFont(FFont);
  oglDestroyFont(FFixedFont);
  FSphere1.Free;
  FSphere2.Free;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key in ['d', 'D'] then begin
    FDrawMode := TDrawMode(succ(ord(FDrawMode)) mod 3);
    case FDrawMode of
      dmFill:   // fill mode
        begin
          glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
          glEnable(GL_DEPTH_TEST);
          glEnable(GL_CULL_FACE);
        end;
      dmWireframe:  // wireframe mode
        begin
          glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
          glDisable(GL_DEPTH_TEST);
          glDisable(GL_CULL_FACE);
        end;
      dmPoint:    // point mode
        begin
          glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
          glDisable(GL_DEPTH_TEST);
          glDisable(GL_CULL_FACE);
        end;
    end;
    OpenGLControl.Invalidate;
  end;
end;


{-------------------------------------------------------------------------------
  Initialize OpenGL
  Disable unused features
-------------------------------------------------------------------------------}
procedure TForm1.InitGL;
begin
  glShadeModel(GL_SMOOTH);                    // shading mathod: GL_SMOOTH or GL_FLAT
  glPixelStorei(GL_UNPACK_ALIGNMENT, 4);      // 4-byte pixel alignment

  // enable /disable features
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  //glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_CULL_FACE);

  // track material ambient and diffuse from surface color, call it before glEnable(GL_COLOR_MATERIAL)
  //glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
  //glEnable(GL_COLOR_MATERIAL);

  glClearColor(0, 0, 0, 0);                   // background color
  glClearStencil(0);                          // clear stencil buffer
  glClearDepth(1.0);                          // 0 is near, 1 is far
  glDepthFunc(GL_LEQUAL);

  InitLights;

  // Create fonts
  oglDestroyfont(FFont);
  oglDestroyfont(FFixedFont);
  FFont := oglCreateBitmapFont(wglGetCurrentDC, 'Arial', 24, [fsBold]);
  FFixedFont := oglCreateBitmapFont(wglGetCurrentDC, 'Courier New', 14, []);

  // Load texture
  FTexID := LoadTexture('../../src/earth2048.bmp', true);
//  FTexID := LoadTexture('../../src/moon1024.bmp', true);
end;


{-------------------------------------------------------------------------------
  Initialize lights
-------------------------------------------------------------------------------}
procedure TForm1.InitLights;
const
  AMBIENT: ToglVector4f  = (0.3, 0.3, 0.3, 1.0);    // ambient light
  DIFFUSE: ToglVector4f  = (0.7, 0.7, 0.7, 1.0);    // diffuse light
  SPECULAR: ToglVector4f = (1.0, 1.0, 1.0, 1.0);    // specular light
  LIGHT_POS: ToglVector4f = (0.0, 0.0, 1.0, 0.0);   // directional light
begin
  // set up light colors (ambient, diffuse, specular)
  glLightfv(GL_LIGHT0, GL_AMBIENT, @AMBIENT);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @DIFFUSE);
  glLightfv(GL_LIGHT0, GL_SPECULAR, @SPECULAR);

  // position the light
  glLightfv(GL_LIGHT0, GL_POSITION, @LIGHT_POS);

  // MUST enable each light source after configuration
  glEnable(GL_LIGHT0);
end;


{-------------------------------------------------------------------------------
  Load  raw image as a texture
-------------------------------------------------------------------------------}
function TForm1.LoadTexture(const AFileName: String; Wrap: Boolean): GLuint;
var
  bmp: TBitmap;
  w, h: Integer;
  data: PByte;
  lType: GLenum;
  format: GLenum;
  texture: GLuint;
begin
  Result := 0;

  if not FileExists(AFileName) then
    exit;

  bmp := TBitmap.Create;
  try
    bmp.LoadfromFile(AFileName);
    w := bmp.Width;
    h := bmp.Height;
    data := bmp.RawImage.Data;
    lType := GL_UNSIGNED_BYTE;  // only allow BMP with 8-bit per channel

    // We assume the image is 8-bit, 24-bit or 32-bit BMP
    case bmp.PixelFormat of
      pf8Bit: format := GL_LUMINANCE;
      pf24Bit: format := GL_RGB;
      pf32bit: format := GL_BGRA; //GL_RGBA;
      else exit;    // not supported --> exit
    end;

    // generate texture ID
    glGenTextures(1, @texture);

    // set active texture and configure it
    glBindTexture(GL_TEXTURE_2D, texture);

    // select modulate to mix texture with color for shading
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    //glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE);

    // if wrap is true, the texture wraps over at the edges (repeat)
    //       ... false, the texture ends at the edges (clamp)
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, IfThen(Wrap, GL_REPEAT, GL_CLAMP));
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, IfThen(Wrap, GL_REPEAT, GL_CLAMP));
    //glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    //glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    // copy texture data
    glTexImage2D(GL_TEXTURE_2D, 0, format, w, h, 0, format, lType, data);
    //glGenerateMipmap(GL_TEXTURE_2D);

    // build our texture mipmaps
    case bmp.Pixelformat of
      pf8Bit:
        gluBuild2DMipmaps(GL_TEXTURE_2D, 1, w, h, GL_LUMINANCE, lType, data);
      pf24bit:
        gluBuild2DMipmaps(GL_TEXTURE_2D, 3, width, height, GL_RGB, lType, data);
      pf32bit:
        gluBuild2DMipmaps(GL_TEXTURE_2D, 4, width, height, GL_RGBA, ltype, data);
    end;

    Result := texture;
  finally
    bmp.Free;
  end;
end;


procedure TForm1.OpenGLControlKeyPress(Sender: TObject; var Key: char);
begin
  if Key in ['d', 'D'] then begin
    FDrawMode := TDrawMode(succ(ord(FDrawMode)) mod 3);
    case FDrawMode of
      dmFill:   // fill mode
        begin
          glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
          glEnable(GL_DEPTH_TEST);
          glEnable(GL_CULL_FACE);
        end;
      dmWireframe:  // wireframe mode
        begin
          glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
          glDisable(GL_DEPTH_TEST);
          glDisable(GL_CULL_FACE);
        end;
      dmPoint:    // point mode
        begin
          glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
          glDisable(GL_DEPTH_TEST);
          glDisable(GL_CULL_FACE);
        end;
    end;
  end;
end;


procedure TForm1.OpenGLControlMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseX := X;
  FMouseY := Y;
end;


procedure TForm1.OpenGLControlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if (ssLeft in Shift) then begin
    FCameraAngleY := FCameraAngleY + (x - FMouseX);
    FCameraAngleX := FCameraAngleX + (y - FMouseY);
    FMouseX := X;
    FMouseY := Y;
    OpenGLControl.Invalidate;
  end else
  if (ssRight in Shift) then begin
    FCameraDistance := FCameraDistance - round((Y - FMouseY) * 0.2);
    FMouseY := Y;
    OpenGLControl.Invalidate;
  end;
end;


procedure TForm1.OpenGLControlMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
const
  SPEED = 0.0005;
var
  f: Double;
begin
  if (ssCtrl in Shift) then
    f := -1.0
  else
    f := 1.0;
  if (ssShift in Shift) then
    f := f * 2;
  FCameraDistance := FCameraDistance * (1.0 + f * WheelDelta * SPEED);
  OpenGLControl.Invalidate;
end;


procedure TForm1.OpenGLControlPaint(Sender: TObject);
begin
  if not OpenGLControl.MakeCurrent then
    exit;

  if not FInitDone then begin
    InitGL;
    ToPerspective;
    FInitDone := true;
  end;

  // clear buffer
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

  // save the initial ModelView matrix before modifying ModelView matrix
  glPushMatrix();

  // tramsform modelview matrix
  glTranslatef(0, 0, -FCameraDistance);

  DrawScene;
  DrawInfo;

  glPopMatrix();

  OpenGLControl.SwapBuffers;
end;


procedure TForm1.OpenGLControlResize(Sender: TObject);
begin
  if FInitDone and OpenGLControl.MakeCurrent then
    ToPerspective;
end;

{-------------------------------------------------------------------------------
  Display info messages
-------------------------------------------------------------------------------}
procedure TForm1.DrawInfo;
var
  s: String;
  L: TStrings;
  w, h, x, y: Integer;
begin
  // backup current model-view matrix
  glPushMatrix();                     // save current modelview matrix
  glLoadIdentity();                   // reset modelview matrix

  // set to 2D orthogonal projection
  glMatrixMode(GL_PROJECTION);        // switch to projection matrix
  glPushMatrix();                     // save current projection matrix
  glLoadIdentity();                   // reset projection matrix
  glOrtho(0, OpenGLControl.Width, 0, OpenGLControl.Height, -1, 1); // set to orthogonal projection

  // write text
  glDisable(GL_LIGHTING);
  glColor4f(1, 1, 1, 1);
  oglTextSize(FFixedFont, 'Tgj', w, h);
  x := 4;
  y := OpenGLControl.Height - h;
  L := TStringList.Create;
  try
    L.Text := FSphere1.Info;
    for s in L do begin
      oglTextOut(FFixedFont, x, y, s);
      dec(y, h);
    end;
  finally
    L.Free;
  end;
  glEnable(GL_LIGHTING);

  // restore projection matrix
  glPopMatrix();                   // restore to previous projection matrix

  // restore modelview matrix
  glMatrixMode(GL_MODELVIEW);      // switch to modelview matrix
  glPopMatrix();                   // restore to previous modelview matrix

end;


{-------------------------------------------------------------------------------
  Set projection matrix as orthogonal
-------------------------------------------------------------------------------}
procedure TForm1.ToOrtho;
begin
  // set viewport to be the entire window of the OpenGLControl
  glViewport(0, 0, OpenGLControl.Width, OpenGLControl.Height);

  // set orthographic viewing frustum
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(0, OpenGLControl.Width, 0, OpenGLControl.Height, -1, 1);

  // switch to modelview matrix in order to set scene
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
end;


{-------------------------------------------------------------------------------
  Set the projection matrix as perspective
-------------------------------------------------------------------------------}
procedure TForm1.ToPerspective;
var
  aspect: GLFloat;
begin
  // set viewport to be the entire window of the OpenGLControl
  glViewport(0, 0, OpenGLControl.Width, OpenGLControl.Height);

  // set perspective viewing frustum
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  aspect := OpenGLControl.Width / OpenGLControl.Height;
  gluPerspective(VIEW_ANGLE, aspect, NEAR_CLIPDIST, FAR_CLIPDIST);

  // switch to modelview matrix in order to set scene
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
end;

end.

