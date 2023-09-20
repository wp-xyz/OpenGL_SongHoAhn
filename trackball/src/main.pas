unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Types,
  gl, glu, glut, GLWin32WGLContext,
  oglUtils, vectors, matrices, quaternions, oglTrackball;

const
  DEFAULT_ROTANGLE_X = 20;
  DEFAULT_ROTANGLE_Y = 30;
  VIEW_ANGLE = 60.0;
  NEAR_CLIPDIST = 1;
  FAR_CLIPDIST = 10000;
  RADIUS_SCALE = 0.5;
  PATH_COUNT = 30;
  POINT_COUNT = 20;
  RAD2DEG = 180.0 / pi;

type
  TDrawMode = (dmFill, dmWireFrame, dmPoint);
  TVector3Array = array of TVector3;


  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    OpenGLControl: TOpenGLControl;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure OpenGLControlKeyPress(Sender: TObject; var Key: char);
    procedure OpenGLControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControlMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure OpenGLControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControlMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OpenGLControlPaint(Sender: TObject);
    procedure OpenGLControlResize(Sender: TObject);
  private
    { private declarations }
    FInitDone: Boolean;
    FCameraDistance: Double;
    FMouseX, FMouseY: Integer;
    FPrevX, FPrevY: Integer;
    FMouseLeftDown, FMouseRightDown: Boolean;
    FDrawMode: TDrawMode;
    FTexID: GLuint;
    FFont: ToglFont;
    FFixedFont: ToglFont;
    FQuat: TQuaternion;
    FPrevQuat: TQuaternion;
    FCirclePoints: TVector3Array;
    FPathPoints: TVector3Array;
    FSphereVector: TVector3;
    FSphereRadius: GLfloat;
    FTrackball: TTrackball;
    FSphere: PGLUquadric;
    FPoint: PGLUQuadric;
    function BuildCircle(ARadius: GLfloat; Steps: Integer): TVector3Array;
    procedure Draw2D;
    procedure Draw3D;
    procedure DrawAxis(ASize: GLfloat);
    procedure DrawCircle(const APoints: TVector3Array);
    procedure DrawInfo;
    procedure DrawPath(const APoints: TVector3Array);
    procedure GenerateMousePath;
    procedure GetRotationAxisAngle(const v1, v2: TVector3; out Axis: TVector3; out Angle: GLfloat);
    function GetRotationQuaternion(const v1, v2: TVector3): TQuaternion;
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
  Math, Clipbrd, oglTypes;

const
  GL_BGRA = $80E1;


function IfThen(b: Boolean; i, j: Integer): Integer;
begin
  if b then Result := i else result := j;
end;


{ TForm1 }

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
  FDrawMode := dmFill;

  if OpenGLControl.Width > OpenGLControl.Height then
    FSphereRadius := OpenGLControl.Height * RADIUS_SCALE
  else
    FSphereRadius := OpenGLControl.Width * RADIUS_SCALE;
  FTrackBall := TTrackball.Create(FSphereRadius, OpenGLControl.Width, OpenGLControl.Height);

  FQuat := Quaternion(1, 0, 0, 0);  // init rotation quaternion
  FCameraDistance := FSphereRadius * 3.0;
  FCirclePoints := BuildCircle(FSphereRadius, 100);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  oglDestroyFont(FFont);
  oglDestroyFont(FFixedFont);
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
   Compute mouse path on a sphere between 2 mouse positions
-------------------------------------------------------------------------------}
procedure TForm1.GenerateMousePath;
var
  v1, v2, v: TVector3;
  alpha: GLfloat;
  i: Integer;
begin
  v1 := FSphereVector;
  v2 := FTrackball.GetVector(FMouseX, FMouseY);

  SetLength(FPathPoints, PATH_COUNT + 1);
  for i := 0 to PATH_COUNT do begin
    alpha := i / PATH_COUNT;
    v := slerp(v1, v2, alpha);
    v := v.Normalize * (FSphereRadius + 0.3);
    FPathPoints[i] := v;
  end;
end;


{-------------------------------------------------------------------------------
  Returns rotation axis and angle
-------------------------------------------------------------------------------}
procedure TForm1.GetRotationAxisAngle(const v1, v2: TVector3;
  out Axis: TVector3; out Angle: GLfloat);
begin
  // get rotation axis
  Axis := v1.Cross(v2);

  // compute angle
  Angle := arccos(v1.dot(v2));
end;


{-------------------------------------------------------------------------------
  Returns rotation quaternion
-------------------------------------------------------------------------------}
function TForm1.GetRotationQuaternion(const v1, v2: TVector3): TQuaternion;
var
  v : TVector3;
  angle: GLfloat;
begin
  // get rotation axis
  v := v1.cross(v2);

  // compute angle
  angle := arccos(v1.dot(v2));

  Result := quaternion(v, angle*0.5);   // return half angle for quaternion
end;


{-------------------------------------------------------------------------------
  Initialize OpenGL
  Disable unused features
-------------------------------------------------------------------------------}
procedure TForm1.InitGL;
var
  white: array[0..3] of GLfloat = (1, 1, 1, 1);
begin
  glShadeModel(GL_SMOOTH);                    // shading mathod: GL_SMOOTH or GL_FLAT
  glPixelStorei(GL_UNPACK_ALIGNMENT, 4);      // 4-byte pixel alignment

  // enable /disable features
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  //glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  //glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_CULL_FACE);
  glEnable(GL_BLEND);

  // track material ambient and diffuse from surface color, call it before glEnable(GL_COLOR_MATERIAL)
  glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_COLOR_MATERIAL);

  glClearColor(0, 0, 0, 0);                   // background color
  glClearStencil(0);                          // clear stencil buffer
  glClearDepth(1.0);                          // 0 is near, 1 is far
  glDepthFunc(GL_LEQUAL);

  InitLights;
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 128);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, white);

  // Create fonts
  oglDestroyfont(FFont);
  oglDestroyfont(FFixedFont);
  FFont := oglCreateBitmapFont(wglGetCurrentDC, 'Arial', 24, [fsBold]);
  FFixedFont := oglCreateBitmapFont(wglGetCurrentDC, 'Courier New', 14, []);

  // Load texture
  FTexID := LoadTexture('../../src/earth2048.bmp', true);
//  FTexID := LoadTexture('../../src/moon1024.bmp', true);

  FSphere := gluNewQuadric();
  gluQuadricDrawStyle(FSphere, GLU_FILL);  // GLU_FILL, GLU_LINE, GLU_SILHOUETTE, GLU_POINT

  FPoint := gluNewQuadric();
  gluQuadricDrawStyle(FPoint, GLU_FILL);   // GLU_FILL, GLU_LINE, GLU_SILHOUETTE, GLU_POINT

end;


{-------------------------------------------------------------------------------
  Initialize lights
-------------------------------------------------------------------------------}
procedure TForm1.InitLights;
const
  AMBIENT: ToglArray4f  = (0.3, 0.3, 0.3, 1.0);    // ambient light
  DIFFUSE: ToglArray4f  = (0.7, 0.7, 0.7, 1.0);    // diffuse light
  SPECULAR: ToglArray4f = (1.0, 1.0, 1.0, 1.0);    // specular light
  LIGHT_POS: ToglArray4f = (0.0, 0.0, 1.0, 0.0);   // directional light
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
  case Key of
    #27:
      Close;
    'd', 'D':
      begin
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
    'r', 'R':  // Reset quaternion
      FQuat := Quaternion(1, 0, 0, 0);
    ' ':
      if FTrackball.Mode = tmArc then
        FTrackball.Mode := tmProject
      else
        FTrackball.Mode := tmArc;
  end;
end;


procedure TForm1.OpenGLControlMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseX := X;
  FMouseY := Y;

  if Button = mbLeft then begin
    FMouseLeftDown := true;
    // remember mouse coords and quaternion before rotation
    FPrevX := x;
    FPrevY := y;
    FPrevQuat := FQuat;
  end else
  if Button = mbRight then
    FMouseRightDown := true;
end;


procedure TForm1.OpenGLControlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  v1, v2: TVector3;
  delta: TQuaternion;
begin
  if (ssLeft in Shift) then begin
    v1 := FTrackball.GetUnitVector(FPrevX, FPrevY);
    v2 := FTrackball.GetUnitVector(x, y);

    delta := GetQuaternion(v1, v2);
    FQuat := delta * FPrevQuat;

    FMouseX := X;
    FMouseY := Y;
    GenerateMousePath;
    OpenGLControl.Invalidate;
  end else
  if (ssRight in Shift) then begin
    FCameraDistance := FCameraDistance - round((Y - FMouseY) * FSphereRadius);
    FMouseY := Y;
    OpenGLControl.Invalidate;
  end else begin
    FMouseX := x;
    FMouseY := y;
    FSphereVector := FTrackball.getVector(FMouseX, FMouseY);
  end;
end;


procedure TForm1.OpenGLControlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbleft then begin
    FMouseLeftDown := false;
    SetLength(FPathPoints, 0); // clear mouse path
  end else
  if Button = mbRight then
    FMouseRightDown := false;
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
var
  mat: TMatrix4;
begin
  if not OpenGLControl.MakeCurrent then
    exit;

  if not FInitDone then begin
    InitGL;
    ToPerspective;
    FInitDone := true;
  end;


  // clear framebuffer
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

  // save the initial ModelView matrix before modifying ModelView matrix
  glPushMatrix();

  // tramsform camera
  mat := FQuat.GetMatrix(); // view matrix
  mat.Transpose();
  mat.Translate(0, 0, -FCameraDistance);
  glMultMatrixf(mat.Get());

  Draw3D();   // draw 3D sphere, cursor vector and axis
  Draw2D();   // draw 2D trackball and mapped cursor point

  DrawInfo(); // print text

  glPopMatrix();

  OpenGLControl.SwapBuffers;
end;


procedure TForm1.OpenGLControlResize(Sender: TObject);
begin
  if FInitDone and OpenGLControl.MakeCurrent then
    ToPerspective;

  // set the trackball
  if OpenGLControl.Width > OpenGLControl.Height then
    FSphereRadius := OpenGLControl.Height * RADIUS_SCALE
  else
    FSphereRadius := OpenGLControl.Width * RADIUS_SCALE;
  FTrackball.SetParams(FSphereRadius, OpenGLControl.Width, OpenGLControl.Height);
  FCameraDistance := FSphereRadius * 3.0;
  FCirclePoints := BuildCircle(FSphereRadius, 100);
end;

{-------------------------------------------------------------------------------
  Make points of a circle
-------------------------------------------------------------------------------}
function TForm1.BuildCircle(ARadius: GLfloat; Steps: Integer): TVector3Array;
const
  TWO_PI = 2.0 * pi;
var
  x, y, a: GLfloat;
  i: Integer;
begin
  if Steps < 2 then begin
    Result := nil;
    exit;
  end;

  SetLength(Result, Steps + 1);
  for i := 0 to Steps do begin
    a := TWO_PI / Steps * i;
    x := ARadius * cos(a);
    y := ARadius * sin(a);
    Result[i] := Vector3(x, y, 0);
  end;
end;


{-------------------------------------------------------------------------------
  Draw 2D part
-------------------------------------------------------------------------------}
procedure TForm1.Draw2D;
begin
  // set to 2D orthogonal projection
  glMatrixMode(GL_PROJECTION);     // switch to projection matrix
  glPushMatrix();                  // save current projection matrix
  glLoadIdentity();                // reset projection matrix
  glOrtho(
    -OpenGLControl.Width*0.5, OpenGLControl.Width*0.5,
    -OpenGLControl.Height*0.5, OpenGLControl.Height*0.5,
    -100, 100
  );

  // backup current model-view matrix
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();                     // save current modelview matrix
  glLoadIdentity();                   // reset modelview matrix

  glDisable(GL_LIGHTING);
  glLineWidth(1);
  glColor4f(1, 1, 0, 1);
  DrawCircle(FCirclePoints);

  glEnable(GL_LIGHTING);

  glMatrixMode(GL_PROJECTION);
  glPopMatrix();
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix();
end;


{-------------------------------------------------------------------------------
  Draw 3D part
-------------------------------------------------------------------------------}
procedure TForm1.Draw3D;
var
  vec: TVector3;
begin
  glPushMatrix();
  glLoadIdentity();   // reset because the point doesn't affect trackball rotation
  glTranslatef(0, 0, -FCameraDistance);

  vec := FTrackball.GetVector(round(FMouseX), round(FMouseY));

  // draw mouse path
  glDisable(GL_LIGHTING);
  glColor4f(1, 1, 0, 1);
  glLineWidth(5);
  DrawPath(FPathPoints);
  glLineWidth(2);
  glBegin(GL_LINES);
    glVertex3f(0, 0, 0);
    glVertex3f(FSphereVector.x, FSphereVector.y, FSphereVector.z);
    glVertex3f(0, 0, 0);
    glVertex3f(vec.x, vec.y, vec.z);
  glEnd();
  glEnable(GL_LIGHTING);

  // draw the point on the sphere where mouse is clicked
  if FMouseLeftDown then begin
    glColor4f(1, 0, 1, 1);
    glPushMatrix();
    glTranslatef(FSphereVector.x, FSphereVector.y, FSphereVector.z);
    gluSphere(FPoint, FSphereRadius*0.05, 20, 20); // radius, slice, stack
    glPopMatrix();
  end;

  // draw the point where mouse is currently located
  glTranslatef(vec.x, vec.y, vec.z);
  glColor4f(0, 1, 1, 1);
  gluSphere(FPoint, FSphereRadius*0.05, 20, 20); // radius, slice, stack

  glPopMatrix();

  // draw axis
  DrawAxis(FSphereRadius + FSphereRadius * 0.1);

  // draw teapot
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glCullFace(GL_FRONT);
  glColor4f(1.0, 1.0, 1.0, 1.0);

  //DrawScene;
  glutWireTeapot(FSphereRadius * 0.6);

  //glColor4f(0.7, 0.7, 0.7, 0.6);
  //gluSphere(FSphere, FSphereRadius, 100, 100); // radius, slice, stack
  glCullFace(GL_BACK);
  glColor4f(0.7, 0.7, 0.7, 0.6);
  gluSphere(FSphere, FSphereRadius, 100, 100); // radius, slice, stack

  //DrawScene;
  glutWireTeapot(FSphereRadius * 0.6);

  glDisable(GL_BLEND);
  glPopMatrix();
end;

{-------------------------------------------------------------------------------
  Draw the local axis of an object
-------------------------------------------------------------------------------}
procedure TForm1.DrawAxis(ASize: GLfloat);
begin
  //glDepthFunc(GL_ALWAYS);     // to avoid visual artifacts with grid lines
  glDisable(GL_LIGHTING);

  // draw axis
  glLineWidth(3);
  glBegin(GL_LINES);
    glColor3f(1, 0, 0);
    glVertex3f(0, 0, 0);
    glVertex3f(ASize, 0, 0);
    glColor3f(0, 1, 0);
    glVertex3f(0, 0, 0);
    glVertex3f(0, ASize, 0);
    glColor3f(0, 0, 1);
    glVertex3f(0, 0, 0);
    glVertex3f(0, 0, ASize);
  glEnd();
  glLineWidth(1);

  // draw arrows(actually big square dots)
  glPointSize(5);
  glBegin(GL_POINTS);
    glColor3f(1, 0, 0);
    glVertex3f(ASize, 0, 0);
    glColor3f(0, 1, 0);
    glVertex3f(0, ASize, 0);
    glColor3f(0, 0, 1);
    glVertex3f(0, 0, ASize);
  glEnd();
  glPointSize(1);

  // restore default settings
  glEnable(GL_LIGHTING);
  //glDepthFunc(GL_LEQUAL);
end;

{-------------------------------------------------------------------------------
  Draw a circle
-------------------------------------------------------------------------------}
procedure TForm1.DrawCircle(const APoints: TVector3Array);
var
  i: Integer;
begin
  glBegin(GL_LINES);
  for i := 0 to High(APoints)-1 do begin
    glVertex3fv(@APoints[i]);
    glVertex3fv(@APoints[i+1]);
  end;
  glEnd();
end;

{-------------------------------------------------------------------------------
  Draw mouse path
-------------------------------------------------------------------------------}
procedure TForm1.DrawPath(const APoints: TVector3Array);
var
  i: Integer;
begin
  glBegin(GL_LINES);
    for  i := 0 to High(APoints) - 1 do begin
      glVertex3fv(@APoints[i].x);
      glVertex3fv(@APoints[i+1].x);
    end;
  glEnd();
end;

{-------------------------------------------------------------------------------
  Display info messages
-------------------------------------------------------------------------------}
procedure TForm1.DrawInfo;
var
  s: String;
  w, h: Integer;
  x, y: Integer;
  v, v1, v2: TVector3;
  angle: GLfloat;
begin
  // backup current model-view matrix
  glPushMatrix();                     // save current modelview matrix
  glLoadIdentity();                   // reset modelview matrix
  glDisable(GL_LIGHTING);

  // set to 2D orthogonal projection
  glMatrixMode(GL_PROJECTION);        // switch to projection matrix
  glPushMatrix();                     // save current projection matrix
  glLoadIdentity();                   // reset projection matrix
  gluOrtho2D(0, OpenGLControl.Width, 0, OpenGLControl.Height);

  v1 := FSphereVector;                             // first vector on sphere
  v2 := FTrackball.GetVector(FMouseX, FMouseY);    // second vector n sphere
  angle := RAD2DEG * arccos(v1.dot(v2) / (v1.Length * v2.Length));

  // write text
  glColor4f(1, 1, 1, 1);
  oglTextSize(FFixedFont, 'Tgj', w, h);
  x := 4;
  y := OpenGLControl.Height - h;
  s := 'MouseCoords: (' + IntToStr(FMouseX) + ', ' + IntToStr(FMouseY) + ')';
  oglTextOut(FFixedFont, x, y, s);
  dec(y, h);

  s := 'Point on sphere: ' + v1.Info;
  if FMouseleftdown then
    s := s + ' - ' + v2.Info;
  oglTextOut(FFixedFont, x, y, s);
  dec(y, h);

  v := v2;
  v.Normalize;
  s := 'Normalized: ' + v.Info;
  oglTextOut(FFixedFont, x, y, s);
  dec(y, h);

  s := 'Angle between points: ' + FormatFloat('0.0', angle);
  oglTextOut(FFixedFont, x, y, s);
  dec(y, h);

  if FTrackball.Mode = tmARC then s := 'ARC' else s := 'PROJECT';
  s := 'Mode: ' + s;
  oglTextOut(FFixedFont, x, y, s);
  dec(y, h);

  s := 'Camera distance: ' + FormatFloat('0.000', FCameraDistance);
  oglTextOut(FFixedFont, x, y, s);
  dec(y, h);

  s := 'Press SPACE to change trackball mode.';
  oglTextOut(FFixedFont, x, y, s);
  dec(y, h);

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

