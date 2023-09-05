unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Types,
  gl, glu, GLWin32WGLContext,
  oglUtils;


const
  CAMERA_DISTANCE = 10.0;
  DEFAULT_ROTANGLE_X = 20;
  DEFAULT_ROTANGLE_Y = 30;
  VIEW_ANGLE = 60.0;
  NEAR_CLIPDIST = 0.1;
  FAR_CLIPDIST = 100;

type
  TDrawMode = (dmFill, dmWireFrame, dmPoint);

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
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
    FFixedFont: ToglFont;
    FListID: GLuint;
    FDisplayListUsed: Boolean;

    procedure DrawScene;
    procedure InitGL;
    procedure InitLights;
    procedure SetCamera(PosX, PosY, PosZ, TargetX, TargetY, TargetZ: GLfloat);
    procedure ShowInfo;
    procedure ToPerspective;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Clipbrd, oglTypes, oglTeaPot;

const
  GL_BGRA = $80E1;


function IfThen(b: Boolean; i, j: Integer): Integer;
begin
  if b then Result := i else result := j;
end;


{ TForm1 }

procedure TForm1.DrawScene;
begin
//
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  FCameraDistance := CAMERA_DISTANCE;
  FCameraAngleX := 0.0;
  FCameraAngleY := 0.0;
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  glDeleteLists(FListID, 1);     // delete one
  oglDestroyFont(FFixedFont);
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
    Close
  else
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
  end else
  if Key = ' ' then begin
    FDisplayListUsed := not FDisplayListUsed;
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
  //glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  //glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_CULL_FACE);

  // track material ambient and diffuse from surface color
  // call it before glEnable(GL_COLOR_MATERIAL)
  glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_COLOR_MATERIAL);

  glClearColor(0, 0, 0, 0);                   // background color
  glClearStencil(0);                          // clear stencil buffer
  glClearDepth(1.0);                          // 0 is near, 1 is far
  glDepthFunc(GL_LEQUAL);

  InitLights;
  SetCamera(0, 0, FCameraDistance, 0, 0, 0);

  // Create fonts
  oglDestroyfont(FFixedFont);
  FFixedFont := oglCreateBitmapFont(wglGetCurrentDC, 'Courier New', 14, []);

  // compile a display list of teapot mesh
  // see detail in createTeapotDL()
  if FListID = 0 then FListId := CreateTeapotDL;
end;


{-------------------------------------------------------------------------------
  Initialize lights
-------------------------------------------------------------------------------}
procedure TForm1.InitLights;
const
  AMBIENT: ToglVector4f  = (0.2, 0.2, 0.2, 1.0);    // ambient light
  DIFFUSE: ToglVector4f  = (0.7, 0.7, 0.7, 1.0);    // diffuse light
  SPECULAR: ToglVector4f = (1.0, 1.0, 1.0, 1.0);    // specular light
  LIGHT_POS: ToglVector4f = (0.0, 0.0, 20.0, 0.0);   // positional light
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

  // transform modelview matrix
  glTranslatef(0, 0, -FCameraDistance);
  glRotatef(FCameraAngleX, 1, 0, 0);   // pitch
  glRotatef(FCameraAngleY, 0, 1, 0);   // heading

  if FDisplayListUsed then
    glCallList(FListID)              // render with display list
  else
    DrawTeaPot;                      // render with vertex array, glDrawElements()

  // Draw info messages
  ShowInfo;

  glPopMatrix();

  OpenGLControl.SwapBuffers;
end;


procedure TForm1.OpenGLControlResize(Sender: TObject);
begin
  if FInitDone and OpenGLControl.MakeCurrent then
    ToPerspective;
end;


{-------------------------------------------------------------------------------
  Set camera position and look-at direction
-------------------------------------------------------------------------------}
procedure TForm1.SetCamera(PosX, PosY, PosZ, TargetX, TargetY, TargetZ: GLfloat);
begin
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(PosX, PosY, PosZ, TargetX, TargetY, TargetZ, 0, 1, 0);
             // eye(x,y,z),         focal(x,y,z),        up(x,y,z)
end;


{-------------------------------------------------------------------------------
  Display info messages
-------------------------------------------------------------------------------}
procedure TForm1.ShowInfo;
var
  s: String;
  w, h, x, y: Integer;
begin
  // backup current model-view matrix
  glPushMatrix();                     // save current modelview matrix
  glLoadIdentity();                   // reset modelview matrix

  // set to 2D orthogonal projection
  glMatrixMode(GL_PROJECTION);        // switch to projection matrix
  glPushMatrix();                     // save current projection matrix
  glLoadIdentity();                   // reset projection matrix
  gluOrtho2D(0, OpenGLControl.Width, 0, OpenGLControl.Height); // set to orthogonal projection
//  glOrtho(0, OpenGLControl.Width, 0, OpenGLControl.Height, -1, 1);

  glDisable(GL_LIGHTING);

// write text
  glColor4f(1, 1, 1, 1);
  oglTextSize(FFixedFont, 'Tgj', w, h);
  x := 4;
  y := OpenGLControl.Height - h;
  if FDisplayListUsed then
    s := 'on'
  else
    s := 'off';
  oglTextOut(FFixedFont, x, y, 'Display List: ' + s);

  glEnable(GL_LIGHTING);

  // restore projection matrix
  glPopMatrix();                   // restore to previous projection matrix

  // restore modelview matrix
  glMatrixMode(GL_MODELVIEW);      // switch to modelview matrix
  glPopMatrix();                   // restore to previous modelview matrix
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

