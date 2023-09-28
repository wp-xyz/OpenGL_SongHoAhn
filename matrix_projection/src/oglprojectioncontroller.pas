///////////////////////////////////////////////////////////////////////////////
// Original file header by Song Ho Ahn
///////////////////////////////////////////////////////////////////////////////
// ModelGL.cpp
// ===========
// Model component of OpenGL
//
//  AUTHOR: Song Ho Ahn (song.ahn@gmail.com)
// CREATED: 2008-10-02
// UPDATED: 2018-06-07
///////////////////////////////////////////////////////////////////////////////
// Lazarus port by W.Pamler
///////////////////////////////////////////////////////////////////////////////

unit oglProjectionController;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  gl, glu, oglMath, oglCameraModel, oglSphere;

type
  TProjectionController = class
  private
    FBackColor: ToglVector4f;
    FDrawMode: Integer;
    FDrawModeChanged: Boolean;
    FFrustumVertices: array[0..7] of ToglVector3f;   // 8 vertices of frustum
    FFrustumNormals: array[0..5] of ToglVector3f;    // 6 face normals of frustom
    FMouseX, FMouseY: Integer;
    FProjectionBottom: GLfloat;
    FProjectionFar: GLfloat;
    FProjectionLeft: GLfloat;
    FProjectionMode: Integer;
    FProjectionNear: GLfloat;
    FProjectionRight: GLfloat;
    FProjectionTop: GLfloat;
    FSphere: ToglSphere;
    FWindowHeight: Integer;
    FWindowWidth: Integer;
    FWindowSizeChanged: Boolean;

    // These are for 3rd person view
    FCameraAngleX: GLfloat;
    FCameraAngleY: GLfloat;
    FCameraDistance: GLfloat;

    // 4x4 transform matrices
    FMatrixView: ToglMatrix4f;
    FMatrixModel: ToglMatrix4f;
    FMatrixModelView: ToglMatrix4f;
    FMatrixProjection: ToglMatrix4f;

    procedure SetDrawMode(AMode: Integer);
    procedure SetProjectionBottom(AValue: GLfloat);
    procedure SetProjectionFar(AValue: GLfloat);
    procedure SetProjectionLeft(AValue: GLfloat);
    procedure SetProjectionNear(AValue: GLfloat);
    procedure SetProjectionRight(AValue: GLfloat);
    procedure SetProjectionTop(AValue: GLfloat);

  protected
    procedure ComputeFrustumVertices(L, R, B, T, N, F: GLfloat);
    procedure DrawAxis(ASize: GLfloat);
    procedure DrawFrustum(FoVY, AspectRatio, NearPlane, FarPlane: GLfloat);
    procedure DrawFrustum(L, R, B, T, N, F: GLfloat);
    procedure DrawGrid(ASize, AStep: GLfloat);    // Draw a grid on xz plane
    procedure DrawSpheres;
    procedure DrawSub1;                           // Draw upper window
    procedure DrawSub2;                           // Draw bottom window
    procedure InitLights;       // Add a white light source to scene
    function SetFrustum(L, R, B, T, N, F: GLfloat): ToglMatrix4f;
    function SetFrustum(FOVY, AspectRatio, Front, Back: GLfloat): ToglMatrix4f;
    function SetOrthoFrustum(L, R, B, T: GLfloat; N:GLfloat = -1; F:GLfloat = +1): ToglMatrix4f;
    procedure SetViewport(x, y, w, h: Integer);
    procedure SetViewportSub(x, y, AWidth, AHeight: Integer; ANearPlane, AFarPlane: GLfloat);
    procedure SetViewportSub(x, y, AWidth, AHeight: Integer; ALeft, ARight, ABottom, ATop, AFront, ABack: GLfloat);
    procedure UpdateProjectionMatrix;
  public
    constructor Create;
    procedure Draw;
    procedure InitGL;       // Initialize OpenGL status
    procedure RotateCamera(X, Y: Integer);
    procedure SetCamera(PosX, PosY, PosZ, TargetX, TargetY, TargetZ: GLfloat);
    procedure SetMousePosition(X, Y: Integer);
    procedure SetProjection(L, R, B, T, N, F: GLfloat);
    procedure SetWindowSize(AWidth, AHeight: Integer);
    procedure ZoomCamera(y: Integer);
    procedure ZoomCameraDelta(Delta: GLfloat);

    property DrawMode: Integer read FDrawMode write SetDrawMode;
    property MatrixProjection: ToglMatrix4f read FMatrixProjection;
    property ProjectionBottom: GLfloat read FProjectionBottom write SetProjectionBottom;
    property ProjectionFar: GLfloat read FProjectionFar write SetProjectionFar;
    property ProjectionLeft: GLfloat read FProjectionLeft write SetProjectionLeft;
    property ProjectionMode: Integer read FProjectionMode write FProjectionMode;
    property ProjectionNear: GLfloat read FProjectionNear write SetProjectionNear;
    property ProjectionRight: GLfloat read FProjectionRight write SetProjectionRight;
    property ProjectionTop: GLfloat read FProjectionTop write SetProjectionTop;
  end;

implementation

const
  DEG2RAD = pi / 180;
  FOV_Y = 60.0;
  NEAR_PLANE = 1.0;
  FAR_PLANE = 100.0;
  CAMERA_ANGLE_X = 45.0;     // pitch in degree
  CAMERA_ANGLE_Y = -45.0;    // heading in degree
  CAMERA_DISTANCE = 25.0;    // camera distance

  // default projection matrix values
  DEFAULT_LEFT = -0.5;
  DEFAULT_RIGHT = 0.5;
  DEFAULT_BOTTOM = -0.5;
  DEFAULT_TOP = 0.5;
  DEFAULT_NEAR = 1.0;
  DEFAULT_FAR = 10.0;

constructor TProjectionController.Create;
begin
  inherited Create;

  FBackColor := Vector4f(0.0, 0.0, 0.0, 0.0);
  FCameraAngleX := CAMERA_ANGLE_X;
  FCameraAngleY := CAMERA_ANGLE_Y;
  FCameraDistance := CAMERA_DISTANCE;
  FProjectionLeft := DEFAULT_LEFT;
  FProjectionRight := DEFAULT_RIGHT;
  FProjectionBottom := DEFAULT_BOTTOM;
  FProjectionTop := DEFAULT_TOP;
  FProjectionNear := DEFAULT_NEAR;
  FProjectionFar := DEFAULT_FAR;
  FProjectionMode := 0;

  // Init sphere
  FSphere := ToglSphere.Create(0.5);

  // Init projection matrix
  FMatrixProjection := SetFrustum(
    FProjectionLeft, FProjectionRight,
    FProjectionBottom, FProjectionTop,
    FProjectionNear, FProjectionFar
  );

  // Model, view, modelview matrices are fixed in this app
  FMatrixModel.Identity();
  FMatrixView.Identity();
  FMatrixView.Translate(0, 0, -7); // move the camera away from 3D objects
  FMatrixModelView := FMatrixView * FMatrixModel;
end;

//------------------------------------------------------------------------------
// Compute 8 vertices of frustum
//------------------------------------------------------------------------------
procedure TProjectionController.ComputeFrustumVertices(L, R, B, T, N, F: GLfloat);
var
  ratio: GLfloat;
  farLeft: GLfloat;
  farRight: GLfloat;
  farBottom: GLfloat;
  farTop: GLfloat;
begin
  // Perspective mode
  if (FProjectionMode = 0) then
    ratio := F / N
  // orthographic mode
  else
    ratio := 1.0;

  farLeft   := L * ratio;
  farRight  := R * ratio;
  farBottom := B * ratio;
  farTop    := T * ratio;

  // Compute 8 vertices of the frustum
  // near top right
  FFrustumVertices[0] := Vector3f(R, T, -N);
  // near top left
  FFrustumVertices[1] := Vector3f(L, T, -N);
  // near bottom left
  FFrustumVertices[2] := Vector3f(L, B, -N);
  // Near bottom right
  FFrustumVertices[3] := Vector3f(R, B, -N);
  // Far top right
  FFrustumVertices[4] := Vector3f(farRight, farTop, -F);
  // far top left
  FFrustumVertices[5] := Vector3f(farLeft, farTop, -f);
  // far bottom left
  FFrustumVertices[6] := Vector3f(farLeft, farBottom, -f);
  // far bottom right
  FFrustumVertices[7] := Vector3f(farRight, farBottom, -f);

  // Compute normals
  FFrustumNormals[0] := CrossProduct(FFrustumVertices[5] - FFrustumVertices[1], FFrustumVertices[2] - FFrustumVertices[1]);
  FFrustumNormals[0].Normalize;

  FFrustumNormals[1] := CrossProduct(FFrustumVertices[3] - FFrustumVertices[0], FFrustumVertices[4] - FFrustumVertices[0]);
  FFrustumNormals[1].Normalize;

  FFrustumNormals[2] := CrossProduct(FFrustumVertices[6] - FFrustumVertices[2], FFrustumVertices[3] - FFrustumVertices[2]);
  FFrustumNormals[2].Normalize;

  FFrustumNormals[3] := CrossProduct(FFrustumVertices[4] - FFrustumVertices[0], FFrustumVertices[1] - FFrustumVertices[0]);
  FFrustumNormals[3].Normalize;

  FFrustumNormals[4] := CrossProduct(FFrustumVertices[1] - FFrustumVertices[0], FFrustumVertices[3] - FFrustumVertices[0]);
  FFrustumNormals[4].Normalize;

  FFrustumNormals[5] := CrossProduct(FFrustumVertices[7] - FFrustumVertices[4], FFrustumVertices[5] - FFrustumVertices[4]);
  FFrustumNormals[5].Normalize;
end;

//------------------------------------------------------------------------------
// Draw 2D/3D scene
//------------------------------------------------------------------------------
procedure TProjectionController.Draw;
begin
  if FWindowSizeChanged then
  begin
    SetViewport(0, 0, FWindowWidth, FWindowHeight);
    FWindowSizeChanged := false;
  end;

  if FDrawModeChanged then
  begin
    case FDrawMode of
      0: begin    // fill mode
           glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
           glEnable(GL_DEPTH_TEST);
           glEnable(GL_CULL_FACE);
         end;
      1: begin    // wireframe mode
           glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
           //glDisable(GL_DEPTH_TEST);
           glDisable(GL_CULL_FACE);
         end;
      2: begin    // point mode
          glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
          //glDisable(GL_DEPTH_TEST);
          glDisable(GL_CULL_FACE);
        end;
    end;
    FDrawModeChanged := false;
  end;

  DrawSub1;

  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_CULL_FACE);

  DrawSub2;
end;

//------------------------------------------------------------------------------
// Draw the local axis of an object
//------------------------------------------------------------------------------
procedure TProjectionController.DrawAxis(ASize: GLfloat);
begin
  glDepthFunc(GL_ALWAYS);     // to avoid visual artifacts with grid lines
  glDisable(GL_LIGHTING);

  // Draw axis
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

  // Draw arrows(actually big square dots)
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
  glDepthFunc(GL_LEQUAL);
end;

//------------------------------------------------------------------------------
// Draw frustum
//------------------------------------------------------------------------------
procedure TProjectionController.DrawFrustum(
  FoVY, AspectRatio, NearPlane, FarPlane: GLfloat);
var
  tangent: GLfloat;
  nearHeight, nearWidth: GLfloat;
begin
  tangent := tan(FovY / 2 * DEG2RAD);
  nearHeight := NearPlane * tangent;
  nearWidth := NearHeight * AspectRatio;

  DrawFrustum(-nearWidth, nearWidth, -nearHeight, nearHeight, nearPlane, farPlane);
end;


//------------------------------------------------------------------------------
// Draw frustum with 6 params (left, right, bottom, top, near, far)
//------------------------------------------------------------------------------
procedure TProjectionController.DrawFrustum(L, R, B, T, N, F: GLfloat);
const
  colorLine1: array[0..3] of GLfloat  = (0.7, 0.7, 0.7, 0.7);
  colorline2: array[0..3] of GLfloat  = (0.2, 0.2, 0.2, 0.7);
  colorPlane1: array[0..3] of GLfloat = (0.5, 0.5, 0.5, 0.4);
var
  i: Integer;
begin
  ComputeFrustumVertices(L, R, B, T, N, F);

  // Draw lines
  glDisable(GL_LIGHTING);
  glDisable(GL_CULL_FACE);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  // Draw the edges around frustum
  if (FProjectionMode = 0) then
  begin
    glBegin(GL_LINES);
      glColor4fv(@colorLine2);
      glVertex3f(0, 0, 0);
      glColor4fv(@ColorLine1);
      glVertex3fv(@FFrustumVertices[4].x);

      glColor4fv(@colorLine2);
      glVertex3f(0, 0, 0);
      glColor4fv(@ColorLine1);
      glVertex3fv(@FFrustumVertices[5].x);

      glColor4fv(@colorLine2);
      glVertex3f(0, 0, 0);
      glColor4fv(@colorLine1);
      glVertex3fv(@FFrustumVertices[6].x);

      glColor4fv(@colorLine2);
      glVertex3f(0, 0, 0);
      glColor4fv(@ColorLine1);
      glVertex3fv(@FFrustumVertices[7].x);
    glEnd();
  end else
  begin
    glColor4fv(@colorLine1);
    glBegin(GL_LINES);
      glVertex3fv(@FFrustumVertices[0].x);
      glVertex3fv(@FFrustumVertices[4].x);
      glVertex3fv(@FFrustumVertices[1].x);
      glVertex3fv(@FFrustumVertices[5].x);
      glVertex3fv(@FFrustumVertices[2].x);
      glVertex3fv(@FFrustumVertices[6].x);
      glVertex3fv(@FFrustumVertices[3].x);
      glVertex3fv(@FFrustumVertices[7].x);
    glEnd();
  end;

  glColor4fv(@colorLine1);
  glBegin(GL_LINE_LOOP);
    glVertex3fv(@FFrustumVertices[4].x);
    glVertex3fv(@FFrustumVertices[5].x);
    glVertex3fv(@FFrustumVertices[6].x);
    glVertex3fv(@FFrustumVertices[7].x);
  glEnd();

  glColor4fv(@colorLine1);
  glBegin(GL_LINE_LOOP);
    glVertex3fv(@FFrustumVertices[0].x);
    glVertex3fv(@FFrustumVertices[1].x);
    glVertex3fv(@FFrustumVertices[2].x);
    glVertex3fv(@FFrustumVertices[3].x);
  glEnd();

  glEnable(GL_CULL_FACE);
  glEnable(GL_LIGHTING);

  // Frustum is transparent.
  // Draw the frustum faces twice: backfaces first then frontfaces second.
  for i := 0 to 1 do
  begin
    if (i = 0) then
    begin
      // for inside planes
      //glCullFace(GL_FRONT);
      //glLightModelf(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
    end else
    begin
      // draw outside planes
      glCullFace(GL_BACK);
      glLightModelf(GL_LIGHT_MODEL_TWO_SIDE, GL_FALSE);
    end;

    glColor4fv(@colorPlane1);
    glBegin(GL_QUADS);
      // left
      glNormal3fv(@FFrustumNormals[0].x);
      glVertex3fv(@FFrustumVertices[1].x);
      glVertex3fv(@FFrustumVertices[5].x);
      glVertex3fv(@FFrustumVertices[6].x);
      glVertex3fv(@FFrustumVertices[2].x);
      // right
      glNormal3fv(@FFrustumNormals[1].x);
      glVertex3fv(@FFrustumVertices[0].x);
      glVertex3fv(@FFrustumVertices[3].x);
      glVertex3fv(@FFrustumVertices[7].x);
      glVertex3fv(@FFrustumVertices[4].x);
      // bottom
      glNormal3fv(@FFrustumNormals[2].x);
      glVertex3fv(@FFrustumVertices[2].x);
      glVertex3fv(@FFrustumVertices[6].x);
      glVertex3fv(@FFrustumVertices[7].x);
      glVertex3fv(@FFrustumVertices[3].x);
      // top
      glNormal3fv(@FFrustumNormals[3].x);
      glVertex3fv(@FFrustumVertices[0].x);
      glVertex3fv(@FFrustumVertices[4].x);
      glVertex3fv(@FFrustumVertices[5].x);
      glVertex3fv(@FFrustumVertices[1].x);
      // front
      glNormal3fv(@FFrustumNormals[4].x);
      glVertex3fv(@FFrustumVertices[0].x);
      glVertex3fv(@FFrustumVertices[1].x);
      glVertex3fv(@FFrustumVertices[2].x);
      glVertex3fv(@FFrustumVertices[3].x);
      // back
      glNormal3fv(@FFrustumNormals[5].x);
      glVertex3fv(@FFrustumVertices[7].x);
      glVertex3fv(@FFrustumVertices[6].x);
      glVertex3fv(@FFrustumVertices[5].x);
      glVertex3fv(@FFrustumVertices[4].x);
    glEnd;
  end;
end;

//------------------------------------------------------------------------------
// Draw a grid on the xz plane
//------------------------------------------------------------------------------
procedure TProjectionController.DrawGrid(ASize, AStep: GLfloat);
var
  step: GLfloat;
begin
  // disable lighting
  glDisable(GL_LIGHTING);

  glBegin(GL_LINES);

    glColor3f(0.3, 0.3, 0.3);
    step := AStep;
    while step <= ASize do
    begin
      glVertex3f(-ASize, 0,  step);   // lines parallel to X-axis
      glVertex3f( ASize, 0,  step);
      glVertex3f(-ASize, 0, -step);   // lines parallel to X-axis
      glVertex3f( ASize, 0, -step);

      glVertex3f( step, 0, -ASize);   // lines parallel to Z-axis
      glVertex3f( step, 0,  ASize);
      glVertex3f(-step, 0, -ASize);   // lines parallel to Z-axis
      glVertex3f(-step, 0,  ASize);

      step  := step + AStep;
    end;

    // x-axis
    glColor3f(0.5, 0, 0);
    glVertex3f(-ASize, 0, 0);
    glVertex3f( ASize, 0, 0);

    // z-axis
    glColor3f(0, 0, 0.5);
    glVertex3f(0, 0, -ASize);
    glVertex3f(0, 0,  ASize);

  glEnd();

  // Enable lighting back
  glEnable(GL_LIGHTING);
end;

//------------------------------------------------------------------------------
// Daw spheres
//------------------------------------------------------------------------------
procedure TProjectionController.DrawSpheres;
const
  COLOR0: array[0..3] of GLfloat = (0.7, 0.7, 0.7, 1.0);
  COLOR1: array[0..3] of GLfloat = (1.0, 0.0, 0.0, 1.0);
  COLOR2: array[0..3] of GLfloat = (1.0, 0.6, 0.0, 1.0);
  COLOR3: array[0..3] of GLfloat = (1.0, 1.0, 0.0, 1.0);
  COLOR4: array[0..3] of GLfloat = (0.0, 1.0, 0.0, 1.0);
  COLOR5: array[0..3] of GLfloat = (0.0, 1.0, 1.0, 1.0);
  COLOR6: array[0..3] of GLfloat = (0.0, 0.0, 1.0, 1.0);
  COLOR7: array[0..3] of GLfloat = (1.0, 0.0, 1.0, 1.0);
  SPECULARCOLOR: array[0..3] of GLfloat = (1.0, 1.0, 1.0, 1.0);
  SHININESS: GLfloat = 128.0;
begin
  // Set default specular and shiniess using glMaterial
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, shininess); // range 0 ~ 128
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @SPECULARCOLOR);
  glMaterialfv(GL_FRONT, GL_AMBIENT, @COLOR0);

  glPushMatrix;
  glTranslatef(0, 0, 3);
  //glColor3fv(color1);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR1);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(1, 0, 2);
  //glColor3fv(color2);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR2);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(-1, 0, 2);
  //glColor3fv(color2);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR2);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(0, 1, 2);
  //glColor3fv(color2);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR2);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(0, -1, 2);
  //glColor3fv(color2);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR2);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(2, 0, 1);
  //glColor3fv(color3);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR3);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(-2, 0, 1);
  //glColor3fv(color3);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR3);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(0, 2, 1);
  //glColor3fv(color3);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR3);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(0, -2, 1);
  //glColor3fv(color3);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR3);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(3, 0, 0);
  //glColor3fv(color4);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR4);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(-3, 0, 0);
  //glColor3fv(color4);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR4);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(0, 3, 0);
  //glColor3fv(color4);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR4);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(0, -3, 0);
  //glColor3fv(color4);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR4);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(4, 0, -1);
  //glColor3fv(color5);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR5);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(-4, 0, -1);
  //glColor3fv(color5);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR5);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(0, 4, -1);
  //glColor3fv(color5);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR5);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(0, -4, -1);
  //glColor3fv(color5);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR5);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(5, 0, -2);
  //glColor3fv(color6);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR6);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(-5, 0, -2);
  //glColor3fv(color6);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR6);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(0, 5, -2);
  //glColor3fv(color6);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR6);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(0, -5, -2);
  //glColor3fv(color6);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR6);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(6, 0, -3);
  //glColor3fv(color7);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR7);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix();
  glTranslatef(-6, 0, -3);
  //glColor3fv(color7);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR7);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(0, 6, -3);
  //glColor3fv(color7);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR7);
  FSphere.Draw;
  glPopMatrix;

  glPushMatrix;
  glTranslatef(0, -6, -3);
  //glColor3fv(color7);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, @COLOR7);
  FSphere.Draw;
  glPopMatrix;
end;

//------------------------------------------------------------------------------
// Draw upper window (view from the camera)
//------------------------------------------------------------------------------
procedure TProjectionController.DrawSub1;
var
  halfWidth: Integer;
begin
  halfWidth := FWindowWidth div 2;

  // Clear buffer
  SetViewportSub(0, 0, halfWidth, FWindowHeight,
    FProjectionLeft, FProjectionRight, FProjectionBottom, FProjectionTop,
    FProjectionNear, FProjectionFar
  );
  glClearColor(0.15, 0.15, 0.15, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

  glPushMatrix;

  // Copy  ModelView matrix to OpenGL after transpose
  // This matrix is fixed and pre-computed in constructor
  glLoadMatrixf(FMatrixModelView.Get());

  glDisable(GL_COLOR_MATERIAL);
  DrawSpheres;
  glEnable(GL_COLOR_MATERIAL);

  glPopMatrix;
end;

//------------------------------------------------------------------------------
// Draw bottom window (3rd person view)
//------------------------------------------------------------------------------
procedure TProjectionController.DrawSub2;
var
  halfWidth: Integer;
  matProj, matView, matModel, matModelView: ToglMatrix4f;
begin
  halfWidth := FWindowWidth div 2;

  // Set right viewport (perspective)
  glViewport(halfWidth, 0, halfWidth, FWindowHeight);
  glScissor(halfWidth, 0, halfWidth, FWindowHeight);

  // Set perspective viewing frustum
  matProj := SetFrustum(FOV_Y, halfWidth/FWindowHeight, 1, 1000); // FOV, AspectRatio, NearClip, FarClip
  glMatrixMode(GL_PROJECTION);
  glLoadMatrixf(matProj.Get);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  // Clear buffer
  glClearColor(FBackColor[0], FBackColor[1], FBackColor[2], FBackColor[3]);   // background color
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

  glPushMatrix();

  // First, transform the camera (viewing matrix) from world space to eye space
  matView.Identity;
  matView.RotateY(FCameraAngleY);
  matView.RotateX(FCameraAngleX);
  matView.Translate(0, 0, -FCameraDistance);
  glLoadMatrixf(matView.Get);
  // equivalent OpenGL calls
  //glTranslatef(0, 0, -FCameraDistance);
  //glRotatef(FCameraAngleX, 1, 0, 0); // pitch
  //glRotatef(FCameraAngleY, 0, 1, 0); // heading

  // Draw grid
  DrawGrid(10, 1);

  // Draw balls
  glDisable(GL_COLOR_MATERIAL);
  drawSpheres();
  glEnable(GL_COLOR_MATERIAL);

  // Draw the camera
  matModel.Identity;
  matModel.Translate(0, 0, 7);
  matModelView := matView * matModel;
  glLoadMatrixf(matModelView.Get);

  DrawCamera();
  DrawFrustum(FProjectionLeft, FProjectionRight, FProjectionBottom, FProjectionTop,
    FProjectionNear, FProjectionFar
  );

  glPopMatrix();
end;

//------------------------------------------------------------------------------
// Initialize OpenGL states and scene
//------------------------------------------------------------------------------
procedure TProjectionController.InitGL;
begin
  glShadeModel(GL_SMOOTH);                        // shading mathod: GL_SMOOTH or GL_FLAT
  glPixelStorei(GL_UNPACK_ALIGNMENT, 4);          // 4-byte pixel alignment

  // Enable/disable features
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  //glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  //glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_CULL_FACE);
  glEnable(GL_BLEND);
  glEnable(GL_SCISSOR_TEST);

  // Track material ambient and diffuse from surface color, call it before glEnable(GL_COLOR_MATERIAL)
  glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_COLOR_MATERIAL);

  glClearColor(FBackColor[0], FBackColor[1], FBackColor[2], FBackColor[3]);   // background color
  glClearStencil(0);                              // clear stencil buffer
  glClearDepth(1.0);                             // 0 is near, 1 is far
  glDepthFunc(GL_LEQUAL);

  InitLights();
end;

//------------------------------------------------------------------------------
// Initialize lights
//------------------------------------------------------------------------------
procedure TProjectionController.InitLights;
const
  lightKa: Array[0..3] of GLfloat  = (0.2, 0.2, 0.2, 1.0);  // ambient light
  lightKd: Array[0..3] of GLfloat  = (0.8, 0.8, 0.8, 1.0);  // diffuse light
  lightKs: Array[0..3] of GLfloat  = (1.0, 1.0, 1.0, 1.0);  // specular light
  lightPos: Array[0..3] of GLfloat = (0.0, 0.0, 1.0, 0.0);  // directional light
begin
  // Set up light colors (ambient, diffuse, specular)
  glLightfv(GL_LIGHT0, GL_AMBIENT, @lightKa);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @lightKd);
  glLightfv(GL_LIGHT0, GL_SPECULAR, @lightKs);

  // Position the light in eye space
  glLightfv(GL_LIGHT0, GL_POSITION, @lightPos);

  glEnable(GL_LIGHT0);      // MUST enable each light source after configuration
end;

//------------------------------------------------------------------------------
// Rotate the camera for subWin2
//------------------------------------------------------------------------------
procedure TProjectionController.RotateCamera(X, Y: Integer);
begin
  FCameraAngleY := FCameraAngleY + (X - FMouseX);
  FCameraAngleX := FCameraAngleX + (Y - FMouseY);
  FMouseX := x;
  FMouseY := y;
end;

//------------------------------------------------------------------------------
// Set camera position and lookat direction
//------------------------------------------------------------------------------
procedure TProjectionController.SetCamera(
  PosX, PosY, PosZ, TargetX, TargetY, TargetZ: GLfloat);
var
  lForward: ToglVector4f;
  lUp: ToglVector4f;
  lLeft: ToglVector4f;
  lPosition: ToglVector4f;
  invLength: GLfloat;
begin
  // Determine forward vector (direction reversed because it is camera)
  lForward[0] := PosX - TargetX;    // x
  lForward[1] := PosY - TargetY;    // y
  lForward[2] := PosZ - TargetZ;    // z
  lForward[3] := 0.0;               // w

  // Normalize it without w-component
  invLength := 1.0 / sqrt(sqr(lForward[0]) + sqr(lForward[1]) +  sqr(lForward[2]));
  lForward[0] := lForward[0] * invLength;
  lForward[1] := lForward[1] * invLength;
  lForward[2] := lForward[2] * invLength;

  // Assume up direction is straight up
  lUp[0] := 0.0;   // x
  lUp[1] := 1.0;   // y
  lUp[2] := 0.0;   // z
  lUp[3] := 0.0;   // w

  // Compute left vector with cross product
  lLeft[0] := lUp[1]*lForward[2] - lUp[2]*lForward[1];  // x
  lLeft[1] := lUp[2]*lForward[0] - lUp[0]*lForward[2];  // y
  lLeft[2] := lUp[0]*lForward[1] - lUp[1]*lForward[0];  // z
  lLeft[3] := 1.0;                                      // w

  // Recompute orthogonal up vector
  lUp[0] := lForward[1]*lLeft[2] - lForward[2]*lLeft[1];    // x
  lUp[1] := lForward[2]*lLeft[0] - lforward[0]*lLeft[2];    // y
  lUp[2] := lForward[0]*lLeft[1] - lForward[1]*lLeft[0];    // z
  lUp[3] := 0.0;                                            // w

  // camera position
  lPosition[0] := -PosX;
  lPosition[1] := -PosY;
  lPosition[2] := -PosZ;
  lPosition[3] := 1.0;

  //  axis vectors to matrix
  FMatrixView.Identity();
  FMatrixView.SetColumn(0, lLeft);
  FMatrixView.SetColumn(1, lUp);
  FMatrixView.SetColumn(2, lForward);
  FMatrixView.SetColumn(3, lPosition);
end;

//------------------------------------------------------------------------------
// Change drawing mode
//------------------------------------------------------------------------------
procedure TProjectionController.SetDrawMode(AMode: Integer);
begin
  if FDrawMode <> AMode then
  begin
    FDrawModeChanged := true;
    FDrawMode := AMode;
  end;
end;

//------------------------------------------------------------------------------
// Return a perspective frustum with 6 params similar to glFrustum()
// (left, right, bottom, top, near, far)
//------------------------------------------------------------------------------
function TProjectionController.SetFrustum(L, R, B, T, N, F: GLfloat): ToglMatrix4f;
begin
  Result.Identity;
  Result[0]  :=  2 * N / (R - L);
  Result[5]  :=  2 * N / (T - B);
  Result[8]  :=  (R + L) / (R - L);
  Result[9]  :=  (T + B) / (T - B);
  Result[10] := -(F + N) / (F - N);
  Result[11] := -1;
  Result[14] := -(2 * F * N) / (F - N);
  Result[15] :=  0;
end;

//------------------------------------------------------------------------------
// Set a symmetric perspective frustum with 4 params similar to gluPerspective
// (vertical field of view, aspect ratio, near, far)
//------------------------------------------------------------------------------
function TProjectionController.SetFrustum(FOVY, AspectRatio, Front, Back: GLfloat): ToglMatrix4f;
var
  tangent: GLfloat;
  lHeight: GLfloat;
  lWidth: GLfloat;
begin
  tangent := tan(FOVY/2 * DEG2RAD);     // Tangent of half fovY
  lHeight := Front * tangent;           // Half height of near plane
  lWidth := lHeight * AspectRatio;      // Half width of near plane

   // Params: left, right, bottom, top, near, far
  Result := SetFrustum(-lWidth, lWidth, -lHeight, lHeight, Front, Back);
end;

procedure TProjectionController.SetMousePosition(X, Y: Integer);
begin
  FMouseX := X;
  FMouseY := Y;
end;

//------------------------------------------------------------------------------
// Set a orthographic frustum with 6 params similar to glOrtho()
// (left, right, bottom, top, near, far)
//------------------------------------------------------------------------------
function TProjectionController.SetOrthoFrustum(L, R, B, T, N, F: GLfloat): ToglMatrix4f;
begin
  Result.Identity;
  Result[0]  :=  2 / (R - L);
  Result[5]  :=  2 / (T - B);
  Result[10] := -2 / (F - N);
  Result[12] := -(R + L) / (R - L);
  Result[13] := -(T + B) / (T - B);
  Result[14] := -(F + N) / (F - N);
end;

//------------------------------------------------------------------------------
// Set 6 params of frustum
//------------------------------------------------------------------------------
procedure TProjectionController.SetProjection(L, R, B, T, N, F: GLfloat);
begin
  FProjectionLeft := L;
  FProjectionRight := R;
  FProjectionBottom := B;
  FProjectionTop := T;
  FProjectionNear := N;
  FProjectionFar := F;

  case FProjectionMode of
    0: FMatrixProjection := SetFrustum(L, R, B, T, N, F);
    1: FMatrixProjection := SetOrthoFrustum(L, R, B, T, N, F);
    else raise Exception.Create('Unknown projection mode');
  end;
end;

procedure TProjectionController.SetProjectionBottom(AValue: GLfloat);
begin
  FProjectionBottom := AValue;
  UpdateProjectionMatrix;
end;

procedure TProjectionController.SetProjectionFar(AValue: GLfloat);
begin
  FProjectionfar := AValue;
  UpdateProjectionMatrix;
end;

procedure TProjectionController.SetProjectionLeft(AValue: GLfloat);
begin
  FProjectionLeft := AValue;
  UpdateProjectionMatrix;
end;

procedure TProjectionController.SetProjectionNear(AValue: GLfloat);
begin
  FProjectionNear := AValue;
  UpdateProjectionMatrix;
end;

procedure TProjectionController.SetProjectionRight(AValue: GLfloat);
begin
  FProjectionRight := AValue;
  UpdateProjectionMatrix;
end;

procedure TProjectionController.SetProjectionTop(AValue: GLfloat);
begin
  FProjectionTop := AValue;
  UpdateProjectionMatrix;
end;

//------------------------------------------------------------------------------
// Configure projection and viewport
//------------------------------------------------------------------------------
procedure TProjectionController.SetViewport(x, y, w, h: Integer);
var
  matrix: ToglMatrix4f;
begin
  // Set viewport to be the entire window
  glViewport(GLsizei(x), GLsizei(y), GLsizei(w), GLsizei(h));

  // Set perspective viewing frustum
  matrix := SetFrustum(FOV_Y, w/h, NEAR_PLANE, FAR_PLANE); // FOV, AspectRatio, NearClip, FarClip

  // Copy projection matrix to OpenGL
  glMatrixMode(GL_PROJECTION);
  glLoadMatrixf(matrix.Get());
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
end;

//------------------------------------------------------------------------------
// Configure projection and viewport of sub window
//------------------------------------------------------------------------------
procedure TProjectionController.SetViewportSub(x, y, AWidth, AHeight: Integer;
  ANearPlane, AFarPlane: GLfloat);
var
  matrix: ToglMatrix4f;
begin
  // Set viewport
  glViewport(x, y, AWidth, AHeight);
  glScissor(x, y, AWidth, AHeight);

  // Set perspective viewing frustum
  matrix := SetFrustum(FOV_Y, AWidth/AHeight, ANearPlane, AFarPlane); // FOV, AspectRatio, NearClip, FarClip

  // Copy projection matrix to OpenGL
  glMatrixMode(GL_PROJECTION);
  glLoadMatrixf(matrix.get());
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
end;

procedure TProjectionController.SetViewportSub(x, y, AWidth, AHeight: Integer;
  ALeft, ARight, ABottom, ATop, AFront, ABack: GLfloat);
var
  matrix: ToglMatrix4f;
begin
  // Set viewport
  glViewport(x, y, AWidth, AHeight);
  glScissor(x, y, AWidth, AHeight);

  // Set viewing frustum
  if (FProjectionMode = 0) then // perspective
    matrix := SetFrustum(ALeft, ARight, ABottom, ATop, AFront, ABack)
  else                         // orthographic
    matrix := SetOrthoFrustum(ALeft, ARight, ABottom, aTop, AFront, aBack);

  // ACopy projection matrix to OpenGL
  glMatrixMode(GL_PROJECTION);
  glLoadMatrixf(matrix.Get());
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
end;

//------------------------------------------------------------------------------
// Set rendering window size
//------------------------------------------------------------------------------
procedure TProjectionController.SetWindowSize(AWidth, AHeight: Integer);
begin
  // Assign the width/height of viewport
  FWindowWidth := AWidth;
  FWindowHeight := AHeight;
  FWindowSizeChanged := true;
end;

//------------------------------------------------------------------------------
// Update projection matrix
//------------------------------------------------------------------------------
procedure TProjectionController.UpdateProjectionMatrix;
begin
  case FProjectionMode of
    0: // perspective
      FMatrixProjection := SetFrustum(
        FProjectionLeft, FProjectionRight, FProjectionBottom, FProjectionTop,
        FProjectionNear, FProjectionFar
      );
    1: // orthographic
      FMatrixProjection := SetOrthoFrustum(
        FProjectionLeft, FProjectionRight,
        FProjectionBottom, FProjectionTop,
        FProjectionNear, FProjectionFar);
  end;
end;

//------------------------------------------------------------------------------
// Zoom the camera for subWin2
//------------------------------------------------------------------------------
procedure TProjectionController.ZoomCamera(y: Integer);
begin
  FCameraDistance := FCameraDistance - (y - FMouseY) * 0.1;
  FMouseY := y;
end;

procedure TProjectionController.ZoomCameraDelta(Delta: GLfloat);
begin
  FCameraDistance := FCameraDistance - Delta;
end;

end.

