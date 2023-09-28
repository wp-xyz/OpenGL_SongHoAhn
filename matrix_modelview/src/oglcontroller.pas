unit oglController;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  gl, glu, oglTypes, oglMath, oglCamera;

type
  TModelViewController = class
  private
    // Members
    FBackColor: ToglVector4f;

    FWindowWidth: Integer;
    FWindowHeight: Integer;
    FWindowSizeChanged: Boolean;
    FPovWidth: Integer;

    // Mouse
    FMouseX, FMouseY: Integer;

    // Cameras
    FCameraAngle: ToglVector3f;
    FCameraPosition: ToglVector3f;
    FModelPosition: ToglVector3f;
    FModelAngle: ToglVector3f;
    FMatrixView: ToglMatrix4f;
    FMatrixModel: ToglMatrix4f;
    FMatrixModelView: ToglMatrix4f;
    FMatrixProjection: ToglMatrix4f;

    // Draw mode
    FDrawMode: Integer;
    FDrawModeChanged: Boolean;

    // 3rd person view
    FCameraDistance: GLfloat;
    FCameraAngleX: GLfloat;
    FCameraAngleY: GLfloat;

    // User events for painting the scene
    FOnDrawCamera: TNotifyEvent;
    FOnDrawScene: TNotifyEvent;

    procedure DrawAxis(ASize: GLfloat);
    procedure DrawFrustum(AFieldOfView, AspectRatio, ANearPlane, AFarPlane: GLfloat);
    procedure DrawGrid(ASize, AStep: GLfloat);
    procedure DrawSub1;
    procedure DrawSub2;
    procedure InitLights;
    procedure SetCamera(PosX, PosY, PosZ, TargetX, TargetY, TargetZ: GLfloat);
    procedure SetCameraAngleX(AngleX: GLfloat);
    procedure SetCameraAngleY(AngleY: GLfloat);
    procedure SetCameraAngleZ(AngleZ: GLfloat);
    procedure SetCameraX(PosX: GLfloat);
    procedure SetCameraY(PosY: GLfloat);
    procedure SetCameraZ(PosZ: GLfloat);
    procedure SetDrawMode(AMode: Integer);
    function SetFrustum(L, R, B, T, N, F: GLfloat): ToglMatrix4f;
    function SetFrustum(AFieldOfView, AspectRatio, AFront, ABack: GLfloat): ToglMatrix4f;
    procedure SetModelAngleX(AngleX: GLfloat);
    procedure SetModelAngleY(AngleY: GLfloat);
    procedure SetModelAngleZ(AngleZ: GLfloat);
    procedure SetModelX(PosX: GLfloat);
    procedure SetModelY(PosY: GLfloat);
    procedure SetModelZ(PosZ: GLfloat);
    function SetOrthoFrustum(L, R, B, T, N, F: GLfloat): ToglMatrix4f;

    procedure SetViewMatrix(x, y, z, pitch, heading, roll: GLfloat);
    procedure SetViewport(x, y, w, h: Integer);
    procedure SetViewportSub(x, y, AWidth, AHeight: Integer; ANearPlane, AFarPlane: GLfloat);
    procedure UpdateModelMatrix;
    procedure UpdateViewMatrix;

  protected
    procedure DrawCamera; virtual;
    procedure DrawModel; virtual;

  public
    constructor Create;
    procedure Draw;
    procedure InitGL;
    procedure ResetModelMatrix;
    procedure ResetViewMatrix;
    procedure RotateCamera(x, y: Integer);
    procedure SetModelMatrix(x, y, z, rx, ry, rz: GLfloat);
    procedure SetMousePosition(X, Y: Integer);
    procedure SetWindowSize(AWidth, AHeight: Integer);
    procedure ZoomCamera(y: Integer);
    procedure ZoomCameraDelta(Delta: GLfloat);
    property CameraAngleX: GLfloat read FCameraAngle.X write SetCameraAngleX;
    property CameraAngleY: GLfloat read FCameraAngle.Y write SetCameraAngleY;
    property CameraAngleZ: GLfloat read FCameraAngle.Z write SetCameraAngleZ;
    property CameraX: GLfloat read FCameraPosition.X write SetCameraX;
    property CameraY: GLfloat read FCameraPosition.Y write SetCameraY;
    property CameraZ: GLfloat read FCameraPosition.Z write SetCameraZ;
    property MatrixModel: ToglMatrix4f read FMatrixModel;
    property MatrixModelView: ToglMatrix4f read FMatrixModelView;
    property MatrixView: ToglMatrix4f read FMatrixView;
    property ModelX: GLfloat read FModelPosition.X write SetModelX;
    property ModelY: GLfloat read FModelPosition.Y write SetModelY;
    property ModelZ: GLfloat read FModelPosition.Z write SetModelZ;
    property ModelAngleX: GLfloat read FModelAngle.X write SetModelAngleX;
    property ModelAngleY: GLfloat read FModelAngle.Y write SetModelAngleY;
    property ModelAngleZ: GLfloat read FModelAngle.Z write SetModelAngleZ;
    property OnDrawCamera: TNotifyEvent read FOnDrawCamera write FOnDrawCamera;
    property OnDrawScene: TNotifyEvent read FOnDrawScene write FOnDrawScene;
  end;

implementation

const
  DEG2RAD = pi / 180;
  FOV_Y = 60.0;              // vertical FOV in degrees
  NEAR_PLANE = 1.0;
  FAR_PLANE = 100.0;
  CAMERA_ANGLE_X = 45.0;     // pitch in degrees in 3rd person view
  CAMERA_ANGLE_Y = -45.0;    // heading in degrees in 3rd person vies
  CAMERA_DISTANCE = 25.0;    // camera distance in 3rd person view

constructor TModelViewController.Create;
begin
  inherited Create;

  // camera in 3rd person view (controlled by mouse)
  FCameraDistance := CAMERA_DISTANCE;
  FCameraAngleX := CAMERA_ANGLE_X;
  FCameraAngleY := CAMERA_ANGLE_Y;

  // normal camera
  FCameraPosition := Vector3f(0.0, 0.0, 10.0);
  FCameraAngle := Vector3f(0.0, 0.0, 0.0);

  // model
  FModelPosition := Vector3f(0.0, 0.0, 0.0);
  FModelAngle := Vector3f(0.0, 0.0, 0.0);

  FBackColor := Vector4f(0.0, 0.0, 0.0, 0.0);

  FMatrixView.Identity();
  FMatrixModel.Identity();
  FMatrixModelView.Identity();
  FMatrixProjection.Identity();
end;

//------------------------------------------------------------------------------
// Draw 2D/3D scene
//------------------------------------------------------------------------------
procedure TModelViewController.Draw;
begin
  DrawSub1();
  DrawSub2();

  // post frame
  if FWindowSizeChanged then
  begin
    SetViewPort(0, 0, FWindowWidth, FWindowHeight);
    FWindowSizeChanged := false;
  end;

  if FDrawModeChanged then
  begin
    case FDrawMode of
      0: begin  // fill mode
           glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
           glEnable(GL_DEPTH_TEST);
           glEnable(GL_CULL_FACE);
         end;
      1: begin   // wireframe mode
           glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
           //glDisable(GL_DEPTH_TEST);
           glDisable(GL_CULL_FACE);
         end;
      2: begin  // point mode
           glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
           //glDisable(GL_DEPTH_TEST);
           glDisable(GL_CULL_FACE);
         end;
    end;
    FDrawModeChanged := false;
  end;
end;

//------------------------------------------------------------------------------
// Draw the local axis of an object
//------------------------------------------------------------------------------
procedure TModelViewController.DrawAxis(ASize: GLfloat);
begin
  glDepthFunc(GL_ALWAYS);     // to avoid visual artifacts with grid lines
  glDisable(GL_LIGHTING);
  glPushMatrix();
  //NOTE: There is a bug on Mac misbehaviours of the light position when you
  //      draw GL_LINES and GL_POINTS. --> Remember the matrix.

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

  // Draw arrows (actually big square dots)
  glPointSize(8);
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
  glPopMatrix();
  glEnable(GL_LIGHTING);
  glDepthFunc(GL_LEQUAL);
end;

//------------------------------------------------------------------------------
// Draws the camera
//------------------------------------------------------------------------------
procedure TModelViewController.DrawCamera;
begin
  if Assigned(FOnDrawCamera) then
    FOnDrawCamera(Self);
end;

//------------------------------------------------------------------------------
// Draw frustum
//------------------------------------------------------------------------------
procedure TModelViewController.DrawFrustum(
  AFieldOfView, AspectRatio, ANearPlane, AFarPlane: GLfloat);
const
  colorLine1: array[0..3] of GLfloat = (0.7, 0.7, 0.7, 0.7);
  colorLine2: array[0..3] of GLfloat = (0.2, 0.2, 0.2, 0.7);
  colorPlane: array[0..3] of GLfloat = (0.5, 0.5, 0.5, 0.5);
var
  tangent, nearHeight, nearWidth, farHeight, farWidth: GLfloat;
  vertices: array[0..7, 0..2] of GLfloat;
begin
  tangent := tan(AFieldOfView/2 * DEG2RAD);
  nearHeight := ANearPlane * tangent;
  nearWidth := nearHeight * AspectRatio;
  farHeight := AFarPlane * tangent;
  farWidth := farHeight * AspectRatio;

  // Compute 8 vertices of the frustum:
  // near top right
  vertices[0,0] :=  nearWidth;   vertices[0,1] :=  nearHeight;   vertices[0,2] := -ANearPlane;
  // near top left
  vertices[1,0] := -nearWidth;   vertices[1,1] :=  nearHeight;   vertices[1,2] := -ANearPlane;
  // near bottom left
  vertices[2,0] := -nearWidth;   vertices[2,1] := -nearHeight;   vertices[2,2] := -ANearPlane;
  // near bottom right
  vertices[3,0] :=  nearWidth;   vertices[3,1] := -nearHeight;   vertices[3,2] := -ANearPlane;
  // far top right
  vertices[4,0] :=  farWidth;    vertices[4,1] :=  farHeight;    vertices[4,2] := -AFarPlane;
  // far top left
  vertices[5,0] := -farWidth;    vertices[5,1] :=  farHeight;    vertices[5,2] := -AFarPlane;
  // far bottom left
  vertices[6,0] := -farWidth;    vertices[6,1] := -farHeight;    vertices[6,2] := -AFarPlane;
  // far bottom right
  vertices[7,0] :=  farWidth;    vertices[7,1] := -farHeight;    vertices[7,2] := -AFarPlane;

  glDisable(GL_LIGHTING);
  glDisable(GL_CULL_FACE);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  // Draw the edges around frustum
  glBegin(GL_LINES);

    glColor4fv(@colorLine2);
    glVertex3f(0, 0, 0);
    glColor4fv(@colorLine1);
    glVertex3fv(vertices[4]);

    glColor4fv(@colorLine2);
    glVertex3f(0, 0, 0);
    glColor4fv(@colorLine1);
    glVertex3fv(vertices[5]);

    glColor4fv(@colorLine2);
    glVertex3f(0, 0, 0);
    glColor4fv(@colorLine1);
    glVertex3fv(vertices[6]);

    glColor4fv(@colorLine2);
    glVertex3f(0, 0, 0);
    glColor4fv(@colorLine1);
    glVertex3fv(vertices[7]);
    glEnd();

    glColor4fv(@colorLine1);
    glBegin(GL_LINE_LOOP);
    glVertex3fv(vertices[4]);
    glVertex3fv(vertices[5]);
    glVertex3fv(vertices[6]);
    glVertex3fv(vertices[7]);
  glEnd();

  glColor4fv(@colorLine1);
  glBegin(GL_LINE_LOOP);
    glVertex3fv(vertices[0]);
    glVertex3fv(vertices[1]);
    glVertex3fv(vertices[2]);
    glVertex3fv(vertices[3]);
  glEnd();

  // Draw near and far plane
  glColor4fv(@colorPlane);
  glBegin(GL_QUADS);
    glVertex3fv(vertices[0]);
    glVertex3fv(vertices[1]);
    glVertex3fv(vertices[2]);
    glVertex3fv(vertices[3]);
    glVertex3fv(vertices[4]);
    glVertex3fv(vertices[5]);
    glVertex3fv(vertices[6]);
    glVertex3fv(vertices[7]);
  glEnd();

  glEnable(GL_CULL_FACE);
  glEnable(GL_LIGHTING);
end;

//------------------------------------------------------------------------------
// Draw a grid on the xz plane
//------------------------------------------------------------------------------
procedure TModelViewController.DrawGrid(ASize, AStep: GLfloat);
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

      step := step + AStep;
    end;

    // x-axis
    glColor3f(0.6, 0.2, 0.2);
    glVertex3f(-ASize, 0, 0);
    glVertex3f( ASize, 0, 0);

    // z-axis
    glColor3f(0.2, 0.2, 0.6);
    glVertex3f(0, 0, -ASize);
    glVertex3f(0, 0,  ASize);

  glEnd();

  // enable lighting back
  glEnable(GL_LIGHTING);
end;

procedure TModelViewController.DrawModel;
begin
  if Assigned(FOnDrawScene) then
    FOnDrawScene(self);
//  DrawTeapot;
end;

//------------------------------------------------------------------------------
// Draw left window (view from the camera)
//------------------------------------------------------------------------------
procedure TModelViewController.DrawSub1;
begin
  // clear buffer (whole area)
  SetViewportSub(0, 0, FWindowWidth, FWindowHeight, 1, 10);
  glClearColor(FBackColor[0], FBackColor[1], FBackColor[2], FBackColor[3]);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

  // make left viewport square viewport
  if (FWindowHeight > FPovWidth) then
    SetViewportSub(0, (FWindowHeight - FPovWidth) div 2, FPovWidth, FPovWidth, 1, 10)
  else
    SetViewportSub((FPovWidth - FWindowHeight) div 2, 0, FWindowHeight, FWindowHeight, 1, 10);

  // clear buffer (square area)
  glClearColor(0.2, 0.2, 0.2, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

  glPushMatrix();

  // set view matrix ========================================================
  // copy the matrix to OpenGL GL_MODELVIEW matrix
  // See updateViewMatrix() how matrixView is constructed. The equivalent
  // OpenGL calls are;
  //    glLoadIdentity();
  //    glRotatef(-cameraAngle[2], 0, 0, 1); // roll (Z)
  //    glRotatef(-cameraAngle[1], 0, 1, 0); // heading (Y)
  //    glRotatef(-cameraAngle[0], 1, 0, 0); // pitch (X)
  //    glTranslatef(-cameraPosition[0], -cameraPosition[1], -cameraPosition[2]);
  glLoadMatrixf(FMatrixView.Get);

  // Always draw the grid at the origin (before any modeling transform)
  DrawGrid(10, 1);

  // Fransform objects ======================================================
  // From now, all transform will be for modeling matrix only.
  // (from object space to world space)
  // See updateModelMatrix() how matrixModel is constructed. The equivalent
  // OpenGL calls are;
  //    glLoadIdentity();
  //    glTranslatef(modelPosition[0], modelPosition[1], modelPosition[2]);
  //    glRotatef(modelAngle[0], 1, 0, 0);
  //    glRotatef(modelAngle[1], 0, 1, 0);
  //    glRotatef(modelAngle[2], 0, 0, 1);

  // compute GL_MODELVIEW matrix by multiplying matrixView and matrixModel
  // before drawing the object:
  // ModelView_M = View_M * Model_M
  // This modelview matrix transforms the objects from object space to eye space.
  glLoadMatrixf(FMatrixModelView.Get());

  // draw a teapot and axis after ModelView transform
  // v' = Mmv * v
  DrawAxis(4);
  DrawModel;

  glPopMatrix();
end;

//------------------------------------------------------------------------------
//  Right window (3rd person view)
//------------------------------------------------------------------------------
procedure TModelViewController.DrawSub2;
var
  matView, matModel, matModelView: ToglMatrix4f;
begin
  // Set right viewport
  SetViewportSub(FPovWidth, 0, FWindowWidth-FPovWidth, FWindowHeight, NEAR_PLANE, FAR_PLANE);

  // it is done in drawSub1(), no need to clear buffer
  //glClearColor(bgColor[0], bgColor[1], bgColor[2], bgColor[3]);   // background color
  //glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

  glPushMatrix();

  // First, transform the camera (viewing matrix) from world space to eye space
  matView.identity();
  matView.RotateY(FCameraAngleY);
  matView.RotateX(FCameraAngleX);
  matView.Translate(0, 0, -FCameraDistance);
  glLoadMatrixf(matView.Get);
  // Equivalent OpenGL calls:
  //glTranslatef(0, 0, -FCameraDistance);
  //glRotatef(FCameraAngleX, 1, 0, 0); // pitch
  //glRotatef(FCameraAngleY, 0, 1, 0); // heading

  // Draw grid
  DrawGrid(10, 1);

  // Transform teapot
  matModel.Identity;
  matModel.RotateZ(FModelAngle[2]);
  matModel.RotateY(FModelAngle[1]);
  matModel.RotateX(FModelAngle[0]);
  matModel.Translate(FModelPosition[0], FModelPosition[1], FModelPosition[2]);
  matModelView := matView * matModel;
  glLoadMatrixf(matModelView.Get);
  // Equivalent OpenGL calls:
  //glTranslatef(modelPosition[0], modelPosition[1], modelPosition[2]);
  //glRotatef(modelAngle[0], 1, 0, 0);
  //glRotatef(modelAngle[1], 0, 1, 0);
  //glRotatef(modelAngle[2], 0, 0, 1);

  // Draw a teapot and axis
  DrawAxis(4);
  DrawModel;

  // Draw camera axis
  matModel.Identity();
  matModel.RotateY(180);  // facing to the -Z axis
  matModel.RotateZ(-FCameraAngle[2]);
  matModel.RotateY(FCameraAngle[1]);
  matModel.RotateX(-FCameraAngle[0]);
  matModel.Translate(FCameraPosition[0], FCameraPosition[1], FCameraPosition[2]);
  matModelView := matView * matModel;
  glLoadMatrixf(matModelView.get());
  DrawAxis(1.0);  //0.8);

  // Transform camera object
  matModel.Identity();
  matModel.RotateZ(-FCameraAngle[2]);
  matModel.RotateY(FCameraAngle[1]);
  matModel.RotateX(-FCameraAngle[0]);
  matModel.Translate(FCameraPosition[0], FCameraPosition[1], FCameraPosition[2]);
  matModelView := matView * matModel;
  glLoadMatrixf(matModelView.Get);
  // Equivalent OpenGL calls:
  //glTranslatef(cameraPosition[0], cameraPosition[1], cameraPosition[2]);
  //glRotatef(-cameraAngle[0], 1, 0, 0);
  //glRotatef(cameraAngle[1], 0, 1, 0);
  //glRotatef(-cameraAngle[2], 0, 0, 1);

  // Draw the camera
  DrawCamera();
  DrawFrustum(FOV_Y, 1, 1, 10);

  glPopMatrix();
end;

//------------------------------------------------------------------------------
// Initialize OpenGL states and scene
//------------------------------------------------------------------------------
procedure TModelViewController.InitGL;
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

  InitLights;
end;

//------------------------------------------------------------------------------
// Initialize lights
//------------------------------------------------------------------------------
procedure TModelViewController.InitLights;
const
  lightKa: ToglArray4f = (0.3, 0.3, 0.3, 1.0);    // ambient light
  lightKd: ToglArray4f = (0.8, 0.8, 0.8, 1.0);    // diffuse light
  lightKs: ToglArray4f = (1.0, 1.0, 1.0, 1.0);    // specular light
  lightPos: ToglArray4f = (0.0, 1.0, 1.0, 0.0);   // directional light
begin
  // Set up light colors (ambient, diffuse, specular)
  glLightfv(GL_LIGHT0, GL_AMBIENT, @lightKa);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @lightKd);
  glLightfv(GL_LIGHT0, GL_SPECULAR, @lightKs);

  // Position the light in eye space
  glLightfv(GL_LIGHT0, GL_POSITION, @lightPos);

  glEnable(GL_LIGHT0);                            // MUST enable each light source after configuration
end;

procedure TModelViewController.ResetModelMatrix;
begin
  SetModelMatrix(0,0,0, 0,0,0);
end;

procedure TModelViewController.ResetViewMatrix;
begin
  SetViewMatrix(0,0,10, 0,0,0);
end;

//------------------------------------------------------------------------------
// Rotate the camera for subWin2 (3rd person view)
//------------------------------------------------------------------------------
procedure TModelViewController.RotateCamera(x, y: Integer);
begin
  FCameraAngleY := FCameraAngleY + (x - FMouseX);
  FCameraAngleX := FCameraAngleX + (y - FMouseY);
  FMouseX := x;
  FMouseY := y;
end;

//------------------------------------------------------------------------------
// Set camera position and lookat direction
//------------------------------------------------------------------------------
procedure TModelViewController.SetCamera(PosX, PosY, PosZ, TargetX, TargetY, TargetZ: GLfloat);
var
  lForward: ToglVector4f;
  lUp: ToglVector4f;
  lLeft: ToglVector4f;
  lPosition: ToglVector4f;
  invLength: GLfloat;
begin
  // determine forward vector (direction reversed because it is camera)
  lForward[0] := PosX - TargetX;    // x
  lForward[1] := PosY - TargetY;    // y
  lForward[2] := PosZ - TargetZ;    // z
  lForward[3] := 0.0;               // w

  // normalize it without w-component
  invLength := 1.0 / sqrt(sqr(lForward[0])+ sqr(lForward[1]) + sqr(lForward[2]));
  lForward[0] := lForward[0] * invLength;
  lForward[1] := lForward[1] * invLength;
  lForward[2] := lForward[2] * invLength;

  // assume up direction is straight up
  lUp := Vector4f(0.0, 1.0, 0.0, 0.0);
  //              x    y    z    w

  // compute left vector with cross product
  lLeft[0] := lUp[1]*lForward[2] - lUp[2]*lForward[1];  // x
  lLeft[1] := lUp[2]*lForward[0] - lUp[0]*lForward[2];  // y
  lLeft[2] := lUp[0]*lForward[1] - lUp[1]*lForward[0];  // z
  lLeft[3] := 1.0;                                      // w

  // re-compute orthogonal up vector
  lUp[0] := lForward[1]*lLeft[2] - lForward[2]*lLeft[1];    // x
  lUp[1] := lForward[2]*lLeft[0] - lForward[0]*lLeft[2];    // y
  lUp[2] := lForward[0]*lLeft[1] - lForward[1]*lLeft[0];    // z
  lUp[3] := 0.0;                                            // w

  // camera position
  lPosition := Vector4f(-posX, -posY, -posZ, 1.0);

  // copy axis vectors to matrix
  FMatrixView.Identity();
  FMatrixView.SetColumn(0, lLeft);
  FMatrixView.SetColumn(1, lUp);
  FMatrixView.SetColumn(2, lForward);
  FMatrixView.SetColumn(3, lPosition);
end;

procedure TModelViewController.SetCameraAngleX(AngleX: GLfloat);
begin
  FCameraAngle.X := AngleX;
  UpdateViewMatrix;
end;

procedure TModelViewController.SetCameraAngleY(AngleY: GLfloat);
begin
  FCameraAngle.Y := AngleY;
  UpdateViewMatrix;
end;

procedure TModelViewController.SetCameraAngleZ(AngleZ: GLfloat);
begin
  FCameraAngle.Z := AngleZ;
  UpdateViewMatrix;
end;

procedure TModelViewController.SetCameraX(PosX: GLfloat);
begin
  FCameraPosition.X := PosX;
  UpdateViewMatrix;
end;

procedure TModelViewController.SetCameraY(PosY: GLfloat);
begin
  FCameraPosition.Y := PosY;
  UpdateViewMatrix;
end;

procedure TModelViewController.SetCameraZ(PosZ: GLfloat);
begin
  FCameraPosition.Z := PosZ;
  UpdateViewMatrix;
end;

//------------------------------------------------------------------------------
// Change drawing mode
//------------------------------------------------------------------------------
procedure TModelViewController.SetDrawMode(AMode: Integer);
begin
  if FDrawMode <> AMode then
  begin
    FDrawModeChanged := true;
    FDrawMode := AMode;
  end;
end;

//------------------------------------------------------------------------------
// Set a perspective frustum with 6 params similar to glFrustum()
// (left, right, bottom, top, near, far)
// Note: this is for row-major notation. OpenGL needs transpose it
//------------------------------------------------------------------------------
function TModelViewController.SetFrustum(L, R, B, T, N, F: GLfloat): ToglMatrix4f;
begin
  Result.Identity;
  Result[0]  :=  2 * n / (r - l);
  Result[5]  :=  2 * n / (t - b);
  Result[8]  :=  (r + l) / (r - l);
  Result[9]  :=  (t + b) / (t - b);
  Result[10] := -(f + n) / (f - n);
  Result[11] := -1;
  Result[14] := -(2 * f * n) / (f - n);
  Result[15] :=  0;
end;

//------------------------------------------------------------------------------
// Set a symmetric perspective frustum with 4 params similar to gluPerspective
// (vertical field of view, aspect ratio, near, far)
//------------------------------------------------------------------------------
function TModelViewController.SetFrustum(AFieldOfView, AspectRatio, AFront, ABack: GLfloat): ToglMatrix4f;
var
  tangent, h, w: GLfloat;
begin
  tangent := tan(AFieldOfView/2 * DEG2RAD);   // tangent of half fovY
  h := AFront * tangent;                      // half height of near plane
  w := h * AspectRatio;                       // half width of near plane

  // params: left, right, bottom, top, near, far
  Result := SetFrustum(-w, w, -h, h, AFront, ABack);
end;

//------------------------------------------------------------------------------
// Set a orthographic frustum with 6 params similar to glOrtho()
// (left, right, bottom, top, near, far)
// Note: this is for row-major notation. OpenGL needs transpose it
//------------------------------------------------------------------------------
function TModelViewController.SetOrthoFrustum(L, R, B, T, N, F: GLfloat): ToglMatrix4f;
begin
  Result.Identity;
  Result[0]  :=  2 / (R - L);
  Result[5]  :=  2 / (T - B);
  Result[10] := -2 / (F - N);
  Result[12] := -(R + L) / (R - L);
  Result[13] := -(T + B) / (T - B);
  Result[14] := -(F + N) / (F - N);
end;

procedure TModelViewController.SetModelAngleX(AngleX: GLfloat);
begin
  FModelAngle.X := AngleX;
  UpdateModelMatrix;
end;

procedure TModelViewController.SetModelAngleY(AngleY: GLfloat);
begin
  FModelAngle.Y := AngleY;
  UpdateModelMatrix;
end;

procedure TModelViewController.SetModelAngleZ(AngleZ: GLfloat);
begin
  FModelAngle.Z := AngleZ;
  UpdateModelMatrix;
end;

//------------------------------------------------------------------------------
// Set the object position and rotation
//------------------------------------------------------------------------------
procedure TModelViewController.SetModelMatrix(x, y, z, rx, ry, rz: GLfloat);
begin
  FModelPosition[0] := x;
  FModelPosition[1] := y;
  FModelPosition[2] := z;
  FModelAngle[0] := rx;
  FModelAngle[1] := ry;
  FModelAngle[2] := rz;

  UpdateModelMatrix();
end;

procedure TModelViewController.SetModelX(PosX: GLfloat);
begin
  FModelPosition.X := PosX;
  UpdateModelMatrix;
end;

procedure TModelViewController.SetModelY(PosY: GLfloat);
begin
  FModelPosition.Y := PosY;
  UpdateModelMatrix;
end;

procedure TModelViewController.SetModelZ(PosZ: GLfloat);
begin
  FModelPosition.Z := PosZ;
  UpdateModelMatrix;
end;

procedure TModelViewController.SetMousePosition(X, Y: Integer);
begin
  FMouseX := X;
  FMouseY := Y;
end;

//------------------------------------------------------------------------------
// Set the camera position and rotation
//------------------------------------------------------------------------------
procedure TModelViewController.SetViewMatrix(x, y, z, pitch, heading, roll: GLfloat);
begin
  FCameraPosition[0] := x;
  FCameraPosition[1] := y;
  FCameraPosition[2] := z;
  FCameraAngle[0] := pitch;
  FCameraAngle[1] := heading;
  FCameraAngle[2] := roll;
  UpdateViewMatrix();
end;

//------------------------------------------------------------------------------
// Configure projection and viewport
//------------------------------------------------------------------------------
procedure TModelViewController.SetViewport(x, y, w, h: Integer);
var
  lMatrix: ToglMatrix4f;
begin
  // Set viewport to be the entire window
  glViewport(GLsizei(x), GLsizei(y), GLsizei(w), GLsizei(h));

  // Set perspective viewing frustum
  lMatrix := SetFrustum(FOV_Y, w/h, NEAR_PLANE, FAR_PLANE); // FOV, AspectRatio, NearClip, FarClip

  // Copy projection matrix to OpenGL
  glMatrixMode(GL_PROJECTION);
  glLoadMatrixf(lMatrix.Get());
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
end;

//------------------------------------------------------------------------------
// Configure projection and viewport of sub window
//------------------------------------------------------------------------------
procedure TModelViewController.SetViewportSub(x, y, AWidth, AHeight: Integer;
  ANearPlane, AFarPlane: GLfloat);
var
  lMatrix: ToglMatrix4f;
begin
  // Set viewport
  glViewport(x, y, AWidth, AHeight);
  glScissor(x, y, AWidth, AHeight);

  // Set perspective viewing frustum
  lMatrix := SetFrustum(FOV_Y, AWidth/AHeight, ANearPlane, AFarPlane); // FOV, AspectRatio, NearClip, FarClip

  // Copy projection matrix to OpenGL
  glMatrixMode(GL_PROJECTION);
  glLoadMatrixf(lMatrix.Get);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
end;

//------------------------------------------------------------------------------
// Set rendering window size
//------------------------------------------------------------------------------
procedure TModelViewController.SetWindowSize(AWidth, AHeight: Integer);
begin
  // assign the width/height of viewport
  FWindowWidth := AWidth;
  FWindowHeight := AHeight;

  // compute dim for point of view screen
  FPovWidth := FWindowWidth div 2;
  // if it is wider than height, reduce to the height (make it square)
  if (FPovWidth > FWindowHeight) then
    FPovWidth := FWindowHeight;

  FWindowSizeChanged := true;
end;

//------------------------------------------------------------------------------
// Update model matrix
//------------------------------------------------------------------------------
procedure TModelViewController.UpdateModelMatrix;
begin
  // Transform objects from object space to world space
  // ORDER: rotZ -> rotY -> rotX -> translation
  FMatrixModel.Identity();
  FMatrixModel.RotateZ(FModelAngle[2]);
  FMatrixModel.RotateY(FModelAngle[1]);
  FMatrixModel.RotateX(FModelAngle[0]);
  FMatrixModel.Translate(FModelPosition[0], FModelPosition[1], FModelPosition[2]);

  FMatrixModelView := FMatrixView * FMatrixModel;
end;

//------------------------------------------------------------------------------
// Update view matrix
//------------------------------------------------------------------------------
procedure TModelViewController.UpdateViewMatrix;
begin
  // Transform the camera (viewing matrix) from world space to eye space
  // Notice translation nd heading values are negated,
  // because we move the whole scene with the inverse of camera transform
  // ORDER: translation -> rotX -> rotY ->rotZ
  FMatrixView.Identity();
  FMatrixView.Translate(-FCameraPosition[0], -FCameraPosition[1], -FCameraPosition[2]);
  FMatrixView.RotateX(FCameraAngle[0]);     // pitch
  FMatrixView.RotateY(-FCameraAngle[1]);    // heading
  FMatrixView.RotateZ(FCameraAngle[2]);     // roll

  FMatrixModelView := FMatrixView * FMatrixModel;
end;

//------------------------------------------------------------------------------
// Zoom the camera for subWin2 (3rd person view)
//------------------------------------------------------------------------------
procedure TModelViewController.ZoomCamera(y: Integer);
begin
  FCameraDistance := FCameraDistance - (Y - FMouseY) * 0.1;
  FMouseY := Y;
end;

procedure TModelViewController.ZoomCameraDelta(Delta: GLfloat);
begin
  FCameraDistance := FCameraDistance - Delta;
end;

end.

