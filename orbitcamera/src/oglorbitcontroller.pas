unit oglOrbitController;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  gl, glu, oglTypes, oglMath, oglOrbitCamera;

type
  ToglOrbitController = class
  private
    // members
    FBackColor: ToglVector4f;

    FWindowWidth: Integer;
    FWindowHeight: Integer;
    FWindowSizeChanged: Boolean;
    FMouseLeftDown: Boolean;
    FMouseRightDown: Boolean;
    FMouseX: Integer;
    FMouseY: Integer;
    FNearPlane: GLfloat;
    FFarPlane: GLfloat;
    FFoV: GLfloat;

    // Grid
    FGridEnabled: Boolean;
    FGridSize: GLfloat;         // half length of grid
    FGridStep: GLfloat;         // step for next grid line

    // obj
    //ObjModel objModel;
    //ObjModel objCam;
    //bool objLoaded;

    // Cameras
    FCam1: TOrbitCamera;       // for view1
    FCam2: TOrbitCamera;       // for view2
    FCameraAngle: ToglVector3f;
    FCameraPosition: ToglVector3f;
    FCameraTarget: ToglVector3f;
    FCameraQuaternion: ToglQuaternion;
    FCameraMatrix: ToglMatrix4f;

    // Projection matrix
    FMatrixProjection: ToglMatrix4f;

    // Material
    FDefaultAmbient: ToglVector4f;
    FDefaultDiffuse: ToglVector4f;
    FDefaultSpecular: ToglVector4f;
    FDefaultShininess: GLfloat;
    FCamAmbient: ToglVector4f;
    FCamDiffuse: ToglVector4f;
    FCamSpecular: ToglVector4f;
    FCamShininess: GLfloat;

    // Events for drawing the camera and the object
    FOnDrawCamera: TNotifyEvent;
    FOnDrawScene: TNotifyEvent;

    // Vertices for FoV
    FFovVertices: array[0..4] of ToglVector3f;
    FFovNormals: array[0..3] of ToglVector3f;
    FFovEnabled: Boolean;
    procedure ComputeFovVertices(AFov: GLfloat);
    procedure Draw2D(AScreenID: Integer);
    procedure DrawFocalLine;
    procedure DrawFocalPoint;
    procedure DrawFOV;
    procedure DrawGridXZ(ASize, AStep: GLfloat);  // Draw a grid on XZ plane
    procedure DrawGridXY(ASize, AStep: GLfloat);  // Draw a grid on XY plane
    procedure PreFrame;
    procedure PostFrame;
    procedure SetCameraAngleX(x: GLfloat);
    procedure SetCameraAngleY(y: GLfloat);
    procedure SetCameraAngleZ(z: GLfloat);
    procedure SetCameraPositionX(x: GLfloat);
    procedure SetCameraPositionY(y: GLfloat);
    procedure SetCameraPositionZ(z: GLfloat);
    procedure SetCameraTargetX(x: GLfloat);
    procedure SetCameraTargetY(y: GLfloat);
    procedure SetCameraTargetZ(z: GLfloat);
    procedure SetFrustum(L, R, B, T, N, F: GLfloat);
    procedure SetFrustum(FOVY, AspectRatio, Front, Back: GLfloat);
    procedure SetFov(FieldOfView: GLfloat);
    procedure SetGridSize(ASize: GLfloat);
    procedure SetOrthoFrustum(L, R, B, T: GLfloat;  N: GLfloat = -1; F: GLfloat = -1);
    procedure SetViewport(x, y, AWidth, AHeight: Integer);

  protected
    procedure DrawCamera; virtual;      // Draw camera in world space
    procedure DrawObj; virtual;         // Draw some object for demonstration
    procedure InitLights;               // Add a white light to the scene

  public
    constructor Create;
    procedure Draw(AScreenID: Integer);

    procedure InitGL;

    procedure ResetCamera;
    procedure RotateCamera(X, Y: Integer);
    procedure SetMouseLeft(ADown: Boolean);
    procedure SetMouseRight(ADown: Boolean);
    procedure SetMousePosition(X, Y: Integer);
    procedure SetWindowSize(AWidth, AHeight: Integer);
    procedure ZoomCamera(Y: Integer);
    procedure ZoomCameraDelta(Delta: GLfloat);

    property CameraAngle: ToglVector3f read FCameraAngle write FCameraAngle;
    property CameraAngleX: GLfloat read FCameraAngle.X write SetCameraAngleX;
    property CameraAngleY: GLfloat read FCameraAngle.Y write SetCameraAngleY;
    property CameraAngleZ: GLfloat read FCameraAngle.Z write SetCameraAngleZ;

    property CameraPosition: ToglVector3f read FCameraPosition write FCameraPosition;
    property CameraPositionX: GLfloat read FCameraPosition.X write SetCameraPositionX;
    property CameraPositionY: GLfloat read FCameraPosition.Y write SetCameraPositionY;
    property CameraPositionZ: GLfloat read FCameraPosition.Z write SetCameraPositionZ;

    property CameraTarget: ToglVector3f read FCameraTarget write FCameraTarget;
    property CameraTargetX: GLfloat read FCameraTarget.X write SetCameraTargetX;
    property CameraTargetY: GLfloat read FCameraTarget.Y write SetCameraTargetY;
    property CameraTargetZ: GLfloat read FCameraTarget.Z write SetCameraTargetZ;

    property CameraMatrix: ToglMatrix4f read FCameraMatrix;
    property CameraQuaternion: ToglQuaternion read FCameraQuaternion;

    property FOVEnabled: Boolean read FFOVEnabled write FFOVEnabled;
    property GridEnabled: Boolean read FGridEnabled write FGridEnabled;
    property GridSize: GLfloat read FGridSize write SetGridSize;

    property OnDrawCamera: TNotifyEvent read FOnDrawCamera write FOnDrawCamera;
    property OnDrawScene: TNotifyEvent read FOnDrawScene write FOnDrawScene;
  end;

implementation

const
  GRID_SIZE = 10.0;
  GRID_STEP = 1.0;
  CAM_DIST = 5.0;
  DEG2RAD = pi / 180.0;
  FOV_Y = 50.0;
  NEAR_PLANE = 0.1;
  FAR_PLANE = 1000.0;
  MAX_LOG_LENGTH = 4096;
  OBJ_SCALE = 0.01;
  OBJ_MODEL = 'data/debugger_small_5k.obj';
  OBJ_CAM = 'data/camera.obj';
  FONT_FILE = 'data/walkway32_bold.fnt';

constructor ToglOrbitController.Create;
var
  camPosition, camTarget: ToglVector3f;
begin
  inherited Create;

  FBackColor := Vector4f(0,0,0,1);
  FNearPlane := NEAR_PLANE;
  FFarPlane := FAR_PLANE;
  FFov := FOV_Y;
  FFovEnabled := true;
  FGridEnabled := true;
  FGridSize := GRID_SIZE;
  FGridStep := GRID_STEP;

  camPosition := Vector3f(CAM_DIST*2, CAM_DIST*1.5, CAM_DIST*2);
  camTarget := Vector3f(0, 0, 0);

  FCam1 := TOrbitCamera.Create;
  FCam1.LookAt(camPosition, camTarget);

  FCam2 := TOrbitCamera.Create;
  FCam2.LookAt(Vector3f(0, 0, CAM_DIST), Vector3f(0, 0, 0));

  FCameraAngle := FCam2.Angle;
  FCameraPosition := FCam2.Position;
  FCameraTarget := FCam2.Target;
  FCameraQuaternion := FCam2.Quaternion;
  FCameraMatrix := FCam2.Matrix;

  // Init default material
  FDefaultAmbient := Vector4f(0.8, 0.6, 0.2, 1.0);
  FDefaultDiffuse := Vector4f(1.0, 0.9, 0.2, 1.0);
  FDefaultSpecular := Vector4f(1.0, 1.0, 1.0, 1.0);
  FDefaultShininess := 128.0;

  // Init camera material
  FCamAmbient := Vector4f(0.0, 0.0, 0.0, 1.0);
  FCamDiffuse := Vector4f(0.9, 0.9, 0.9, 1.0);
  FCamSpecular := Vector4f(1.0, 1.0, 1.0, 1.0);
  FCamShininess := 256.0;

  // Init FOV vertices
  ComputeFovVertices(FFoV);
end;

//------------------------------------------------------------------------------
// Compute vertices for FOV
//------------------------------------------------------------------------------
procedure ToglOrbitController.ComputeFovVertices(AFov: GLfloat);
const
  DIST = 11.0;
var
  halfFOV: GLfloat;
  ratio: GLfloat;
  tan_halfFov: GLfloat;
  tan_halfFov_ratio: GLfloat;
begin
  halfFoV := AFov * 0.5 * DEG2RAD;
  ratio := 1.0;
  tan_halfFov := tan(halfFOV);
  tan_halfFov_ratio := tan(halfFOV*ratio);

  // Compute 5 vertices of the fov
  // origin
  FFovVertices[0].x := 0;
  FFovVertices[0].y := 0;
  FFovVertices[0].z := 0;

  // top-left
  FFovVertices[1].x := tan_halfFov_ratio * DIST;
  FFovVertices[1].y := tan_halfFov * DIST;
  FFovVertices[1].z := DIST;

  // top-right
  FFovVertices[2].x := FFovVertices[2].x - tan_halfFov_ratio * DIST;
  FFovVertices[2].y := tan_halfFov * DIST;
  FFovVertices[2].z := DIST;

  // bottom-left
  FFovVertices[3].x := tan_halfFov_ratio * DIST;
  FFovVertices[3].y := FFovVertices[3].y - tan_halfFov * DIST;
  FFovVertices[3].z := DIST;

  // bottom-right
  FFovVertices[4].x := FFovVertices[4].x - tan_halfFov_ratio * DIST;
  FFovVertices[4].y := FFovVertices[4].y - tan_halfFov * DIST;
  FFovVertices[4].z := DIST;

  // compute normals
  // top
  FFovNormals[0] := CrossProduct(FFovVertices[2] - FFovVertices[0], FFovVertices[1] - FFovVertices[0]);
  FFovNormals[0].Normalize();

  // bottom
  FFovNormals[1] := CrossProduct(FFovVertices[3] - FFovVertices[0], FFovVertices[4] - FFovVertices[0]);
  FFovNormals[1].Normalize();

  // left
  FFovNormals[2] := CrossProduct(FFovVertices[1] - FFovVertices[0], FFovVertices[3] - FFovVertices[0]);
  FFovNormals[2].Normalize();

  // right
  FFovNormals[3] := CrossProduct(FFovVertices[4] - FFovVertices[0], FFovVertices[2] - FFovVertices[0]);
  FFovNormals[3].Normalize();
end;

//------------------------------------------------------------------------------
// Draw 2D/3D scene
//------------------------------------------------------------------------------
procedure ToglOrbitController.Draw(AScreenId: Integer);
var
  matView: ToglMatrix4f;
  matModel: ToglMatrix4f;
  matModelView: ToglMatrix4f;
begin
  PreFrame;

  // clear buffer
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
  //glEnable(GL_BLEND);

  if (AScreenId = 1) then
  begin
    // Set projection matrix to OpenGL
    SetFrustum(FOV_Y, FWindowWidth/FWindowHeight, FNearPlane, FFarPlane);
    glMatrixMode(GL_PROJECTION);
    glLoadMatrixf(FMatrixProjection.Get());
    glMatrixMode(GL_MODELVIEW);

    // From 3rd person camera
    matView := FCam1.Matrix;
    glLoadMatrixf(matView.Get());

    // draw grid
    if FGridEnabled then
      DrawGridXZ(FGridSize, FGridStep);

    // Draw line from camera to focal point
    DrawFocalLine();
    DrawFocalPoint();

    // Matrix for camera model
    matModel.Identity;
    matModel.Translate(FCameraPosition);
    matModel.LookAt(FCameraTarget, FCam2.GetUpAxis());
    matModelView := matView * matModel;

    // Draw obj models
    DrawObj();
    glLoadMatrixf(matModelView.Get());
    DrawCamera();
    if FFovEnabled then
      DrawFov();
  end
  else
  if (AScreenId = 2) then
  begin
    // Set projection matrix to OpenGL
    SetFrustum(FFov, FWindowWidth/FWindowHeight, FNearPlane, FFarPlane);
    glMatrixMode(GL_PROJECTION);
    glLoadMatrixf(FMatrixProjection.Get());
    glMatrixMode(GL_MODELVIEW);

    // From camera object
    glLoadMatrixf(FCameraMatrix.Get());

    // Draw grid
    if FGridEnabled then
      DrawGridXZ(FGridSize, FGridStep);

    // Draw focal point
    DrawFocalPoint();

    // Draw OBJ model
    DrawObj();
  end;

  // Draw 2D
  Draw2D(AScreenId);

  PostFrame();
end;

//------------------------------------------------------------------------------
// Draw 2D
//------------------------------------------------------------------------------
procedure ToglOrbitController.Draw2D(AScreenID: Integer);
begin
  // Set orthogonal projection
  SetOrthoFrustum(0, FWindowWidth, 0, FWindowHeight, -1, 1);
  glMatrixMode(GL_PROJECTION);
  glLoadMatrixf(FMatrixProjection.Get());
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glEnable(GL_TEXTURE_2D);

  if (AScreenId = 1) then
  begin
    //font.drawText(5, (float)windowHeight-font.getHeight(), "3rd Person View");
  end
  else
  if (AScreenId = 2) then
  begin
    //font.drawText(5, (float)windowHeight-font.getHeight(), "Point of View");
  end;

  glDisable(GL_TEXTURE_2D);

  // restore prev settings
  glEnable(GL_BLEND);
  glEnable(GL_COLOR_MATERIAL);
end;

//------------------------------------------------------------------------------
// Draw camera
//------------------------------------------------------------------------------
procedure ToglOrbitController.DrawCamera;
begin
  if Assigned(FOnDrawCamera) then
    FOnDrawCamera(Self);
end;

//------------------------------------------------------------------------------
// Draw a line from camera to focal point
//------------------------------------------------------------------------------
procedure ToglOrbitController.DrawFocalLine;
begin
  // disable lighting
  glDisable(GL_LIGHTING);
  glDepthFunc(GL_ALWAYS);     // to avoid visual artifacts with grid lines
  glLineWidth(1.0);

  glColor4f(1.0, 1.0, 0.2, 0.7);
  glBegin(GL_LINES);
    glVertex3fv(@FCameraPosition);
    glVertex3fv(@FCameraTarget);
  glEnd();

  // Enable lighting back
  glLineWidth(1.0);
  glDepthFunc(GL_LEQUAL);
  glEnable(GL_LIGHTING);
end;

//------------------------------------------------------------------------------
// Draw a line from camera to focal point
//------------------------------------------------------------------------------
procedure ToglOrbitController.DrawFocalPoint;
begin
  // disable lighting
  glDisable(GL_LIGHTING);
  glDepthFunc(GL_ALWAYS);     // to avoid visual artifacts with grid lines
  glPointSize(5.0);

  glColor4f(1.0, 1.0, 0.2, 0.7);
  glBegin(GL_POINTS);
    glVertex3fv(@FCameraTarget);
  glEnd();

  // Enable lighting back
  glPointSize(1.0);
  glDepthFunc(GL_LEQUAL);
  glEnable(GL_LIGHTING);
end;

//------------------------------------------------------------------------------
// Draw Field-Of-View
//------------------------------------------------------------------------------
procedure ToglOrbitController.DrawFOV;
begin
  // Draw backface first
  glCullFace(GL_FRONT);
  glLightModelf(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);

  glBegin(GL_TRIANGLES);
    // top
    glNormal3fv(@FFovNormals[0]);
    glColor4f(0.5, 0.5, 0.5, 0.5);
    glVertex3fv(@FFovVertices[0]);
    glColor4f(0.5, 0.5, 0.5, 0.0);
    glVertex3fv(@FFovVertices[2]);
    glVertex3fv(@FFovVertices[1]);
    // bottom
    glNormal3fv(@FFovNormals[1]);
    glColor4f(0.5, 0.5, 0.5, 0.5);
    glVertex3fv(@FFovVertices[0]);
    glColor4f(0.5, 0.5, 0.5, 0.0);
    glVertex3fv(@FFovVertices[3]);
    glVertex3fv(@FFovVertices[4]);
    // left
    glNormal3fv(@FFovNormals[2]);
    glColor4f(0.5, 0.5, 0.5, 0.5);
    glVertex3fv(@FFovVertices[0]);
    glColor4f(0.5, 0.5, 0.5, 0.0);
    glVertex3fv(@FFovVertices[1]);
    glVertex3fv(@FFovVertices[3]);
    // right
    glNormal3fv(@FFovNormals[3]);
    glColor4f(0.5, 0.5, 0.5, 0.5);
    glVertex3fv(@FFovVertices[0]);
    glColor4f(0.5, 0.5, 0.5, 0.0);
    glVertex3fv(@FFovVertices[4]);
    glVertex3fv(@FFovVertices[2]);
  glEnd();

  // Draw frontface second
  glCullFace(GL_BACK);
  glLightModelf(GL_LIGHT_MODEL_TWO_SIDE, GL_FALSE);

  glBegin(GL_TRIANGLES);
    // top
    glNormal3fv(@FFovNormals[0]);
    glColor4f(0.5, 0.5, 0.5, 0.5);
    glVertex3fv(@FFovVertices[0]);
    glColor4f(0.5, 0.5, 0.5, 0.0);
    glVertex3fv(@FFovVertices[2]);
    glVertex3fv(@FFovVertices[1]);
    // bottom
    glNormal3fv(@FFovNormals[1]);
    glColor4f(0.5, 0.5, 0.5, 0.5);
    glVertex3fv(@FFovVertices[0]);
    glColor4f(0.5, 0.5, 0.5, 0.0);
    glVertex3fv(@FFovVertices[3]);
    glVertex3fv(@FFovVertices[4]);
    // left
    glNormal3fv(@FFovNormals[2]);
    glColor4f(0.5, 0.5, 0.5, 0.5);
    glVertex3fv(@FFovVertices[0]);
    glColor4f(0.5, 0.5, 0.5, 0.0);
    glVertex3fv(@FFovVertices[1]);
    glVertex3fv(@FFovVertices[3]);
    // right
    glNormal3fv(@FFovNormals[3]);
    glColor4f(0.5, 0.5, 0.5, 0.5);
    glVertex3fv(@FFovVertices[0]);
    glColor4f(0.5, 0.5, 0.5, 0.0);
    glVertex3fv(@FFovVertices[4]);
    glVertex3fv(@FFovVertices[2]);
  glEnd();

  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);
  glLineWidth(0.5);

  glBegin(GL_LINES);
    glColor4f(0.5, 0.5, 0.5, 0.8);
    glVertex3fv(@FFovVertices[0]);
    glColor4f(0.5, 0.5, 0.5, 0.0);
    glVertex3fv(@FFovVertices[1]);
    glColor4f(0.5, 0.5, 0.5, 0.8);
    glVertex3fv(@FFovVertices[0]);
    glColor4f(0.5, 0.5, 0.5, 0.0);
    glVertex3fv(@FFovVertices[2]);
    glColor4f(0.5, 0.5, 0.5, 0.8);
    glVertex3fv(@FFovVertices[0]);
    glColor4f(0.5, 0.5, 0.5, 0.0);
    glVertex3fv(@FFovVertices[3]);
    glColor4f(0.5, 0.5, 0.5, 0.8);
    glVertex3fv(@FFovVertices[0]);
    glColor4f(0.5, 0.5, 0.5, 0.0);
    glVertex3fv(@FFovVertices[4]);
  glEnd();

  glLineWidth(1.0);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
end;

//------------------------------------------------------------------------------
// Draw a grid on the xz plane
//------------------------------------------------------------------------------
procedure ToglOrbitController.DrawGridXZ(ASize, AStep: GLfloat);
var
  step: GLfloat;
begin
  // disable lighting
  glDisable(GL_LIGHTING);
  //glDisable(GL_DEPTH_TEST);
  glLineWidth(0.5);

  glBegin(GL_LINES);

    glColor4f(0.5, 0.5, 0.5, 0.5);
    step := AStep;
    while (step <= ASize) do
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
    glColor4f(1, 0, 0, 0.5);
    glColor3f(1, 0, 0);
    glVertex3f(-ASize, 0, 0);
    glVertex3f( ASize, 0, 0);

    // z-axis
    glColor4f(0, 0, 1, 0.5);
    glVertex3f(0, 0, -ASize);
    glVertex3f(0, 0,  ASize);

  glEnd();

  // Enable lighting back
  glLineWidth(1.0);
  //glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
end;

//------------------------------------------------------------------------------
// Draw a grid on the xy plane
//------------------------------------------------------------------------------
procedure ToglOrbitController.DrawGridXY(ASize, AStep: GLfloat);
var
  step: GLfloat;
begin
  glDisable(GL_LIGHTING);
  //glDisable(GL_DEPTH_TEST);
  glLineWidth(0.5);

  glBegin(GL_LINES);

    glColor4f(0.5, 0.5, 0.5, 0.5);
    step := AStep;
    while step <= ASize do
    begin
      glVertex3f(-ASize,  step, 0);   // lines parallel to X-axis
      glVertex3f( ASize,  step, 0);
      glVertex3f(-ASize, -step, 0);   // lines parallel to X-axis
      glVertex3f( ASize, -step, 0);

      glVertex3f( step, -ASize, 0);   // lines parallel to Y-axis
      glVertex3f( step,  ASize, 0);
      glVertex3f(-step, -ASize, 0);   // lines parallel to Y-axis
      glVertex3f(-step,  ASize, 0);

      step := step + AStep;
    end;

    // x-axis
    glColor4f(1.0, 0, 0, 0.5);
    glVertex3f(-ASize, 0, 0);
    glVertex3f( ASize, 0, 0);

    // y-axis
    glColor4f(0, 0, 1.0, 0.5);
    glVertex3f(0, -ASize, 0);
    glVertex3f(0,  ASize, 0);

  glEnd();

  glLineWidth(1.0);
  //glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
end;

//------------------------------------------------------------------------------
// Draw obj model
//------------------------------------------------------------------------------
procedure ToglOrbitController.DrawObj;
begin
  if Assigned(FOnDrawScene) then
    FOnDrawScene(Self);
end;

//------------------------------------------------------------------------------
// Initialize OpenGL states and scene
//------------------------------------------------------------------------------
procedure ToglOrbitController.InitGL;
begin
  glShadeModel(GL_SMOOTH);               // shading mathod: GL_SMOOTH or GL_FLAT
  glPixelStorei(GL_UNPACK_ALIGNMENT, 4); // 4-byte pixel alignment

  // enable /disable features
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  //glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  //glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
  //glEnable(GL_TEXTURE_2D);
  glEnable(GL_CULL_FACE);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  // track material ambient and diffuse from surface color, call it before glEnable(GL_COLOR_MATERIAL)
  glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_COLOR_MATERIAL);

  glClearColor(FBackColor[0], FBackColor[1], FBackColor[2], FBackColor[3]);   // background color
  glClearStencil(0);                              // clear stencil buffer
  glClearDepth(1.0);                             // 0 is near, 1 is far
  glDepthFunc(GL_LEQUAL);

  InitLights();
  //InitFont();
end;

//------------------------------------------------------------------------------
// Initialize lights
//------------------------------------------------------------------------------
procedure ToglOrbitController.InitLights;
var
  lightKa, lightKd, lightKs, lightPos: ToglVector4f;
begin
  // Setup light colors (ambient, diffuse, specular)
  lightKa := Vector4f(0.2, 0.2, 0.2, 1.0);      // ambient light
  lightKd := Vector4f(0.8, 0.8, 0.8, 1.0);      // diffuse light
  lightKs := Vector4f(1.0, 1.0, 1.0, 1.0);      // specular light
  glLightfv(GL_LIGHT0, GL_AMBIENT, @lightKa);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @lightKd);
  glLightfv(GL_LIGHT0, GL_SPECULAR, @lightKs);

  // Position the light in eye space
  lightPos := Vector4f(0, 0, 1, 0);               // directional light
  glLightfv(GL_LIGHT0, GL_POSITION, @lightPos);

  glEnable(GL_LIGHT0);                            // MUST enable each light source after configuration
end;

//------------------------------------------------------------------------------
// Pre-frame
//------------------------------------------------------------------------------
procedure ToglOrbitController.PreFrame;
begin
  if FWindowSizeChanged then
  begin
    SetViewport(0, 0, FWindowWidth, FWindowHeight);
    FWindowSizeChanged := false;
  end;
end;

//------------------------------------------------------------------------------
// Post-frame
//------------------------------------------------------------------------------
procedure ToglOrbitController.PostFrame;
begin
  // Nothing to do here...
end;

//------------------------------------------------------------------------------
// Reset both cameras
//------------------------------------------------------------------------------
procedure ToglOrbitController.ResetCamera;
begin
  // 3rd person camera
  FCam1.LookAt(Vector3f(CAM_DIST*2, CAM_DIST*1.5, CAM_DIST*2), Vector3f(0, 0, 0));

  // Camera object
  FCam2.LookAt(Vector3f(0, 0, CAM_DIST), Vector3f(0, 0, 0));
  FCameraAngle := FCam2.Angle;
  FCameraPosition := FCam2.Position;
  FCameraTarget := FCam2.Target;
  FCameraQuaternion := FCam2.Quaternion;
  FCameraMatrix := FCam2.Matrix;

  // fov
  FFoV := FOV_Y;
  ComputeFovVertices(FFoV);
  SetFrustum(FFoV, FWindowWidth/FWindowHeight, FNearPlane, FFarPlane);
end;

//------------------------------------------------------------------------------
// Rotate the 3rd person camera
//------------------------------------------------------------------------------
procedure ToglOrbitController.RotateCamera(X, Y: Integer);
const
  ANGLE_SCALE = 0.2;
var
  angle: ToglVector3f;
begin
  angle := FCam1.Angle;
  angle.y := angle.y - (x - FMouseX) * ANGLE_SCALE;
  angle.x := angle.x + (y - FMouseY) * ANGLE_SCALE;

  FMouseX := x;
  FMouseY := y;

  // Constrain x angle -89 < x < 89
  if (angle.x < -89.0) then
    angle.x := -89.0
  else if (angle.x > 89.0) then
    angle.x := 89.0;

  FCam1.RotateTo(angle);
end;

//------------------------------------------------------------------------------
// Set camera parameters
//------------------------------------------------------------------------------
procedure ToglOrbitController.SetCameraAngleX(x: GLfloat);
begin
  FCameraAngle.x := x;
  FCam2.Angle := FCameraAngle;
  FCameraPosition := FCam2.Position;
  FCameraQuaternion := FCam2.Quaternion;
  FCameraMatrix := FCam2.Matrix;
end;

procedure ToglOrbitController.SetCameraAngleY(y: GLfloat);
begin
  FCameraAngle.y := y;
  FCam2.Angle := FCameraAngle;
  FCameraPosition := FCam2.Position;
  FCameraQuaternion := FCam2.Quaternion;
  FCameraMatrix := FCam2.Matrix;
end;

procedure ToglOrbitController.SetCameraAngleZ(z: GLfloat);
begin
  FCameraAngle.z := z;
  FCam2.Angle := FCameraAngle;
  FCameraPosition := FCam2.Position;
  FCameraQuaternion := FCam2.Quaternion;
  FCameraMatrix := FCam2.Matrix;
end;

procedure ToglOrbitController.SetCameraPositionX(x: GLfloat);
begin
  FCameraPosition.x := x;
  FCam2.Position := FCameraPosition;
  FCameraAngle := FCam2.Angle;
  FCameraQuaternion := FCam2.Quaternion;
  FCameraMatrix := FCam2.Matrix;
end;

procedure ToglOrbitController.SetCameraPositionY(y: GLfloat);
begin
  FCameraPosition.y := y;
  FCam2.Position := FCameraPosition;
  FCameraAngle := FCam2.Angle;
  FCameraQuaternion := FCam2.Quaternion;
  FCameraMatrix := FCam2.Matrix;
end;

procedure ToglOrbitController.SetCameraPositionZ(z: GLfloat);
begin
  FCameraPosition.z := z;
  FCam2.Position := FCameraPosition;
  FCameraAngle := FCam2.Angle;
  FCameraQuaternion := FCam2.Quaternion;
  FCameraMatrix := FCam2.Matrix;
end;

procedure ToglOrbitController.SetCameraTargetX(x: GLfloat);
begin
  FCameraTarget.x := x;
  FCam2.Target := FCameraTarget;
  FCameraPosition := FCam2.Position;
  FCameraMatrix := FCam2.Matrix;
end;

procedure ToglOrbitController.SetCameraTargetY(y: GLfloat);
begin
  FCameraTarget.y := y;
  FCam2.Target := FCameraTarget;
  FCameraPosition := FCam2.Position;
  FCameraMatrix := FCam2.Matrix;
end;

procedure ToglOrbitController.SetCameraTargetZ(z: GLfloat);
begin
  FCameraTarget.z := z;
  FCam2.Target := FCameraTarget;
  FCameraPosition := FCam2.Position;
  FCameraMatrix := FCam2.Matrix;
end;

//------------------------------------------------------------------------------
// Set a perspective frustum with 6 params similar to glFrustum()
// (left, right, bottom, top, near, far)
// Note: this is for row-major notation. OpenGL needs transpose it
//------------------------------------------------------------------------------
procedure ToglOrbitController.SetFrustum(L, R, B, T, N, F: GLfloat);
begin
  FMatrixProjection.identity();
  FMatrixProjection[0]  :=  2 * n / (r - l);
  FMatrixProjection[5]  :=  2 * n / (t - b);
  FMatrixProjection[8]  :=  (r + l) / (r - l);
  FMatrixProjection[9]  :=  (t + b) / (t - b);
  FMatrixProjection[10] := -(f + n) / (f - n);
  FMatrixProjection[11] := -1;
  FMatrixProjection[14] := -(2 * f * n) / (f - n);
  FMatrixProjection[15] :=  0;
end;

//------------------------------------------------------------------------------
// Set a symmetric perspective frustum with 4 params similar to gluPerspective
// (vertical field of view, aspect ratio, near, far)
//------------------------------------------------------------------------------
procedure ToglOrbitController.SetFrustum(FOVY, AspectRatio, Front, Back: GLfloat);
var
  tangent: GLfloat;
  h, w: GLfloat;
begin
  tangent := tan(FOVY/2 * DEG2RAD);    // tangent of half fovY
  h := Front * tangent;                // half height of near plane
  w := h * AspectRatio;                // half width of near plane

  // params: left, right, bottom, top, near, far
  SetFrustum(-w, w, -h, h, Front, Back);
end;

//------------------------------------------------------------------------------
// Compute grid size and step
//------------------------------------------------------------------------------
procedure ToglOrbitController.SetGridSize(ASize: GLfloat);
begin
  FGridSize := ASize;
  FGridStep := 1;
end;

//------------------------------------------------------------------------------
// Set an orthographic frustum with 6 params similar to glOrtho()
// (left, right, bottom, top, near, far)
// Note: this is for row-major notation. OpenGL needs transpose it
//------------------------------------------------------------------------------
procedure ToglOrbitController.SetOrthoFrustum(L, R, B, T: GLfloat; N: GLfloat; F: GLfloat);
begin
  FMatrixProjection.Identity();
  FMatrixProjection[0]  :=  2 / (R - L);
  FMatrixProjection[5]  :=  2 / (T - B);
  FMatrixProjection[10] := -2 / (F - N);
  FMatrixProjection[12] := -(R + L) / (R - L);
  FMatrixProjection[13] := -(F + B) / (T - B);
  FMatrixProjection[14] := -(F + N) / (F - N);
end;

//------------------------------------------------------------------------------
// Set vertical FOV
//------------------------------------------------------------------------------
procedure ToglOrbitController.SetFov(FieldOfView: GLfloat);
begin
  FFoV := FieldOfView;
  ComputeFovVertices(FFoV);
  SetFrustum(FFoV, FWindowWidth/FWindowHeight, FNearPlane, FFarPlane);
end;

procedure ToglOrbitController.SetMouseLeft(ADown: Boolean);
begin
  FMouseLeftDown := ADown;
end;

procedure ToglOrbitController.SetMouseRight(ADown: Boolean);
begin
  FMouseRightDown := ADown;
end;

procedure ToglOrbitController.SetMousePosition(X, Y: Integer);
begin
  FMouseX := X;
  FMouseY := Y;
end;

//------------------------------------------------------------------------------
// Configure projection and viewport
//------------------------------------------------------------------------------
procedure ToglOrbitController.SetViewport(x, y, AWidth, AHeight: Integer);
begin
  // Set viewport to be the entire window
  glViewport(GLsizei(x), GLsizei(y), GLsizei(AWidth), GLsizei(AHeight));

  // set perspective viewing frustum
  SetFrustum(FFov, AWidth/AHeight, FNearPlane, FFarPlane); // FOV, AspectRatio, NearClip, FarClip
end;

//------------------------------------------------------------------------------
// Set rendering window size
//------------------------------------------------------------------------------
procedure ToglOrbitController.SetWindowSize(AWidth, AHeight: Integer);
begin
  // Assign the width/height of viewport
  FWindowWidth := AWidth;
  FWindowHeight := AHeight;
  FWindowSizeChanged := true;
end;

//------------------------------------------------------------------------------
// Move the 3rd person camera forward
//------------------------------------------------------------------------------
procedure ToglOrbitController.ZoomCamera(Y: Integer);
var
  delta: GLfloat;
begin
  delta := Y - FMouseY;
  ZoomCameraDelta(delta);
end;

procedure ToglOrbitController.ZoomCameraDelta(Delta: GLfloat);
const
  ZOOM_SCALE = 0.5;
  MIN_DIST   = 1.0;
  MAX_DIST   = 30.0;
var
  distance: GLfloat;
begin
  distance := FCam1.Distance;
  distance := distance - (Delta * ZOOM_SCALE);

  // constrain min and max
  if (distance < MIN_DIST) then
    distance := MIN_DIST
  else if (distance > MAX_DIST) then
    distance := MAX_DIST;

  FCam1.Distance := distance;
end;


end.

