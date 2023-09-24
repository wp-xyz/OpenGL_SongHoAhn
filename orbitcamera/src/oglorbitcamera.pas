///////////////////////////////////////////////////////////////////////////////
// Original file header by Son Ho Ahn:
//-----------------------------------------------------------------------------
// OrbitCamera.h
// =============
// Orbital camera class for OpenGL
// Use lookAt() for initial positioning the camera, then call rotateTo() for
// orbital rotation, moveTo()/moveForward() to move camera position only and
// shiftTo() to move position and target together (panning)
//
// Dependencies: Vector2, Vector3, Matrix4, Quaternion, animUtils.h
//
//  AUTHOR: Song Ho Ahn (song.ahn@gmail.com)
// CREATED: 2011-12-02
// UPDATED: 2016-10-24
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// Lazarus port by wp:
// - removed animation
// - dependencies: oglTypes, oglMath
///////////////////////////////////////////////////////////////////////////////

unit oglOrbitCamera;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  gl, glu, oglTypes, oglMath;

type

  { TOrbitCamera }

  TOrbitCamera = class
  private
    // member variables
    FPosition: ToglVector3f;         // camera position at world space
    FTarget: ToglVector3f;           // camera focal (lookat) position at world space
    FDistance: GLfloat;              // distance between position and target
    FAngle: ToglVector3f;            // angle in degrees around the target (pitch, heading, roll)
    FMatrix: ToglMatrix4f;           // 4x4 matrix combined rotation and translation
    FMatrixRotation: ToglMatrix4f;   // rotation only
    FQuaternion: ToglQuaternion;     // quaternion for rotations
    FQuaternionUsed: Boolean;        // flag to use quaternion

    function AngleToMatrix(const aAngle: ToglVector3f): ToglMatrix4f;
    procedure SetDistance(d: GLfloat);
    procedure SetPosition(const V: ToglVector3f);
    procedure SetRotation(const aAngle: ToglVector3f);
    procedure SetTarget(const V: ToglVector3f);
  protected
    procedure ComputeMatrix();
    function MatrixToAngle(const AMatrix: ToglMatrix4f): ToglVector3f;

  public
    constructor Create;
    constructor Create(APosition, ATarget: ToglVector3f);

    // Set position, target and transform matrix so camera looks at the target
    procedure LookAt(const APosition, ATarget: ToglVector3f);
    procedure LookAt(const APosition, ATarget, AUpDir: ToglVector3f);
    procedure LookAt(Px, Py, Pz, Tx, Ty, Tz: GLfloat);
    procedure LookAt(Px, Py, Pz, Tx, Ty, Tz, Ux, Uy, Uz: GLfloat);

    // Move the camera position to the destination
    // If duration(sec) is greater than 0, it will animate for the given duration
    // otherwise, it will set the position immediately
    // Use moveForward() to move the camera forward/backward
    // NOTE: you must call update() before getting the delta movement per frame
    procedure MoveForward(Delta: GLfloat);
    procedure MoveTo(const ATo: ToglVector3f);

    // Rotate the camera around the target point
    // You can use either quaternion or Euler anagles
    procedure Rotate(const Delta: ToglVector3f);
    procedure RotateTo(const aAngle: ToglVector3f);
    procedure RotateTo(const Q: ToglQuaternion);

    // Pan the camera, shift both position and target point in same direction; left/right/up/down
    // Use this function to offset the camera's rotation pivot
    procedure Shift(const Delta: ToglVector2f);
    procedure ShiftTo(const ATo: ToglVector3f);

    // Return camera's 3 axis vectors
    function GetForwardAxis: ToglVector3f;
    function GetLeftAxis: ToglVector3f;
    function GetUpAxis: ToglVector3f;

    procedure SetRotationQ(const Q: ToglQuaternion);

    property Angle: ToglVector3f read FAngle write SetRotation;
    property Distance: GLfloat read FDistance write SetDistance;
    property Matrix: ToglMatrix4f read FMatrix write FMatrix;
    property Position: ToglVector3f read FPosition write SetPosition;
    property Quaternion: ToglQuaternion read FQuaternion write FQuaternion;
    property Target: ToglVector3f read FTarget write SetTarget;
  end;

implementation

const
  DEG2RAD = pi / 180.0;
  RAD2DEG = 180.0 / pi;
  EPSILON = 0.00001;

constructor TOrbitCamera.Create;
begin
  FQuaternion.Init(1, 0, 0, 0);
end;

constructor TOrbitCamera.Create(APosition, ATarget: ToglVector3f);
begin
  FQuaternion.Init(1, 0,0,0);
  LookAt(APosition, ATarget);
end;

//------------------------------------------------------------------------------
// Convert rotation angles (degree) to 4x4 matrix
// NOTE: the angle is for orbit camera, so yaw angle must be reversed before
// matrix computation.
//
// The order of rotation is Roll->Yaw->Pitch (Rx*Ry*Rz)
// Rx: rotation about X-axis, pitch
// Ry: rotation about Y-axis, yaw(heading)
// Rz: rotation about Z-axis, roll
//    Rx           Ry          Rz
// |1  0   0| | Cy  0 Sy| |Cz -Sz 0|   | CyCz        -CySz         Sy  |
// |0 Cx -Sx|*|  0  1  0|*|Sz  Cz 0| = | SxSyCz+CxSz -SxSySz+CxCz -SxCy|
// |0 Sx  Cx| |-Sy  0 Cy| | 0   0 1|   |-CxSyCz+SxSz  CxSySz+SxCz  CxCy|
//------------------------------------------------------------------------------
function TOrbitCamera.AngleToMatrix(const aAngle: ToglVector3f): ToglMatrix4f;
var
  sx, sy, sz, cx, cy, cz, theta: GLfloat;
  lLeft, lUp, lForward: ToglVector3f;
begin
  // Rotation angle about X-axis (pitch)
  theta := aAngle.x * DEG2RAD;
  SinCos(theta, sx, cx);

  // Rotation angle about Y-axis (yaw)
  theta := -aAngle.y * DEG2RAD;
  SinCos(theta, sy, cy);

  // Rotation angle about Z-axis (roll)
  theta := aAngle.z * DEG2RAD;
  SinCos(theta, sz, cz);

  // Determine left axis
  lLeft.x := cy*cz;
  lLeft.y := sx*sy*cz + cx*sz;
  lLeft.z := -cx*sy*cz + sx*sz;

  // Determine up axis
  lUp.x := -cy*sz;
  lUp.y := -sx*sy*sz + cx*cz;
  lUp.z := cx*sy*sz + sx*cz;

  // Determine forward axis
  lForward.x := sy;
  lForward.y := -sx*cy;
  lForward.z := cx*cy;

  // Construct rotation matrix
  Result.Identity;
  Result.SetColumn(0, lLeft);
  Result.SetColumn(1, lUp);
  Result.SetColumn(2, lForward);
end;

//------------------------------------------------------------------------------
// construct camera matrix: M = Mt2 * Mr * Mt1
// where Mt1: move scene to target (-x,-y,-z)
//       Mr : rotate scene at the target point
//       Mt2: move scene away from target with distance -d
//
//     Mt2             Mr               Mt1
// |1  0  0  0|   |r0  r4  r8  0|   |1  0  0 -x|   |r0  r4  r8  r0*-x + r4*-y + r8*-z     |
// |0  1  0  0| * |r1  r5  r9  0| * |0  1  0 -y| = |r1  r5  r9  r1*-x + r5*-y + r9*-z     |
// |0  0  1 -d|   |r2  r6  r10 0|   |0  0  1 -z|   |r2  r6  r10 r2*-x + r6*-y + r10*-z - d|
// |0  0  0  1|   |0   0   0   1|   |0  0  0  1|   |0   0   0   1                         |
//------------------------------------------------------------------------------
procedure TOrbitCamera.ComputeMatrix();
var
  lLeft, lUp, lForward: ToglVector3f;
  lTrans: ToglVector3f;
begin
  // Extract left/up/forward vectors from rotation matrix
  lLeft := Vector3f(FMatrixRotation[0], FMatrixRotation[1], FMatrixRotation[2]);
  lUp := Vector3f(FMatrixRotation[4], FMatrixRotation[5], FMatrixRotation[6]);
  lForward := Vector3f(FMatrixRotation[8], FMatrixRotation[9], FMatrixRotation[10]);

  // Compute translation vector
  lTrans.x := -lLeft.x * FTarget.x - lUp.x * FTarget.y - lForward.x * FTarget.z;
  lTrans.y := -lLeft.y * FTarget.x - lUp.y * FTarget.y - lForward.y * FTarget.z;
  lTrans.z := -lLeft.z * FTarget.x - lUp.z * FTarget.y - lForward.z * FTarget.z - FDistance;

  // Construct matrix
  FMatrix.Identity();
  FMatrix.SetColumn(0, lLeft);
  FMatrix.SetColumn(1, lUp);
  FMatrix.SetColumn(2, lForward);
  FMatrix.SetColumn(3, lTrans);

  // re-compute camera position
  lForward.Init(-FMatrix[2], -FMatrix[6], -FMatrix[10]);
  FPosition := FTarget - (FDistance * lForward);

  (*@@
  //DEBUG: equivalent to the above matrix computation
  matrix.identity();
  matrix.translate(-target.x, -target.y, -target.z); // Mt1: move scene to target point
  matrix = matrixRotation * matrix;                  // Mr : rotate scene at the target point
  matrix.translate(0, 0, -distance);                 // Mt2: move scene away from the target with distance

  // re-compute camera position
  // NOTE: camera's forward vector is the forward vector of inverse matrix
  Vector3 forward(-matrix[2], -matrix[6], -matrix[10]);
  position = target - (distance * forward);
  *)
end;

//------------------------------------------------------------------------------
// Return forward axis
//------------------------------------------------------------------------------
function TOrbitCamera.GetForwardAxis: ToglVector3f;
begin
  Result := Vector3f(-FMatrix[2], -FMatrix[6], -FMatrix[10]);
end;

//------------------------------------------------------------------------------
// Return left axis
//------------------------------------------------------------------------------
function TOrbitCamera.GetLeftAxis: ToglVector3f;
begin
  Result := Vector3f(-FMatrix[0], -FMatrix[4], -FMatrix[8]);
end;

//------------------------------------------------------------------------------
// Return up axis
//------------------------------------------------------------------------------
function TOrbitCamera.GetUpAxis: ToglVector3f;
begin
  Result := Vector3f(FMatrix[1], FMatrix[5], FMatrix[9]);
end;

//------------------------------------------------------------------------------
// Set transform matrix equivalent to gluLookAt()
// 1. Mt: Translate scene to camera position inversely, (-x, -y, -z)
// 2. Mr: Rotate scene inversly so camera looks at the scene
// 3. Find matrix = Mr * Mt
//       Mr               Mt
// |r0  r4  r8  0|   |1  0  0 -x|   |r0  r4  r8  r0*-x + r4*-y + r8 *-z|
// |r1  r5  r9  0| * |0  1  0 -y| = |r1  r5  r9  r1*-x + r5*-y + r9 *-z|
// |r2  r6  r10 0|   |0  0  1 -z|   |r2  r6  r10 r2*-x + r6*-y + r10*-z|
// |0   0   0   1|   |0  0  0  1|   |0   0   0   1                     |
//------------------------------------------------------------------------------
procedure TOrbitCamera.LookAt(const APosition, ATarget: ToglVector3f);
var
  lLeft, lUp, lForward: ToglVector3f;  // 3 axes of matrix for scene
  lTrans: ToglVector3f;                // translation part
  reversedAngle: ToglVector3f;
begin
  // Remember the camera posision & target position
  FPosition := APosition;
  FTarget := ATarget;

  // If pos and target are same, only translate camera to position without rotation
  if (APosition = ATarget) then
  begin
    FMatrix.Identity();
    FMatrix.SetColumn(3, -FPosition);
    // rotation stuff
    FMatrixRotation.Identity();
    FAngle.Init(0,0,0);
    FQuaternion.Init(1,0,0,0);
    exit;
  end;

  // At first, compute the forward vector of rotation matrix
  // NOTE: the direction is reversed (target to camera pos) because of camera transform
  lForward := FPosition - FTarget;
  FDistance := lForward.Length();  // remember the distance
  // Normalize
  lForward[0] := lForward[0]/FDistance;
  lForward[1] := lForward[1]/FDistance;
  lForward[2] := lForward[2]/FDistance;

  // Compute temporary up vector based on the forward vector
  // Watch out when look up/down at 90 degree
  // For example, forward vector is on the Y axis
  if (abs(lForward.X) < EPSILON) and (abs(lForward.z) < EPSILON) then
  begin
    // forward vector is pointing +Y axis
    if (lForward.y > 0) then
      lUp := Vector3f(0, 0, -1)
    else
      // forward vector is pointing -Y axis
      lUp := Vector3f(0, 0, 1);
  end
  else
    // in general, up vector is straight up
    lUp := Vector3f(0, 1, 0);

  // Compute the left vector of rotation matrix
  lLeft := CrossProduct(lUp, lForward);
  lLeft.Normalize();

  // Recalculate the orthonormal up vector
  lUp := CrossProduct(lForward, lLeft);

  // Set inverse rotation matrix: M^-1 = M^T for Euclidean transform
  FMatrixRotation.Identity();
  FMatrixRotation.SetRow(0, lLeft);
  FMatrixRotation.SetRow(1, lUp);
  FMatrixRotation.SetRow(2, lForward);

  // Copy it to matrix
  FMatrix.Identity();
  FMatrix.SetRow(0, lLeft);
  FMatrix.SetRow(1, lUp);
  FMatrix.SetRow(2, lForward);

  // set translation part
  lTrans.x := -FMatrix[0]*FPosition.x - FMatrix[4]*FPosition.y - FMatrix[8]*FPosition.z;
  lTrans.y := -FMatrix[1]*FPosition.x - FMatrix[5]*FPosition.y - FMatrix[9]*FPosition.z;
  lTrans.z := -FMatrix[2]*FPosition.x - FMatrix[6]*FPosition.y - FMatrix[10]*FPosition.z;
  FMatrix.SetColumn(3, lTrans);

  // Set Euler angles
  FAngle := MatrixToAngle(FMatrixRotation);

  // Set quaternion from angle
  reversedAngle.Init(FAngle.x, -FAngle.y, FAngle.z);
  FQuaternion := GetQuaternion(reversedAngle * (DEG2RAD * 0.5)); // half angle
end;

//------------------------------------------------------------------------------
// Set transform matrix with target and camera's up vectors
//------------------------------------------------------------------------------
procedure TOrbitCamera.LookAt(const APosition, ATarget, AUpDir: ToglVector3f);
var
  lLeft, lUp, lForward: ToglVector3f;  // 3 axes of matrix for scene
  lTrans: ToglVector3f;                // translation part
  reversedAngle: ToglVector3f;
begin
  // Remember the camera posision & target position
  FPosition := APosition;
  FTarget := ATarget;

  // If pos and target are same, only translate camera to position without rotation
  if (FPosition = FTarget) then
  begin
    FMatrix.Identity();
    FMatrix.Translate(-FPosition.x, -FPosition.y, -FPosition.z);
    // Rotation stuff
    FMatrixRotation.Identity();
    FAngle.Init(0,0,0);
    FQuaternion.Init(1,0,0,0);
    exit;
  end;

  // Compute the forward vector
  // NOTE: the direction is reversed (target to camera pos) because of camera transform
  lForward := FPosition - FTarget;
  FDistance := lForward.Length();  // remember the distance
  // Normalize
  lForward.X := lForward.X/FDistance;
  lForward.Y := lForward.Y/FDistance;
  lForward.Z := lForward.Z/FDistance;

  // Compute the left vector
  lLeft := CrossProduct(AUpDir, lForward);
  lLeft.Normalize();

  // Recompute the orthonormal up vector
  lUp := CrossProduct(lForward, lLeft);
  // lUp.normalize();   // not needed since lForward and lLeft already are normalized

  // Set inverse rotation matrix: M^-1 = M^T for Euclidean transform
  FMatrixRotation.Identity();
  FMatrixRotation.SetRow(0, lLeft);
  FMatrixRotation.SetRow(1, lUp);
  FMatrixRotation.SetRow(2, lForward);

  // Copy it to matrix
  FMatrix.Identity();
  FMatrix.SetRow(0, lLeft);
  FMatrix.SetRow(1, lUp);
  FMatrix.SetRow(2, lForward);

  // Set translation
  lTrans.x := -FMatrix[0]*FPosition.x - FMatrix[4]*FPosition.y - FMatrix[8]*FPosition.z;
  lTrans.y := -FMatrix[1]*FPosition.x - FMatrix[5]*FPosition.y - FMatrix[9]*FPosition.z;
  lTrans.z := -FMatrix[2]*FPosition.x - FMatrix[6]*FPosition.y - FMatrix[10]*FPosition.z;
  FMatrix.SetColumn(3, lTrans);

  // Set Euler angles
  FAngle := MatrixToAngle(FMatrixRotation);

  // Set quaternion from angle
  reversedAngle.Init(FAngle.x, -FAngle.y, FAngle.z);
  FQuaternion := GetQuaternion(reversedAngle * (DEG2RAD * 0.5)); // half angle
end;

procedure TOrbitCamera.LookAt(Px, Py, Pz, Tx, Ty, Tz: GLfloat);
begin
  LookAt(Vector3f(Px,Py,Pz), Vector3f(Tx,Ty,Tz));
end;

procedure TOrbitCamera.LookAt(Px, Py, Pz, Tx, Ty, Tz, Ux, Uy, Uz: GLfloat);
begin
  LookAt(Vector3f(Px,Py,Pz), Vector3f(Tx,Ty,Tz), Vector3f(Ux,Uy,Uz));
end;

//------------------------------------------------------------------------------
// retrieve angles in degree from rotation matrix, M = Rx*Ry*Rz
// Rx: rotation about X-axis, pitch
// Ry: rotation about Y-axis, yaw(heading)
// Rz: rotation about Z-axis, roll
//    Rx           Ry          Rz
// |1  0   0| | Cy  0 Sy| |Cz -Sz 0|   | CyCz        -CySz         Sy  |
// |0 Cx -Sx|*|  0  1  0|*|Sz  Cz 0| = | SxSyCz+CxSz -SxSySz+CxCz -SxCy|
// |0 Sx  Cx| |-Sy  0 Cy| | 0   0 1|   |-CxSyCz+SxSz  CxSySz+SxCz  CxCy|
//
// Pitch: atan(-m[7] / m[8]) = atan(SxCy/CxCy)
// Yaw  : asin(m[6])         = asin(Sy)
// Roll : atan(-m[3] / m[0]) = atan(SzCy/CzCy)
//------------------------------------------------------------------------------
function TOrbitCamera.MatrixToAngle(const AMatrix: ToglMatrix4f): ToglVector3f;
begin
  Result := AMatrix.GetAngle();

  // reverse yaw
  Result.y := -Result.y;
  Result.z := Result.z;   // wp: ???
end;

//------------------------------------------------------------------------------
// Zoom in/out the camera position with the given delta movement and duration
// It actually moves the camera forward or backward.
// Positive delta means moving forward (decreasing distance)
//------------------------------------------------------------------------------
procedure TOrbitCamera.MoveForward(Delta: GLfloat);
begin
  SetDistance(FDistance - Delta);
end;

//------------------------------------------------------------------------------
// Move the camera position with the given duration
//------------------------------------------------------------------------------
procedure TOrbitCamera.MoveTo(const ATo: ToglVector3f);
begin
  SetPosition(ATo);
end;

//------------------------------------------------------------------------------
// Rotate camera with delta angle
// NOTE: delta angle must be negated already
//------------------------------------------------------------------------------
procedure TOrbitCamera.Rotate(const Delta: ToglVector3f);
begin
  RotateTo(FAngle + Delta); //, duration, mode);
end;

//------------------------------------------------------------------------------
// Rotate camera to the given angle with duration
//------------------------------------------------------------------------------
procedure TOrbitCamera.RotateTo(const aAngle: ToglVector3f);
begin
  FQuaternionUsed := false;
  SetRotation(aAngle);
end;

//------------------------------------------------------------------------------
// Rotate camera to the given quaternion with duration
//------------------------------------------------------------------------------
procedure TOrbitCamera.RotateTo(const Q: ToglQuaternion);
begin
  FQuaternionUsed := true;
  SetRotationQ(Q);
end;

//------------------------------------------------------------------------------
// Set distance of camera, then recompute camera position
//------------------------------------------------------------------------------
procedure TOrbitCamera.SetDistance(d: GLfloat);
begin
  FDistance := d;
  ComputeMatrix();
end;

//------------------------------------------------------------------------------
// Set position of camera, set transform matrix as well
//------------------------------------------------------------------------------
procedure TOrbitCamera.SetPosition(const V: ToglVector3f);
begin
  LookAt(V, FTarget);
end;

//------------------------------------------------------------------------------
// Set transform matrix with rotation angles (degree)
// NOTE: the angle is for camera, so yaw value must be negated for computation.
//
// The order of rotation is Roll->Yaw->Pitch (Rx*Ry*Rz)
// Rx: rotation about X-axis, pitch
// Ry: rotation about Y-axis, yaw(heading)
// Rz: rotation about Z-axis, roll
//    Rx           Ry          Rz
// |1  0   0| | Cy  0 Sy| |Cz -Sz 0|   | CyCz        -CySz         Sy  |
// |0 Cx -Sx|*|  0  1  0|*|Sz  Cz 0| = | SxSyCz+CxSz -SxSySz+CxCz -SxCy|
// |0 Sx  Cx| |-Sy  0 Cy| | 0   0 1|   |-CxSyCz+SxSz  CxSySz+SxCz  CxCy|
///////////////////////////////////////////////////////////////////////////////
procedure TOrbitCamera.SetRotation(const aAngle: ToglVector3f);
var
  reversedAngle: ToglVector3f;
begin
  // Remember angles
  // NOTE: assume all angles are already reversed for camera
  FAngle := aAngle;

  //  quaternion value
  // NOTE: yaw must be negated again for quaternion
  reversedAngle := Vector3f(aAngle.x, -aAngle.y, aAngle.z);
  FQuaternion := GetQuaternion(reversedAngle);

  // Compute rotation matrix from angle
  FMatrixRotation := AngleToMatrix(aAngle);

  // Construct camera matrix
  ComputeMatrix();
end;

//------------------------------------------------------------------------------
// Set rotation with new quaternion
// NOTE: quaternion value is for matrix, so MatrixToAngle() will reverse yaw.
//------------------------------------------------------------------------------
procedure TOrbitCamera.SetRotationQ(const Q: ToglQuaternion);
begin
  // Remember the current quaternion
  FQuaternion := Q;

  // FQuaternion to matrix
  FMatrixRotation := Q.GetMatrix();

  // Construct matrix
  ComputeMatrix();

  // Compute angle from matrix
  FAngle := MatrixToAngle(FMatrixRotation);
end;

//------------------------------------------------------------------------------
// Set target of camera, then rebuild matrix
// Rotation parts are not changed, but translation part must be recalculated
// And, position is also shifted
//------------------------------------------------------------------------------
procedure TOrbitCamera.SetTarget(const V: ToglVector3f);
var
  forward: ToglVector3f;
begin
  FTarget := V;

  // forward vector of camera
  forward := Vector3f(-FMatrix[2], -FMatrix[6], -FMatrix[10]);
  FPosition := FTarget - FDistance * forward;
  ComputeMatrix();
end;

//------------------------------------------------------------------------------
// Shift the camera position and target left/right/up/down
//------------------------------------------------------------------------------
procedure TOrbitCamera.Shift(const Delta: ToglVector2f);
var
  cameraLeft, cameraUp: ToglVector3f;
  deltaMovement: ToglVector3f;
  newTarget: ToglVector3f;
begin
  // Get left & up vectors of camera
  cameraLeft := Vector3f(-FMatrix[0], -FMatrix[4], -FMatrix[8]);
  cameraUp := Vector3f(-FMatrix[1], -FMatrix[5], -FMatrix[9]);

  // Compute delta movement
  deltaMovement := Delta.x * cameraLeft;
  deltaMovement := deltaMovement + (-Delta.y) * cameraUp;   // reverse up direction

  //  new target position
  newTarget := FTarget + deltaMovement;

  ShiftTo(newTarget);
end;

//------------------------------------------------------------------------------
// Pan the camera target left/right/up/down with the given duration
// The camera position will be shifted after transform
//------------------------------------------------------------------------------
procedure TOrbitCamera.ShiftTo(const ATo: ToglVector3f);
begin
  SetTarget(ATo);
end;

end.

