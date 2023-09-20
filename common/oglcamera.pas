///////////////////////////////////////////////////////////////////////////////
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
///////////////////////////////////////////////////////////////////////////////

unit oglCamera;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, gl,
  oglTypes, oglMath;

type
  ToglCamera = class
  private
    FPosition: ToglVector3f;       // Camera position at world space
    FTarget: ToglVector3f;         // Camera focus (lookat) at world space
    FDistance: GLfloat;            // Distance between camera and target
    FAngle: ToglVector3f;          // Angles (in degrees) around the target (pitch, heading, roll)
    FMatrix: ToglMatrix4f;         // 4x4 matrix combining rotation and translation
    FMatrixRotation: ToglMatrix4f; // 4x4 rotation matrix only
    FQuaternion: ToglQuaternion;   // quaternion for rotations
    FQuaternionUsed: Boolean;      // flag to use quaternion
    {
    function GetAngle: ToglVector3f;
    function GetDistance: GLfloat;
    function GetPosition: ToglVector3f;
    function GetQuaternion: ToglQuaternion;
    function GetTarge: ToglVector3f;
    function GetMatrix: ToglMatrix4f;
    }
    procedure SetPosition(const AValue: ToglVector3f);
    procedure SetTarget(const Avalue: ToglVector3f);
    procedure SetDistance(AValue: GLfloat);
    procedure SetRotation(AValue: ToglVector3f);  // angles in degrees
    procedure SetRotationQuat(AValue: ToglQuaternion);

  protected
    function AngleToMatrix(const Angles: ToglVector3f): ToglMatrix4f;
    procedure ComputeMatrix;
    function MatrixToAngle(const AMatrix: ToglMatrix4f): ToglVector3f;

  public
    constructor Create(APosition, ATarget: ToglVector3f);

    // Set position, target and transform matrix so camera looks at the target
    procedure LookAt(const APosition, ATarget: ToglVector3f);
    procedure LookAt(const APosition, ATarget, AUpDir: ToglVector3f);
    procedure LookAt(PX, PY, PZ, TX, TY, TZ: GLfloat);
    procedure LookAt(PX, PY, PZ, TX, TY, TZ, UX, UY, UZ: GLfloat);

    // Move the camera position to the destination
    // If duration(sec) is greater than 0, it will animate for the given duration
    // otherwise, it will set the position immediately
    // Use moveForward() to move the camera forward/backward
    // NOTE: You must call update() before getting the delta movement per frame
    procedure MoveTo(const NewPos: ToglVector3f); // float duration=0, Gil::AnimationMode mode=Gil::EASE_OUT);
    procedure MoveForward(const Delta: GLfloat); //float duration=0, Gil::AnimationMode mode=Gil::EASE_OUT);
    {
    void startForward(float maxSpeed=1.0f, float accel=1.0f);
    void stopForward();
    }

    // Pan the camera, shift both position and target point in same direction; left/right/up/down
    // Use this function to offset the camera's rotation pivot
    procedure ShiftTo(const NewPos: ToglVector3f);  //float duration=0, Gil::AnimationMode mode=Gil::EASE_OUT);
    procedure Shift(const Delta: ToglVector2f);  //float duration=0, Gil::AnimationMode mode=Gil::EASE_OUT);
    {
    void startShift(const Vector2& shiftVector, float accel=1.0f);
    void stopShift();
    }

    // Rotate the camera around the target point
    // You can use either quaternion or Euler angles
    procedure RotateTo(const Angle: ToglVector3f);   //float duration=0.0f, Gil::AnimationMode mode=Gil::EASE_OUT);
    procedure RotateTo(const Quat: ToglQuaternion);  //float duration=0.0f, Gil::AnimationMode mode=Gil::EASE_OUT);
    procedure Rotate(const DeltaAngle: ToglVector3f); //float duration=0.0f, Gil::AnimationMode mode=Gil::EASE_OUT);

    // Return camera's 3 axis vectors
    function GetLeftAxis: ToglVector3f;
    function GetUpAxis: ToglVector3f;
    function GetForwardAxis: ToglVector3f;

    property Angle: ToglVector3f read FAngle; // write FAngle;
    property Distance: GLfloat read FDistance write SetDistance;
    property Matrix: ToglMatrix4f read FMatrix; // write SetMatrix;
    property Position: ToglVector3f read FPosition write SetPosition;
    property Quaternion: ToglQuaternion read FQuaternion; // write SetQuaternion;
    property Target: ToglVector3f read FTarget write SetTarget;

  end;

implementation

const
  DEG2RAD = pi / 180.0;
  RAD2DEG = 180.0 / pi;
  EPSILON = 0.00001;

constructor ToglCamera.Create(APosition, ATarget: ToglVector3f);
begin
  FQuaternion.Init(1, 0, 0, 0);
  LookAt(APosition, ATarget);
end;

//------------------------------------------------------------------------------
// Convert rotation angles (in degrees) to 4x4 matrix
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
function ToglCamera.AngleToMatrix(const Angles: ToglVector3f): ToglMatrix4f;
var
  sx, sy, sz, cx, cy, cz, theta: GLfloat;
  left, up, forward: ToglVector3f;
begin
  // Rotation angle about X-axis (pitch)
  theta := Angles.x * DEG2RAD;
  SinCos(theta, sx, cx);

  // Totation angle about Y-axis (yaw)
  theta := Angles.y * -DEG2RAD;
  SinCos(theta, sy, cy);

  // Rotation angle about Z-axis (roll)
  theta := Angles.z * DEG2RAD;
  SinCos(theta, sz, cz);

  // Determine left axis
  left := Vector3f(cy*cz, sx*sy*cz + cx*sz, -cx*sy*cz + sx*sz);

  // Determine up axis
  up := Vector3f(-cy*sz, -sx*sy*sz + cx*cz, cx*sy*sz + sx*cz);

  // Forward axis
  forward := Vector3f(sy, -sx*cy, cx*cy);

  // Construct rotation matrix
  Result.SetColumn(0, left);
  Result.SetColumn(1, up);
  Result.SetColumn(2, forward);
end;

//------------------------------------------------------------------------------
// Construct camera matrix: M = Mt2 * Mr * Mt1
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
procedure ToglCamera.ComputeMatrix;
var
  left, up, forward, trans: ToglVector3f;
begin
  // extract left/up/forward vectors from rotation matrix
  left := Vector3f(FMatrixRotation[0], FMatrixRotation[1], FMatrixRotation[2]);
  up := Vector3f(FMatrixRotation[4], FMatrixRotation[5], FMatrixRotation[6]);
  forward := Vector3f(FMatrixRotation[8], FMatrixRotation[9], FMatrixRotation[10]);

  // Compute translation vector
  trans.x := left.x * -FTarget.x + up.x * -FTarget.y + forward.x * -FTarget.z;
  trans.y := left.y * -FTarget.x + up.y * -FTarget.y + forward.y * -FTarget.z;
  trans.z := left.z * -FTarget.x + up.z * -FTarget.y + forward.z * -FTarget.z - FDistance;

  // Construct matrix
  FMatrix.Identity;
  FMatrix.SetColumn(0, left);
  FMatrix.SetColumn(1, up);
  FMatrix.SetColumn(2, forward);
  FMatrix.SetColumn(3, trans);

  // Recompute camera position
  forward.Init(-FMatrix[2], -FMatrix[6], -FMatrix[10]);
  FPosition := FTarget - (FDistance * forward);

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

///////////////////////////////////////////////////////////////////////////////
// return left, up, forward axis
///////////////////////////////////////////////////////////////////////////////
function ToglCamera.GetForwardAxis: ToglVector3f;
begin
  Result := Vector3f(-FMatrix[2], -FMatrix[6], -FMatrix[10]);
end;

function ToglCamera.GetLeftAxis: ToglVector3f;
begin
  Result := Vector3f(-FMatrix[0], -FMatrix[4], -FMatrix[8]);
end;

function ToglCamera.GetUpAxis: ToglVector3f;
begin
  Result := Vector3f(FMatrix[1], FMatrix[5], FMatrix[9]);
end;

//------------------------------------------------------------------------------
// Set transform matrix equivalent to gluLookAt()
// 1. Mt: Translate scene to camera position inversely, (-x, -y, -z)
// 2. Mr: Rotate scene inversely so that camera looks at the scene
// 3. Find matrix = Mr * Mt
//       Mr               Mt
// |r0  r4  r8  0|   |1  0  0 -x|   |r0  r4  r8  r0*-x + r4*-y + r8 *-z|
// |r1  r5  r9  0| * |0  1  0 -y| = |r1  r5  r9  r1*-x + r5*-y + r9 *-z|
// |r2  r6  r10 0|   |0  0  1 -z|   |r2  r6  r10 r2*-x + r6*-y + r10*-z|
// |0   0   0   1|   |0  0  0  1|   |0   0   0   1                     |
///////////////////////////////////////////////////////////////////////////////
procedure ToglCamera.LookAt(const APosition, ATarget: ToglVector3f);
var
  left, up, forward: ToglVector3f;  // 3 axes of matrix for scene
  trans: ToglVector3f;  // translation vector
  reversedAngle: ToglVector3f;
begin
  // Remeber the camera and target positions
  FPosition := APosition;
  FTarget := ATarget;

  // If pos and target are same, only translate camera to position without rotation
  if (FPosition = FTarget) then
  begin
    FMatrix.Identity;
    FMatrix.SetColumn(3, -FPosition);
    // Rotation stuff
    FMatrixRotation.Identity;
    FAngle.Init(0,0,0);
    FQuaternion.Init(1,0,0,0);
    exit;
  end;

  // First, compute the forward vector of rotation matrix
  // NOTE: the direction is reversed (target to camera pos) because of camera transform
  forward := FPosition - FTarget;
  FDistance := forward.Length;   // remember the distance
  // normalize
  forward := forward * (1.0/FDistance);

  // Compute temporary up vector based on the forward vector
  // Watch out when look up/down at 90 degree
  // for example, forward vector is on the Y axis
  if SameValue(forward.x, 0.0, EPSILON) and SameValue(forward.z, 0.0, EPSILON) then
  begin
    // forward vector is pointing +Y axis
    if (forward.y > 0) then
      up := Vector3f(0, 0, -1)
    else
    // forward vector is pointing -Y axis
      up := Vector3f(0, 0, 1);
  end else
  begin
    // in general, up vector is straight up
    up := Vector3f(0, 1, 0);

    // Compute the left vector of rotation matrix
    left := CrossProduct(up, forward);
    left.Normalize();

    // Re-calculate the orthonormal up vector
    up := CrossProduct(forward, left);

    // Set inverse rotation matrix: M^-1 = M^T for Euclidean transform
    FMatrixRotation.Identity;
    FMatrixRotation.SetRow(0, left);
    FMatrixRotation.SetRow(1, up);
    FMatrixRotation.SetRow(2, forward);

    // copy it to matrix
    FMatrix.Identity;
    FMatrix.SetRow(0, left);
    FMatrix.SetRow(1, up);
    FMatrix.SetRow(2, forward);

    // Set translation part
    trans.x := FMatrix[0]*-FPosition.x + FMatrix[4]*-FPosition.y + FMatrix[8]*-FPosition.z;
    trans.y := FMatrix[1]*-FPosition.x + FMatrix[5]*-FPosition.y + FMatrix[9]*-FPosition.z;
    trans.z := FMatrix[2]*-FPosition.x + FMatrix[6]*-FPosition.y + FMatrix[10]*-FPosition.z;
    FMatrix.SetColumn(3, trans);

    // Set Euler angles
    FAngle := MatrixToAngle(FMatrixRotation);

    // Set quaternion from angle
    reversedAngle.Init(FAngle.X, -FAngle.Y, FAngle.Z);
    FQuaternion := oglMath.GetQuaternion(reversedAngle * (DEG2RAD * 0.5)); // half angle
  end;
end;

//------------------------------------------------------------------------------
// Set transform matrix with target and camera's up vectors
//------------------------------------------------------------------------------
procedure ToglCamera.LookAt(const APosition, ATarget, AUpDir: ToglVector3f);
var
  left, up, forward: ToglVector3f;   // 3 axis vectors for scene
  trans: ToglVector3f;
  reversedAngle: ToglVector3f;
begin
  // Remeber the camera and target positions
  FPosition := APosition;
  FTarget := ATarget;

  // If pos and target are same, only translate camera to position without rotation
  if (FPosition = FTarget) then
  begin
    FMatrix.Identity;
    FMatrix.Translate(-FPosition.x, -FPosition.y, -FPosition.z);
    // rotation stuff
    FMatrixRotation.Identity;
    FAngle.Init(0,0,0);
    FQuaternion.Init(1,0,0,0);
    exit;
  end;

  // Compute the forward vector
  // NOTE: the direction is reversed (target to camera pos) because of camera transform
  forward := FPosition - FTarget;
  FDistance := forward.Length;       // remember the distance
  // Normalize
  forward := forward * (1.0/FDistance);

  // Compute the left vector
  left := CrossProduct(AUpDir, forward);
  left.Normalize;

  // Recompute the orthonormal up vector
  up := CrossProduct(forward, left);

  // Set inverse rotation matrix: M^-1 = M^T for Euclidean transform
  FMatrixRotation.Identity;
  FMatrixRotation.SetRow(0, left);
  FMatrixRotation.SetRow(1, up);
  FMatrixRotation.SetRow(2, forward);

  // Copy it to matrix
  FMatrix.Identity;
  FMatrix.SetRow(0, left);
  FMatrix.SetRow(1, up);
  FMatrix.SetRow(2, forward);

  // Set translation
  trans.x := FMatrix[0]*-FPosition.x + FMatrix[4]*-FPosition.y + FMatrix[8]*-FPosition.z;
  trans.y := FMatrix[1]*-FPosition.x + FMatrix[5]*-FPosition.y + FMatrix[9]*-FPosition.z;
  trans.z := FMatrix[2]*-FPosition.x + FMatrix[6]*-FPosition.y + FMatrix[10]*-FPosition.z;
  FMatrix.SetColumn(3, trans);

  // Set Euler angles
  FAngle := MatrixToAngle(FMatrixRotation);

  // Set quaternion from angle
  reversedAngle.Init(FAngle.x, -FAngle.y, FAngle.z);
  FQuaternion := oglMath.GetQuaternion(reversedAngle * (DEG2RAD * 0.5)); // half angle
end;

procedure ToglCamera.LookAt(px, py, pz, tx, ty, tz: GLfloat);
begin
  LookAt(Vector3f(px,py,pz), Vector3f(tx,ty,tz));
end;

procedure ToglCamera.LookAt(px, py, pz, tx, ty, tz, ux, uy, uz: GLfloat);
begin
  LookAt(Vector3f(px,py,pz), Vector3f(tx,ty,tz), Vector3f(ux,uy,uz));
end;

//------------------------------------------------------------------------------
// Retrieve angles in degree from rotation matrix, M = Rx*Ry*Rz
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
function ToglCamera.MatrixToAngle(const AMatrix: ToglMatrix4f): ToglVector3f;
begin
  Result := AMatrix.GetAngle;

  // reverse yaw
  Result.Y := -Result.Y;
  Result.Z :=  Result.Z;
end;

//------------------------------------------------------------------------------
// Zoom in/out the camera position with the given delta movement and duration
// It actually moves the camera forward or backward.
// Positive delta means moving forward (decreasing distance)
//------------------------------------------------------------------------------
procedure ToglCamera.MoveForward(const Delta: GLfloat); //float duration, Gil::AnimationMode mode)
begin
  SetDistance(FDistance - Delta);
(*
    if(duration <= 0.0f)
    {
        setDistance(distance - delta);
    }
    else
    {
        forwardingFrom = distance;
        forwardingTo = distance - delta;
        forwardingTime = 0;
        forwardingDuration = duration;
        forwardingMode = mode;
        forwarding = true;
*)
end;

//------------------------------------------------------------------------------
// Move the camera position with the given duration
//------------------------------------------------------------------------------
procedure ToglCamera.MoveTo(const NewPos: ToglVector3f);  //float duration, Gil::AnimationMode mode)
begin
  SetPosition(NewPos);

  (*
    if(duration <= 0.0f)
    {
        setPosition(to);
    }
    else
    {
        movingFrom = position;
        movingTo = to;
        movingVector = movingTo - movingFrom;
        movingVector.normalize();
        movingTime = 0;
        movingDuration = duration;
        movingMode = mode;
        moving = true;
    }
  *)
end;

//------------------------------------------------------------------------------
// Rotate camera with delta angle
// NOTE: delta angle must be negated already
//------------------------------------------------------------------------------
procedure ToglCamera.Rotate(const DeltaAngle: ToglVector3f);  //float duration, Gil::AnimationMode mode)
begin
  RotateTo(Fangle + DeltaAngle); // rotateTo(angle + delta, duration, mode);
end;

//------------------------------------------------------------------------------
// Rotate camera to the given angle with duration
//------------------------------------------------------------------------------
procedure ToglCamera.RotateTo(const Angle: ToglVector3f);  //float duration, Gil::AnimationMode mode)
begin
  FQuaternionUsed := false;
  SetRotation(Angle);
(*
    quaternionUsed = false;
    if(duration <= 0.0f)
    {
        setRotation(angle);
    }
    else
    {
        turningAngleFrom = this->angle;
        turningAngleTo = angle;
        turningTime = 0;
        turningDuration = duration;
        turningMode = mode;
        turning = true;
    }
*)
end;

//------------------------------------------------------------------------------
// Rotate camera to the given quaternion with duration
//------------------------------------------------------------------------------
procedure ToglCamera.RotateTo(const Quat: ToglQuaternion); // float duration, Gil::AnimationMode mode)
begin
  FQuaternionUsed := true;
  SetRotationQuat(Quat);
(*
    quaternionUsed = true;
    if(duration <= 0.0f)
    {
        setRotation(q);
    }
    else
    {
        turningQuaternionFrom = this->quaternion;
        turningQuaternionTo = q;
        turningTime = 0;
        turningDuration = duration;
        turningMode = mode;
        turning = true;
    }
*)
end;
//------------------------------------------------------------------------------
// Set distance of camera, then recompute camera position
//------------------------------------------------------------------------------
procedure ToglCamera.SetDistance(AValue: GLfloat);
begin
  if AValue = FDistance then
    exit;
  FDistance := AValue;
  ComputeMatrix;
end;

//------------------------------------------------------------------------------
// Set position of camera, set transform matrix as well
//------------------------------------------------------------------------------
procedure ToglCamera.SetPosition(const AValue: ToglVector3f);
begin
  if AValue = FPosition then
    exit;
  FPosition := AValue;
  LookAt(FPosition, FTarget);
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
//------------------------------------------------------------------------------
procedure ToglCamera.SetRotation(AValue: ToglVector3f);
var
  reversedAngle: ToglVector3f;
begin
  // remember angles
  // NOTE: assume all angles are already reversed for camera
  FAngle := AValue;

  // remember quaternion value
  // NOTE: yaw must be negated again for quaternion
  reversedAngle.Init(FAngle.x, -FAngle.y, FAngle.z);
  FQuaternion := oglMath.GetQuaternion(reversedAngle);

  // Compute rotation matrix from angle
  FMatrixRotation := AngleToMatrix(FAngle);

  // Construct camera matrix
  ComputeMatrix();
end;

//------------------------------------------------------------------------------
// Set rotation with new quaternion
// NOTE: quaternion value is for matrix, so MatrixToAngle() will reverse yaw.
//------------------------------------------------------------------------------
procedure ToglCamera.SetRotationQuat(AValue: ToglQuaternion);
begin
  // Remember the current quaternion
  FQuaternion := AValue;

  // Quaternion to matrix
  FMatrixRotation := FQuaternion.GetMatrix;

  // Construct matrix
  ComputeMatrix;

  // Compute angle from matrix
  FAngle := MatrixToAngle(FMatrixRotation);
end;

//------------------------------------------------------------------------------
// Set target of camera, then rebuild matrix
// Rotation parts are not changed, but translation part must be recalculated
// And, position is also shifted
//------------------------------------------------------------------------------
procedure ToglCamera.SetTarget(const AValue: ToglVector3f);
var
  forward: ToglVector3f;
begin
  if AValue = FTarget then
    exit;
  FTarget := AValue;

  // forward vector of camera
  forward.Init(-FMatrix[2], -FMatrix[6], -FMatrix[10]);
  FPosition := FTarget - (FDistance * forward);
  ComputeMatrix;
end;

//------------------------------------------------------------------------------
// Shift the camera position and target left/right/up/down
//------------------------------------------------------------------------------
procedure ToglCamera.Shift(const Delta: ToglVector2f);  //float duration, Gil::AnimationMode mode)
var
  cameraLeft, cameraUp: ToglVector3f;
  deltaMovement: ToglVector3f;
  newTarget: ToglVector3f;
begin
  // Get left & up vectors of camera
  cameraLeft.Init(-FMatrix[0], -FMatrix[4], -FMatrix[8]);
  cameraUp.Init(-FMatrix[1], -FMatrix[5], -FMatrix[9]);

  // Compute delta movement
  deltaMovement := Delta.X * cameraLeft;
  deltaMovement := deltaMoveMent - Delta.Y * cameraUp;   // reverse up direction

  // Find new target position
  newTarget := FTarget + deltaMovement;

  ShiftTo(newTarget); //, duration, mode);
end;

//------------------------------------------------------------------------------
// Pan the camera target left/right/up/down with the given duration
// The camera position will be shifted after transform
//------------------------------------------------------------------------------
procedure ToglCamera.ShiftTo(const NewPos: ToglVector3f); // float duration, Gil::AnimationMode mode)
begin
  SetTarget(NewPos);
  (*
    if(duration <= 0.0f)
    {
        setTarget(to);
    }
    else
    {
        shiftingFrom = target;
        shiftingTo = to;
        shiftingVector = shiftingTo - shiftingFrom;
        shiftingVector.normalize();
        shiftingTime = 0;
        shiftingDuration = duration;
        shiftingMode = mode;
        shifting = true;
    }
  *)
end;

end.

