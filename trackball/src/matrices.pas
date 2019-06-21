///////////////////////////////////////////////////////////////////////////////
// NxN Matrix Math classes
//
// The elements of the matrix are stored as column major order.
// | 0 2 |    | 0 3 6 |    |  0  4  8 12 |
// | 1 3 |    | 1 4 7 |    |  1  5  9 13 |
//            | 2 5 8 |    |  2  6 10 14 |
//                         |  3  7 11 15 |
//
//  AUTHOR: Song Ho Ahn (song.ahn@gmail.com)
// CREATED: 2005-06-24
// UPDATED: 2016-04-12
//
// Copyright (C) 2005 Song Ho Ahn
///////////////////////////////////////////////////////////////////////////////

unit matrices;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, gl, vectors;

type
  TMatrixData2 = array[0..3] of GLfloat;
  TMatrix2 = record
    m: TMatrixData2;
    function Determinant: GLfloat;
    function Get: TMatrixData2;
    function Identity: TMatrix2;
    function Invert: TMatrix2;
    procedure SetColumn(AIndex: Integer; v: TVector2);
    procedure SetRow(AIndex: Integer; v: TVector2);
    function Transpose: TMatrix2;
  end;

  function Matrix2(m0, m1, m2, m3: GLfloat): TMatrix2;

  operator +(const A, B: TMatrix2): TMatrix2;
  operator -(const A: TMatrix2): TMatrix2;
  operator -(const A, B: TMatrix2): TMatrix2;
  operator *(const A: TMatrix2; const V: TVector2): TVector2;
  operator *(const A, B: TMatrix2): TMatrix2;
  operator =(const A, B: TMatrix2): Boolean;
  operator <>(const A, B: TMatrix2): Boolean;


type
  TMatrixData3 = array[0..8] of GLfloat;
  TMatrix3 = record
    m: TMatrixData3;
    function Determinant: GLfloat;
    function Get: TMatrixData3;
    function Identity: TMatrix3;
    function Invert: TMatrix3;
    function Transpose: TMatrix3;
  end;

  function Matrix3(src: TMatrixData3): TMatrix3;
  function Matrix3(m0, m1, m2, m3, m4, m5, m6, m7, m8: GLfloat): TMatrix3;

type
  TMatrixData4 = array[0..15] of GLfloat;
  TMatrix4 = record
  private
    tm: TMatrixData4;    // Data of transposed matrix
    function GetCoFactor(m0, m1, m2, m3, m4, m5, m6, m7, m8: GLfloat): GLfloat;
    function InvertAffine: TMatrix4;
    function InvertGeneral: TMatrix4;
  public
    m: TMatrixData4;
    function Determinant: GLfloat;
    function Get: TMatrixData4;
    function GetTranspose: TMatrixData4;
    function Identity: TMatrix4;
    function Invert: TMatrix4;
    function LookAt(Target: TVector3): TMatrix4;
    function LookAt(const Target, UpVec: TVector3): TMatrix4;
    function Rotate(Angle: GLfloat; Axis: TVector3): TMatrix4;
    function Rotate(Angle, x, y, z: GLfloat): TMatrix4;
    function RotateX(Angle: GLfloat): TMatrix4;
    function RotateY(Angle: GLfloat): TMatrix4;
    function RotateZ(Angle: GLfloat): TMatrix4;
    function Scale(s: GLfloat): TMatrix4;
    function Scale(x, y, z: GLfloat): TMatrix4;
    procedure SetColumn(AIndex: Integer; v: TVector3);
    procedure SetColumn(AIndex: Integer; v: TVector4);
    procedure SetRow(AIndex: Integer; v: TVector3);
    procedure SetRow(AIndex: Integer; v: TVector4);
    function Translate(const x, y, z: GLfloat): TMatrix4;
    function Translate(const v: TVector3): TMatrix4;
    function Transpose: TMatrix4;
  end;

  function Matrix4(src: TMatrixData4): TMatrix4;
  function Matrix4(m00, m01, m02, m03,                       // 1st column
                   m04, m05, m06, m07,                       // 2nd column
                   m08, m09, m10, m11,                       // 3rd column
                   m12, m13, m14, m15: GLfloat): TMatrix4;   // 4th column

  operator +(const A, B: TMatrix4): TMatrix4;
  operator -(const A: TMatrix4): TMatrix4;
  operator -(const A, B: TMatrix4): TMatrix4;
  operator *(const A: TMatrix4; const V: TVector3): TVector3;
  operator *(const A: TMatrix4; const V: TVector4): TVector4;
  operator *(const A, B: TMatrix4): TMatrix4;
  operator *(const s: GLfloat; const A: TMatrix4): TMatrix4;
  operator *(const V: TVector3; const A: TMatrix4): TVector3;
  operator *(const V: TVector4; const A: TMatrix4): TVector4;
  operator =(const A, B: TMatrix4): Boolean;
  operator <>(const A, B: TMatrix4): Boolean;


implementation

uses
  Math;

const
  DEG2RAD: GLfloat = pi / 180.0;
  EPSILON: GLfloat = 0.00001;

function Matrix2(m0, m1, m2, m3: GLfloat): TMatrix2;
begin
  Result.m[0] := m0;
  Result.m[1] := m1;
  Result.m[2] := m2;
  Result.m[3] := m3;
end;

{-------------------------------------------------------------------------------
  Returns the determinant of 2x2 matrix
-------------------------------------------------------------------------------}
function TMatrix2.Determinant: GLfloat;
begin
  Result := m[0] * m[3] - m[1] * m[2];
end;

function TMatrix2.Get: TMatrixData2;
begin
  Move(m[0], Result[0], SizeOf(TMatrixData2));
end;

{-------------------------------------------------------------------------------
  Identity matrix
-------------------------------------------------------------------------------}
function TMatrix2.Identity: TMatrix2;
begin
  m[0] := 1;
  m[1] := 0;
  m[2] := 0;
  m[3] := 1;
end;

{-------------------------------------------------------------------------------
  Inverse of 2x2 matrix
  If cannot find inverse, set identity matrix
-------------------------------------------------------------------------------}
function TMatrix2.Invert: TMatrix2;
var
  det: GLfloat;
  invDet: GLFloat;
  tmp: GLfloat;
begin
  det := Determinant;
  if abs(det) <= EPSILON then begin
    Result := Identity;
    exit;
  end;

  tmp := m[0];     // copy the first element
  invDet := 1.0 / det;
  m[0] :=  invDet * m[3];
  m[1] := -invDet * m[1];
  m[2] := -invDet * m[2];
  m[3] :=  invDet * tmp;

  Result := Self;
end;

procedure TMatrix2.SetColumn(AIndex: Integer; v: TVector2);
begin
  m[AIndex*2    ] := v.x;
  m[AIndex*2 + 1] := v.y;
end;

procedure TMatrix2.SetRow(AIndex: Integer; v: TVector2);
begin
  m[AIndex    ] := v.x;
  m[AIndex + 2] := v.y;
end;

{-------------------------------------------------------------------------------
 Transpose 2x2 matrix
-------------------------------------------------------------------------------}
function TMatrix2.Transpose: TMatrix2;
var
  tmp: GLfloat;
begin
  tmp := m[1];
  m[1] := m[3];
  m[3] := tmp;
  Result := Self;
end;

{-------------------------------------------------------------------------------
  Operators
-------------------------------------------------------------------------------}

operator +(const A, B: TMatrix2): TMatrix2;
begin
  Result := Matrix2(
    A.m[0] + B.m[0],
    A.m[1] + B.m[1],
    A.m[2] + B.m[2],
    A.m[3] + B.m[3]
  );
end;

operator -(const A: TMatrix2): TMatrix2;
begin
  Result := Matrix2(-A.m[0], -A.m[1], -A.m[2], -A.m[3]);
end;

operator -(const A, B: TMatrix2): TMatrix2;
begin
  Result := Matrix2(
    A.m[0] - B.m[0],
    A.m[1] - B.m[1],
    A.m[2] - B.m[2],
    A.m[3] - B.m[3]
  );
end;

operator *(const A: TMatrix2; const V: TVector2): TVector2;
begin
  Result := Vector2(
    A.m[0]*V.x + A.m[2]*V.y,
    A.m[1]*V.x + A.m[3]*V.y
  );
end;

operator *(const A, B: TMatrix2): TMatrix2;
begin
  Result := Matrix2(
    A.m[0] * B.m[0] + A.m[2] * B.m[1],
    A.m[1] * B.m[0] + A.m[3] * B.m[1],
    A.m[0] * B.m[2] + A.m[2] * B.m[3],
    A.m[1] * B.m[2] + A.m[3] * B.m[3]
  );
end;

operator =(const A, B: TMatrix2): Boolean;
begin
  Result := (A.m[0] = B.m[0]) and
            (A.m[1] = B.m[1]) and
            (A.m[2] = B.m[2]) and
            (A.m[3] = B.m[3]);
end;

operator <>(const A, B: TMatrix2): Boolean;
begin
  Result := not (A = B);
end;


{===============================================================================
                                TMatrix3
===============================================================================}
function Matrix3(src: TMatrixData3): TMatrix3;
begin
  Result := Matrix3(
    src[0],  src[1],  src[2],
    src[3],  src[4],  src[5],
    src[6],  src[7],  src[8]
  );
end;

function Matrix3(m0, m1, m2,                       // 1st column
                 m3, m4, m5,                       // 2nd column
                 m6, m7, m8: GLfloat): TMatrix3;   // 4th column
begin
  Result.m[0] := m0;   Result.m[1] := m1;   Result.m[2] := m2;
  Result.m[3] := m3;   Result.m[4] := m4;   Result.m[5] := m5;
  Result.m[6] := m6;   Result.m[7] := m7;   Result.m[8] := m8;
end;


{-------------------------------------------------------------------------------
  Return determinant of 3x3 matrix
-------------------------------------------------------------------------------}
function TMatrix3.Determinant: GLfloat;
begin
  Result :=  m[0] * (m[4] * m[8] - m[5] * m[7]) -
             m[1] * (m[3] * m[8] - m[5] * m[6]) +
             m[2] * (m[3] * m[7] - m[4] * m[6]);
end;

function TMatrix3.Get: TMatrixData3;
begin
  Move(m[0], Result[0], SizeOf(TMatrixData3));
end;

function TMatrix3.Identity: TMatrix3;
begin
  FillChar(m, SizeOf(m), 0);
  m[0] := 1.0;
  m[4] := 1.0;
  m[8] := 1.0;
  Result := Self;
end;

{-------------------------------------------------------------------------------
  Calculate inverxe of 3x3 matrix
   If cannot find inverse, set identity matrix
-------------------------------------------------------------------------------}
function TMatrix3.Invert: TMatrix3;
var
  det, invDet: GLfloat;
  tmp: array[0..8] of GLfloat;
begin
  tmp[0] := m[4] * m[8] - m[5] * m[7];
  tmp[1] := m[2] * m[7] - m[1] * m[8];
  tmp[2] := m[1] * m[5] - m[2] * m[4];
  tmp[3] := m[5] * m[6] - m[3] * m[8];
  tmp[4] := m[0] * m[8] - m[2] * m[6];
  tmp[5] := m[2] * m[3] - m[0] * m[5];
  tmp[6] := m[3] * m[7] - m[4] * m[6];
  tmp[7] := m[1] * m[6] - m[0] * m[7];
  tmp[8] := m[0] * m[4] - m[1] * m[3];

  // check determinant if it is 0
  det := m[0] * tmp[0] + m[1] * tmp[3] + m[2] * tmp[6];
  if (abs(det) <= EPSILON) then begin
    Result := Identity;    // cannot inverse, make it idenety matrix
    exit;
  end;

  // divide by the determinant
  invDet := 1.0 / det;
  m[0] := invDet * tmp[0];
  m[1] := invDet * tmp[1];
  m[2] := invDet * tmp[2];
  m[3] := invDet * tmp[3];
  m[4] := invDet * tmp[4];
  m[5] := invDet * tmp[5];
  m[6] := invDet * tmp[6];
  m[7] := invDet * tmp[7];
  m[8] := invDet * tmp[8];

  Result := self;
end;

{-------------------------------------------------------------------------------
  Transpose 3x3 matrix
-------------------------------------------------------------------------------}
function TMatrix3.Transpose: TMatrix3;
begin
  Exchange(m[1],  m[3]);
  Exchange(m[2],  m[6]);
  Exchange(m[5],  m[7]);

  Result := self;
end;

{===============================================================================
                                TMatrix4
===============================================================================}
function Matrix4(src: TMatrixData4): TMatrix4;
begin
  Result := Matrix4(
    src[0], src[1], src[2],  src[3],  src[4],  src[5],  src[6],  src[7],
    src[8], src[9], src[10], src[11], src[12], src[13], src[14], src[15]
  );
end;

function Matrix4(m00, m01, m02, m03,                       // 1st column
                 m04, m05, m06, m07,                       // 2nd column
                 m08, m09, m10, m11,                       // 3rd column
                 m12, m13, m14, m15: GLfloat): TMatrix4;   // 4th column
begin
  Result.m[0] := m00;   Result.m[1] := m01;   Result.m[2] := m02;   Result.m[3] := m03;
  Result.m[4] := m04;   Result.m[5] := m05;   Result.m[6] := m06;   Result.m[7] := m07;
  Result.m[8] := m08;   Result.m[9] := m09;   Result.m[10]:= m10;   Result.m[11]:= m11;
  Result.m[12]:= m12;   Result.m[13]:= m13;   Result.m[14]:= m14;   Result.m[15]:= m15;
end;


{-------------------------------------------------------------------------------
  Returns the determinant of 4x4 matrix
-------------------------------------------------------------------------------}
function TMatrix4.Determinant: GLfloat;
begin
  Result :=
    m[0] * GetCofactor(m[5],m[6],m[7], m[9],m[10],m[11], m[13],m[14],m[15]) -
    m[1] * GetCofactor(m[4],m[6],m[7], m[8],m[10],m[11], m[12],m[14],m[15]) +
    m[2] * GetCofactor(m[4],m[5],m[7], m[8],m[9], m[11], m[12],m[13],m[15]) -
    m[3] * GetCofactor(m[4],m[5],m[6], m[8],m[9], m[10], m[12],m[13],m[14]);
end;

{-------------------------------------------------------------------------------
  Returns the matrix coefficients as an array to be passed to OpenGL
-------------------------------------------------------------------------------}
function TMatrix4.Get: TMatrixData4;
begin
  Move(m[0], Result[0], SizeOf(TMatrixData4));
end;

{-------------------------------------------------------------------------------
  Computes the cofactor of 3x3 minor matrix without sign
  Input params are 9 elements of the minor matrix
  NOTE: The caller must know its sign.
-------------------------------------------------------------------------------}
function TMatrix4.GetCoFactor(m0, m1, m2, m3, m4, m5, m6, m7, m8: GLfloat): GLfloat;
begin
  Result := m0 * (m4 * m8 - m5 * m7) -
            m1 * (m3 * m8 - m5 * m6) +
            m2 * (m3 * m7 - m4 * m6);
end;

{-------------------------------------------------------------------------------
  Returns the coefficients of the transposed matrix as an array to be passed
  to OpenGL.
-------------------------------------------------------------------------------}
function TMatrix4.GetTranspose: TMatrixData4;
begin
  tm[0] := m[0];   tm[1] := m[4];   tm[2] := m[8];   tm[3] := m[12];
  tm[4] := m[1];   tm[5] := m[5];   tm[6] := m[9];   tm[7] := m[13];
  tm[8] := m[2];   tm[9] := m[6];   tm[10]:= m[10];  tm[11]:= m[14];
  tm[12]:= m[3];   tm[13]:= m[7];   tm[14]:= m[11];  tm[15]:= m[15];

  Move(tm[0], Result[0], SizeOf(TMatrixData4));
end;

{-------------------------------------------------------------------------------
  Sets the matrix coefficient to those of the identity matrix
-------------------------------------------------------------------------------}
function TMatrix4.Identity: TMatrix4;
begin
  FillChar(m[0], SizeOf(TMatrixData4), 0);
  m[0] := 1.0;
  m[5] := 1.0;
  m[10] := 1.0;
  m[15] := 1.0;
end;

{-------------------------------------------------------------------------------
  Calculate inverse of 4x4 matrix
-------------------------------------------------------------------------------}
function TMatrix4.Invert: TMatrix4;
begin
  // If the 4th row is [0,0,0,1] then it is affine matrix and
  // it has no projective transformation.
  if (m[3] = 0) and (m[7] = 0) and (m[11] = 0) and (m[15] = 1) then
    Result := InvertAffine
  else begin
    Result := InvertGeneral;
        (*@@ invertProjective() is not optimized (slower than generic one)
        if(fabs(m[0]*m[5] - m[1]*m[4]) > EPSILON)
            this->invertProjective();   // inverse using matrix partition
        else
            this->invertGeneral();      // generalized inverse
        *)
  end;
end;

{-------------------------------------------------------------------------------
  Compute the inverse of a 4x4 affine transformation matrix

  Affine transformations are generalizations of Euclidean transformations.
  Affine transformation includes translation, rotation, reflection, scaling,
  and shearing. Length and angle are NOT preserved.
    M = [ R | T ]
        [ --+-- ]    (R denotes 3x3 rotation/scale/shear matrix)
        [ 0 | 1 ]    (T denotes 1x3 translation matrix)

    y = M*x  ->  y = R*x + T  ->  x = R^-1*(y - T)  ->  x = R^-1*y - R^-1*T

    [ R | T ]-1   [ R^-1 | -R^-1 * T ]
    [ --+-- ]   = [ -----+---------- ]
    [ 0 | 1 ]     [  0   +     1     ]
-------------------------------------------------------------------------------}
function TMatrix4.InvertAffine: TMatrix4;
var
  r: TMatrix3;
  x, y, z: GLfloat;
begin
  // R^-1
  r := Matrix3(m[0],m[1],m[2], m[4],m[5],m[6], m[8],m[9],m[10]);
  r.Invert;

  m[0] := r.m[0];  m[1] := r.m[1];  m[2] := r.m[2];
  m[4] := r.m[3];  m[5] := r.m[4];  m[6] := r.m[5];
  m[8] := r.m[6];  m[9] := r.m[7];  m[10]:= r.m[8];

  // -R^-1 * T
  x := m[12];
  y := m[13];
  z := m[14];
  m[12] := -(r.m[0] * x + r.m[3] * y + r.m[6] * z);
  m[13] := -(r.m[1] * x + r.m[4] * y + r.m[7] * z);
  m[14] := -(r.m[2] * x + r.m[5] * y + r.m[8] * z);

  // last row should be unchanged (0,0,0,1)
  m[3] := 0.0;  m[7] := 0.0;  m[11] := 0.0;  m[15] := 1.0;

  Result := Self;
end;

{-------------------------------------------------------------------------------
  Compute the inverse of a general 4x4 matrix using Cramer's Rule
  If cannot find inverse, return indentity matrix
     M^-1 = adj(M) / det(M)
-------------------------------------------------------------------------------}
function TMatrix4.InvertGeneral: TMatrix4;
var
  cofactor0, cofactor1, cofactor2, cofactor3: GLfloat;
  cofactor4, cofactor5, cofactor6, cofactor7: GLfloat;
  cofactor8, cofactor9, cofactor10, cofactor11: GLfloat;
  cofactor12, cofactor13, cofactor14, cofactor15: GLfloat;
  det, invDet: GLfloat;
begin
  cofactor0 := GetCofactor(m[5],m[6],m[7], m[9],m[10],m[11], m[13],m[14],m[15]);
  cofactor1 := GetCofactor(m[4],m[6],m[7], m[8],m[10],m[11], m[12],m[14],m[15]);
  cofactor2 := GetCofactor(m[4],m[5],m[7], m[8],m[9], m[11], m[12],m[13],m[15]);
  cofactor3 := GetCofactor(m[4],m[5],m[6], m[8],m[9], m[10], m[12],m[13],m[14]);

  // get determinant
  det := m[0] * cofactor0 - m[1] * cofactor1 + m[2] * cofactor2 - m[3] * cofactor3;
  if (abs(det) <= EPSILON) then begin
    Result := Identity;
    exit;
  end;

  // get rest of cofactors for adj(M)
  cofactor4 := GetCofactor(m[1],m[2],m[3], m[9],m[10],m[11], m[13],m[14],m[15]);
  cofactor5 := GetCofactor(m[0],m[2],m[3], m[8],m[10],m[11], m[12],m[14],m[15]);
  cofactor6 := GetCofactor(m[0],m[1],m[3], m[8],m[9], m[11], m[12],m[13],m[15]);
  cofactor7 := GetCofactor(m[0],m[1],m[2], m[8],m[9], m[10], m[12],m[13],m[14]);

  cofactor8 := GetCofactor(m[1],m[2],m[3], m[5],m[6], m[7],  m[13],m[14],m[15]);
  cofactor9 := GetCofactor(m[0],m[2],m[3], m[4],m[6], m[7],  m[12],m[14],m[15]);
  cofactor10:= GetCofactor(m[0],m[1],m[3], m[4],m[5], m[7],  m[12],m[13],m[15]);
  cofactor11:= GetCofactor(m[0],m[1],m[2], m[4],m[5], m[6],  m[12],m[13],m[14]);

  cofactor12:= GetCofactor(m[1],m[2],m[3], m[5],m[6], m[7],  m[9], m[10],m[11]);
  cofactor13:= GetCofactor(m[0],m[2],m[3], m[4],m[6], m[7],  m[8], m[10],m[11]);
  cofactor14:= GetCofactor(m[0],m[1],m[3], m[4],m[5], m[7],  m[8], m[9], m[11]);
  cofactor15:= GetCofactor(m[0],m[1],m[2], m[4],m[5], m[6],  m[8], m[9], m[10]);

  // build inverse matrix = adj(M) / det(M)
  // adjugate of M is the transpose of the cofactor matrix of M
  invDet := 1.0 / det;
  m[0] :=  invDet * cofactor0;
  m[1] := -invDet * cofactor4;
  m[2] :=  invDet * cofactor8;
  m[3] := -invDet * cofactor12;

  m[4] := -invDet * cofactor1;
  m[5] :=  invDet * cofactor5;
  m[6] := -invDet * cofactor9;
  m[7] :=  invDet * cofactor13;

  m[8] :=  invDet * cofactor2;
  m[9] := -invDet * cofactor6;
  m[10]:=  invDet * cofactor10;
  m[11]:= -invDet * cofactor14;

  m[12]:= -invDet * cofactor3;
  m[13]:=  invDet * cofactor7;
  m[14]:= -invDet * cofactor11;
  m[15]:=  invDet * cofactor15;

  Result := self;
end;

{-------------------------------------------------------------------------------
  Rotate matrix to face along the target direction
  NOTE: This function will clear the previous rotation and scale info and
  rebuild the matrix with the target vector. But it will keep the previous
  translation values.
-------------------------------------------------------------------------------}
function TMatrix4.LookAt(Target: TVector3): TMatrix4;
var
  fwd, up, down, left: TVector3;
begin
  // copy lookat vector and normalize
  fwd := Target;
  fwd.Normalize();

  // compute temporal up vector
  // if forward vector is near Y-axis, use up vector (0,0,-1) or (0,0,1)

  if (abs(fwd.x) < EPSILON) and (abs(fwd.z) < EPSILON) then
  begin
    // forward vector is pointing +Y axis
    if (fwd.y > 0) then
      up := Vector3(0, 0, -1)
    else
    // forward vector is pointing -Y axis
      up := Vector3(0, 0, 1);
  end else
    // assume up vector is +Y axis
    up := Vector3(0, 1, 0);

  // compute left vector
  left := up.Cross(fwd);
  left.Normalize;

  // re-compute up vector
  up := fwd.Cross(left);
  up.Normalize;

  // NOTE: overwrite rotation and scale info of the current matrix
  SetColumn(0, left);
  SetColumn(1, up);
  SetColumn(2, fwd);

  Result := Self;
end;

function TMatrix4.LookAt(const Target, UpVec: TVector3): TMatrix4;
var
  fwd, up, left: TVector3;
begin
  // copy lookat vector and normalize
  fwd := Target;
  fwd.Normalize;

  // compute left vector
  left := UpVec.Cross(fwd);
  left.Normalize;

  // compute orthonormal up vector
  up := fwd.Cross(left);
  up.Normalize;

  // NOTE: overwrite rotation and scale info of the current matrix
  SetColumn(0, left);
  SetColumn(1, up);
  SetColumn(2, fwd);

  Result := self;
end;

{-------------------------------------------------------------------------------
  Build a rotation matrix with given angle(degree) and rotation axis, then
  multiply it with this object
-------------------------------------------------------------------------------}
function TMatrix4.Rotate(Angle: GLfloat; Axis: TVector3): TMatrix4;
begin
  Result := Rotate(angle, axis.x, axis.y, axis.z);
end;

function TMatrix4.Rotate(Angle, x, y, z: GLfloat): TMatrix4;
var
  s, c, c1: GLfloat;
  m0, m1, m2, m4, m5, m6, m8, m9, m10, m12, m13, m14: GLfloat;
  r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10: GLfloat;
begin
  SinCos(Angle * DEG2RAD, s, c);
  c1 := 1.0 - c;

  m0 := m[0];  m4 := m[4];  m8 := m[8];  m12:= m[12];
  m1 := m[1];  m5 := m[5];  m9 := m[9];  m13:= m[13];
  m2 := m[2];  m6 := m[6];  m10:= m[10]; m14:= m[14];

  // build rotation matrix
  r0 := x * x * c1 + c;
  r1 := x * y * c1 + z * s;
  r2 := x * z * c1 - y * s;
  r4 := x * y * c1 - z * s;
  r5 := y * y * c1 + c;
  r6 := y * z * c1 + x * s;
  r8 := x * z * c1 + y * s;
  r9 := y * z * c1 - x * s;
  r10:= z * z * c1 + c;

  // multiply rotation matrix
  m[0] := r0 * m0 + r4 * m1 + r8 * m2;
  m[1] := r1 * m0 + r5 * m1 + r9 * m2;
  m[2] := r2 * m0 + r6 * m1 + r10* m2;
  m[4] := r0 * m4 + r4 * m5 + r8 * m6;
  m[5] := r1 * m4 + r5 * m5 + r9 * m6;
  m[6] := r2 * m4 + r6 * m5 + r10* m6;
  m[8] := r0 * m8 + r4 * m9 + r8 * m10;
  m[9] := r1 * m8 + r5 * m9 + r9 * m10;
  m[10]:= r2 * m8 + r6 * m9 + r10* m10;
  m[12]:= r0 * m12+ r4 * m13+ r8 * m14;
  m[13]:= r1 * m12+ r5 * m13+ r9 * m14;
  m[14]:= r2 * m12+ r6 * m13+ r10* m14;

  Result := self;
end;

function TMatrix4.RotateX(Angle: GLfloat): TMatrix4;
var
  c, s: GLfloat;
  m1, m2, m5, m6, m9, m10, m13, m14: GLFloat;
begin
  SinCos(Angle * DEG2RAD, s, c);

  m1 := m[1];  m2 := m[2];
  m5 := m[5];  m6 := m[6];
  m9 := m[9];  m10:= m[10];
  m13:= m[13]; m14:= m[14];

  m[1] := m1 * c + m2 *-s;
  m[2] := m1 * s + m2 * c;
  m[5] := m5 * c + m6 *-s;
  m[6] := m5 * s + m6 * c;
  m[9] := m9 * c + m10*-s;
  m[10]:= m9 * s + m10* c;
  m[13]:= m13* c + m14*-s;
  m[14]:= m13* s + m14* c;

  Result := Self;
end;

function TMatrix4.RotateY(Angle: GLfloat): TMatrix4;
var
  c, s: GLfloat;
  m0, m2, m4, m6, m8, m10, m12, m14: GLfloat;
begin
  SinCos(Angle * DEG2RAD, s, c);

  m0 := m[0];  m2 := m[2];
  m4 := m[4];  m6 := m[6];
  m8 := m[8];  m10:= m[10];
  m12:= m[12]; m14:= m[14];

  m[0] := m0 * c + m2 * s;
  m[2] := m0 *-s + m2 * c;
  m[4] := m4 * c + m6 * s;
  m[6] := m4 *-s + m6 * c;
  m[8] := m8 * c + m10* s;
  m[10]:= m8 *-s + m10* c;
  m[12]:= m12* c + m14* s;
  m[14]:= m12*-s + m14* c;

  Result := Self;
end;

function TMatrix4.RotateZ(Angle: GLfloat): TMatrix4;
var
  c, s: GLfloat;
  m0, m1, m4, m5, m8, m9, m12, m13: GLfloat;
begin
  SinCos(Angle * DEG2RAD, s, c);

  m0 := m[0];  m1 := m[1];
  m4 := m[4];  m5 := m[5];
  m8 := m[8];  m9 := m[9];
  m12:= m[12]; m13:= m[13];

  m[0] := m0 * c + m1 *-s;
  m[1] := m0 * s + m1 * c;
  m[4] := m4 * c + m5 *-s;
  m[5] := m4 * s + m5 * c;
  m[8] := m8 * c + m9 *-s;
  m[9] := m8 * s + m9 * c;
  m[12]:= m12* c + m13*-s;
  m[13]:= m12* s + m13* c;

  Result := Self;
end;

{-------------------------------------------------------------------------------
  Uniform scale
-------------------------------------------------------------------------------}
function TMatrix4.Scale(s: GLfloat): TMatrix4;
begin
  Result := Scale(s, s, s);
end;

function TMatrix4.Scale(x, y, z: GLfloat): TMatrix4;
begin
  m[0] *= x;   m[4] *= x;   m[8] *= x;   m[12] *= x;
  m[1] *= y;   m[5] *= y;   m[9] *= y;   m[13] *= y;
  m[2] *= z;   m[6] *= z;   m[10]*= z;   m[14] *= z;
  Result := Self;
end;

procedure TMatrix4.SetColumn(AIndex: Integer; v: TVector3);
begin
  m[AIndex*4    ] := v.x;
  m[AIndex*4 + 1] := v.y;
  m[AIndex*4 + 2] := v.z;
end;

procedure TMatrix4.SetColumn(AIndex: Integer; v: TVector4);
begin
  m[AIndex*4    ] := v.x;
  m[AIndex*4 + 1] := v.y;
  m[AIndex*4 + 2] := v.z;
  m[AIndex*4 + 3] := v.w;
end;

procedure TMatrix4.SetRow(AIndex: Integer; v: TVector3);
begin
  m[AIndex] := v.x;
  m[AIndex + 4] := v.y;
  m[AIndex + 8] := v.z;
end;

procedure TMatrix4.SetRow(AIndex: Integer; v: TVector4);
begin
  m[AIndex    ] := v.x;
  m[AIndex + 4] := v.y;
  m[AIndex + 8] := v.z;
  m[AIndex +12] := v.w;
end;

function TMatrix4.Transpose: TMatrix4;
begin
  Exchange(m[1],  m[4]);
  Exchange(m[2],  m[8]);
  Exchange(m[3],  m[12]);
  Exchange(m[6],  m[9]);
  Exchange(m[7],  m[13]);
  Exchange(m[11], m[14]);
end;

{-------------------------------------------------------------------------------
  Translate this matrix by (x, y, z)
-------------------------------------------------------------------------------}
function TMatrix4.Translate(const x, y, z: GLfloat): TMatrix4;
begin
  m[0] += m[3] * x;   m[4] += m[7] * x;   m[8] += m[11]* x;   m[12]+= m[15]* x;
  m[1] += m[3] * y;   m[5] += m[7] * y;   m[9] += m[11]* y;   m[13]+= m[15]* y;
  m[2] += m[3] * z;   m[6] += m[7] * z;   m[10]+= m[11]* z;   m[14]+= m[15]* z;
end;

function TMatrix4.Translate(const v: TVector3): TMatrix4;
begin
  Result := Translate(v.x, v.y, v.z);
end;


{-------------------------------------------------------------------------------
  Operators
-------------------------------------------------------------------------------}
operator +(const A, B: TMatrix4): TMatrix4;
begin
  Result := Matrix4(
    A.m[0]+B.m[0],   A.m[1]+B.m[1],   A.m[2]+B.m[2],   A.m[3]+B.m[3],
    A.m[4]+B.m[4],   A.m[5]+B.m[5],   A.m[6]+B.m[6],   A.m[7]+B.m[7],
    A.m[8]+B.m[8],   A.m[9]+B.m[9],   A.m[10]+B.m[10], A.m[11]+B.m[11],
    A.m[12]+B.m[12], A.m[13]+B.m[13], A.m[14]+B.m[14], A.m[15]+B.m[15]
  );
end;

operator -(const A: TMatrix4): TMatrix4;
begin
  Result := Matrix4(
    -A.m[0],  -A.m[1],  -A.m[2],  -A.m[3],
    -A.m[4],  -A.m[5],  -A.m[6],  -A.m[7],
    -A.m[8],  -A.m[9],  -A.m[10], -A.m[11],
    -A.m[12], -A.m[13], -A.m[14], -A.m[15]
  );
end;

operator -(const A, B: TMatrix4): TMatrix4;
begin
  Result := Matrix4(
    A.m[0]-B.m[0],   A.m[1]-B.m[1],   A.m[2]-B.m[2],   A.m[3]-B.m[3],
    A.m[4]-B.m[4],   A.m[5]-B.m[5],   A.m[6]-B.m[6],   A.m[7]-B.m[7],
    A.m[8]-B.m[8],   A.m[9]-B.m[9],   A.m[10]-B.m[10], A.m[11]-B.m[11],
    A.m[12]-B.m[12], A.m[13]-B.m[13], A.m[14]-B.m[14], A.m[15]-B.m[15]
  );
end;

operator *(const A: TMatrix4; const V: TVector3): TVector3;
begin
  Result := Vector3(
    A.m[0]*V.x + A.m[4]*V.y + A.m[8]*V.z  + A.m[12],
    A.m[1]*V.x + A.m[5]*V.y + A.m[9]*V.z  + A.m[13],
    A.m[2]*V.x + A.m[6]*V.y + A.m[10]*V.z + A.m[14]
  );
end;

operator *(const A: TMatrix4; const V: TVector4): TVector4;
begin
  Result := Vector4(
    A.m[0]*V.x + A.m[4]*V.y + A.m[8]*V.z  + A.m[12]*V.w,
    A.m[1]*V.x + A.m[5]*V.y + A.m[9]*V.z  + A.m[13]*V.w,
    A.m[2]*V.x + A.m[6]*V.y + A.m[10]*V.z + A.m[14]*V.w,
    A.m[3]*V.x + A.m[7]*V.y + A.m[11]*V.z + A.m[15]*V.w
  );
end;

operator *(const A, B: TMatrix4): TMatrix4;
begin
  Result := Matrix4(
    A.m[0]*B.m[0]  + A.m[4]*B.m[1]  + A.m[8]*B.m[2]  + A.m[12]*B.m[3],   A.m[1]*B.m[0]  + A.m[5]*B.m[1]  + A.m[9]*B.m[2]  + A.m[13]*B.m[3],   A.m[2]*B.m[0]  + A.m[6]*B.m[1]  + A.m[10]*B.m[2]  + A.m[14]*B.m[3],   A.m[3]*B.m[0]  + A.m[7]*B.m[1]  + A.m[11]*B.m[2]  + A.m[15]*B.m[3],
    A.m[0]*B.m[4]  + A.m[4]*B.m[5]  + A.m[8]*B.m[6]  + A.m[12]*B.m[7],   A.m[1]*B.m[4]  + A.m[5]*B.m[5]  + A.m[9]*B.m[6]  + A.m[13]*B.m[7],   A.m[2]*B.m[4]  + A.m[6]*B.m[5]  + A.m[10]*B.m[6]  + A.m[14]*B.m[7],   A.m[3]*B.m[4]  + A.m[7]*B.m[5]  + A.m[11]*B.m[6]  + A.m[15]*B.m[7],
    A.m[0]*B.m[8]  + A.m[4]*B.m[9]  + A.m[8]*B.m[10] + A.m[12]*B.m[11],  A.m[1]*B.m[8]  + A.m[5]*B.m[9]  + A.m[9]*B.m[10] + A.m[13]*B.m[11],  A.m[2]*B.m[8]  + A.m[6]*B.m[9]  + A.m[10]*B.m[10] + A.m[14]*B.m[11],  A.m[3]*B.m[8]  + A.m[7]*B.m[9]  + A.m[11]*B.m[10] + A.m[15]*B.m[11],
    A.m[0]*B.m[12] + A.m[4]*B.m[13] + A.m[8]*B.m[14] + A.m[12]*B.m[15],  A.m[1]*B.m[12] + A.m[5]*B.m[13] + A.m[9]*B.m[14] + A.m[13]*B.m[15],  A.m[2]*B.m[12] + A.m[6]*B.m[13] + A.m[10]*B.m[14] + A.m[14]*B.m[15],  A.m[3]*B.m[12] + A.m[7]*B.m[13] + A.m[11]*B.m[14] + A.m[15]*B.m[15]
  );
end;

operator *(const s: GLfloat; const A: TMatrix4): TMatrix4;
begin
  Result := Matrix4(
    s*A.m[0], s*A.m[1], s*A.m[2],  s*A.m[3],  s*A.m[4],  s*A.m[5],  s*A.m[6],  s*A.m[7],
    s*A.m[8], s*A.m[9], s*A.m[10], s*A.m[11], s*A.m[12], s*A.m[13], s*A.m[14], s*A.m[15]
  );
end;

operator *(const V: TVector3; const A: TMatrix4): TVector3;
begin
  Result := Vector3(
    v.x*A.m[0] + v.y*A.m[1] + v.z*A.m[2],
    v.x*A.m[4] + v.y*A.m[5] + v.z*A.m[6],
    v.x*A.m[8] + v.y*A.m[9] + v.z*A.m[10]
  );
end;

operator *(const V: TVector4; const A: TMatrix4): TVector4;
begin
  Result := Vector4(
    v.x*A.m[0] + v.y*A.m[1] + v.z*A.m[2] + v.w*A.m[3],
    v.x*A.m[4] + v.y*A.m[5] + v.z*A.m[6] + v.w*A.m[7],
    v.x*A.m[8] + v.y*A.m[9] + v.z*A.m[10] + v.w*A.m[11],
    v.x*A.m[12] + v.y*A.m[13] + v.z*A.m[14] + v.w*A.m[15]
  );
end;

operator =(const A, B: TMatrix4): Boolean;
begin
  Result :=
    (A.m[0] = B.m[0])  and (A.m[1] = B.m[1])  and (A.m[2] = B.m[2])  and (A.m[3] = B.m[3])  and
    (A.m[4] = B.m[4])  and (A.m[5] = B.m[5])  and (A.m[6] = B.m[6])  and (A.m[7] = B.m[7])  and
    (A.m[8] = B.m[8])  and (A.m[9] = B.m[9])  and (A.m[10]= B.m[10]) and (A.m[11]= B.m[11]) and
    (A.m[12]= B.m[12]) and (A.m[13]= B.m[13]) and (A.m[14]= B.m[14]) and (A.m[15]= B.m[15]);
end;

operator <>(const A, B: TMatrix4): Boolean;
begin
  Result := not (A = B);
end;

end.

