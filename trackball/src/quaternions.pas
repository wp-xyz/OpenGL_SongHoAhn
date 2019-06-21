///////////////////////////////////////////////////////////////////////////////
// Quaternion.h
//
// Quaternion class represented as sum of a scalar and a vector(rotation axis)
// parts; [s, v] = s + (ix + jy + kz)
//
// When the quaternion is used for 3D rotation, initialize the quaternion with
// the half of the rotation angle (radian), because of double multiplication by
// its inverse, qpq'.
//
//  AUTHOR: Song Ho Ahn (song.ahn@gmail.com)
// CREATED: 2011-12-04
// UPDATED: 2015-04-12
//
// Copyright (C) 2011-2015. Song Ho Ahn
///////////////////////////////////////////////////////////////////////////////

unit quaternions;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, gl, vectors, matrices;

type
  TQuaternion = record
    s: GLfloat;         // scalar part
    x, y, z: GLFloat;   // vector part x,y,z
    function Conjugate: TQuaternion;
    function GetMatrix: TMatrix4;
    function Info(AFloatFormat: String = '0.000'): String;
    function Invert: TQuaternion;
    function Length: GLfloat;
    function Normalize: TQuaternion;
  end;

  function Quaternion(s, x, y, z: GLfloat): TQuaternion;
  function Quaternion(const axis: TVector3; angle: GLfloat): TQuaternion;
  function GetQuaternion(const v1, v2: TVector3): TQuaternion;

  operator +(const q1,q2: TQuaternion): TQuaternion;
  operator -(const q: TQuaternion): TQuaternion;
  operator -(const q1,q2: TQuaternion): TQuaternion;
  operator *(const q: TQuaternion; a: GLfloat):  TQuaternion;
  operator *(a: GLfloat; const q: TQuaternion): TQuaternion;
  operator *(q1, q2: TQuaternion): TQuaternion;


implementation

uses
  Math;

function Quaternion(s, x, y, z: GLfloat): TQuaternion;
begin
  Result.s := s;
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

function Quaternion(const axis: TVector3; angle: GLfloat): TQuaternion;
var
  v: TVector3;
  sin_angle: GLfloat;
  cos_angle: GLfloat;
begin
  v := axis;
  v.Normalize;                            // convert to unit vector
  SinCos(angle, sin_angle, cos_angle);    // angle is radian
  Result.s := cos_angle;
  Result.x := v.x * sin_angle;
  Result.y := v.y * sin_angle;
  Result.z := v.z * sin_angle;
end;

function Quaternion(const v1, v2: TVector3): TQuaternion;
const
  EPSILON = 0.001;
  HALF_PI = pi * 0.5;
var
  v: TVector3;
  u1, u2: TVector3;
  angle: GLfloat;
begin
  // if two vectors are equal return the vector with 0 rotation
  if v1.equal(v2, EPSILON) then begin
    Result := Quaternion(v1, 0);
    exit;
  end;

  // if two vectors are opposite return a perpendicular vector with 180 angle
  if v1.Equal(-v2, EPSILON) then begin
    if (v1.x > -EPSILON) and (v1.x < EPSILON) then    // if x ~= 0
      v := Vector3(1, 0, 0)
    else
    if (v1.y > -EPSILON) and (v1.y < EPSILON) then    // if y ~= 0
      v := Vector3(0, 1, 0)
    else                                              // if z ~= 0
      v := Vector3(0, 0, 1);
    Result := Quaternion(v, HALF_PI);
    exit;
  end;

  u1 := v1;                // convert to normal vector
  u2 := v2;
  u1.Normalize();
  u2.Normalize();

  v := u1.cross(u2);                     // compute rotation axis
  angle := arccos(u1.dot(u2));           // rotation angle
  Result := Quaternion(v, angle*0.5);    // return half angle for quaternion
end;

function GetQuaternion(const v1,v2: TVector3): TQuaternion;
const
  EPSILON = 0.001;
  HALF_PI = pi / 2;
var
  v, u1, u2: TVector3;
  angle: GLfloat;
begin
  // if two vectors are equal return the vector with 0 rotation
  if v1.Equal(v2, EPSILON) then begin
    Result := Quaternion(v1, 0);
    exit;
  end else
  // if two vectors are opposite return a perpendicular vector with 180 angle
  if v1.Equal(-v2, EPSILON) then begin
    if (v1.x > -EPSILON) and (v1.x < EPSILON) then       // if x ~= 0
      v := Vector3(1, 0, 0)
    else
    if (v1.y > -EPSILON) and (v1.y < EPSILON) then       // if y ~= 0
      v := Vector3(0, 1, 0)
    else
      v := Vector3(0, 0, 1);
    Result := Quaternion(v, HALF_PI);
    exit;
  end;

  u1 := v1;                             // convert to normal vector
  u2 := v2;
  u1.Normalize;
  u2.Normalize;

  v := u1.cross(u2);                    // compute rotation axis
  angle := arccos(u1.dot(u2));          // rotation angle
  Result := Quaternion(v, angle*0.5);   // return half angle for quaternion
end;


{ TQuaternion }

function TQuaternion.Conjugate: TQuaternion;
begin
  Result.x := -x;
  Result.y := -y;
  Result.z := -z;
end;

function TQuaternion.GetMatrix: TMatrix4;
var
  x2, y2, z2, xx2, xy2, xz2, yy2, yz2, zz2, sx2, sy2, sz2: GLfloat;
begin
  // NOTE: assume the quaternion is unit length
  // compute common values
  x2  := x + x;
  y2  := y + y;
  z2  := z + z;
  xx2 := x * x2;
  xy2 := x * y2;
  xz2 := x * z2;
  yy2 := y * y2;
  yz2 := y * z2;
  zz2 := z * z2;
  sx2 := s * x2;
  sy2 := s * y2;
  sz2 := s * z2;

  // build 4x4 matrix and return
  Result := Matrix4(1 - (yy2 + zz2),  xy2 - sz2,        xz2 + sy2,        0,
                    xy2 + sz2,        1 - (xx2 + zz2),  yz2 - sx2,        0,
                    xz2 - sy2,        yz2 + sx2,        1 - (xx2 + yy2),  0,
                    0,                0,                0,                1);

  // for non-unit quaternion
  // ss+xx-yy-zz, 2xy-2sz,     2xz+2sy,     0
  // 2xy+2sz,     ss-xx+yy-zz, 2yz+2sx,     0
  // 2xz-2sy,     2yz-2sx,     ss-xx-yy+zz, 0
  // 0,           0,           0,           1
end;

function TQuaternion.Info(AFloatFormat: String = '0.000'): String;
begin
  Result := '(' + FormatFloat(AFloatFormat, s) + ', ' +
                  FormatFloat(AFloatFormat, x) + ', ' +
                  FormatFloat(AFloatFormat, y) + ', ' +
                  FormatFloat(AFloatFormat, z) + ')';
end;

function TQuaternion.Invert: TQuaternion;
const
  EPSILON = 0.00001;
var
  d: GLfloat;
begin
  d := s*s + x*x + y*y + z*z;
  if d < EPSILON then begin
    Result := self;
    exit;   // do nothing if it is zero
  end;
  Result := Conjugate * (1.0 / d);   // q* / |q||q|
end;

function TQuaternion.Length: GLfloat;
begin
  Result := sqrt(s*s + x*x + y*y + z*z);
end;

function TQuaternion.Normalize: TQuaternion;
const
  EPSILON = 0.00001;
var
  d: GLfloat;
  invLength: GLfloat;
begin
  d := s*s + x*x + y*y + z*z;
  if d < EPSILON then
    exit;

  invLength := 1.0 / sqrt(d);
  s := s * invLength;
  x := x * invLength;
  y := y * invLength;
  z := z * invLength;

  Result := self;
end;

{-------------------------------------------------------------------------------
                             Operators
-------------------------------------------------------------------------------}
operator +(const q1, q2: TQuaternion): TQuaternion;
begin
  Result.s := q1.s + q2.s;
  Result.x := q1.x + q2.x;
  Result.y := q1.y + q2.y;
  Result.z := q1.z + q2.z;
end;

operator -(const q: TQuaternion): TQuaternion;
begin
  Result.s := -q.s;
  Result.x := -q.x;
  Result.y := -q.y;
  Result.z := -q.z;
end;

operator -(const q1, q2: TQuaternion): TQuaternion;
begin
  Result.s := q1.s - q2.s;
  Result.x := q1.x - q2.x;
  Result.y := q1.y - q2.y;
  Result.z := q1.z - q2.z;
end;

operator *(const q: TQuaternion; a: GLfloat): TQuaternion;
begin
  Result.s := a * q.s;
  Result.x := a * q.x;
  Result.y := a * q.y;
  Result.z := a * q.z
end;

operator *(a: GLfloat; const q: TQuaternion): TQuaternion;
begin
  Result.s := a * q.s;
  Result.x := a * q.x;
  Result.y := a * q.y;
  Result.z := a * q.z
end;

operator *(q1, q2: TQuaternion): TQuaternion;
var
  v1, v2, v3: TVector3;
  cross: TVector3;
  dot: GLfloat;
begin
  v1 := Vector3(q1.x, q1.y, q1.z);
  v2 := Vector3(q2.x, q2.y, q2.z);

  cross := v1.cross(v2);                 // v x v'
  dot := v1.dot(v2);                     // v . v'
  v3 := cross + q1.s*v2 + q2.s*v1;       // v x v + s v' + s' v
  Result := Quaternion(q1.s * q2.s - dot, v3.x, v3.y, v3.z);
end;

end.

