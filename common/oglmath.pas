///////////////////////////////////////////////////////////////////////////////
// Matrice.cpp
// ===========
// NxN Matrix Math classes
//
// The elements of the matrix are stored as column major order.
// | 0 2 |    | 0 3 6 |    |  0  4  8 12 |
// | 1 3 |    | 1 4 7 |    |  1  5  9 13 |
//            | 2 5 8 |    |  2  6 10 14 |
//                         |  3  7 11 15 |
//
// Dependencies: Vector2, Vector3, Vector3
//
//  AUTHOR: Song Ho Ahn (song.ahn@gmail.com)
// CREATED: 2005-06-24
// UPDATED: 2020-03-26
//
// Copyright (C) 2005 Song Ho Ahn
////////////////////////////////////////////////////////////////////////////////
// Quaternion.h
// ============
// Quaternion class represented as sum of a scalar and a vector(rotation axis)
// parts; [s, v] = s + (ix + jy + kz)
//
// When the quaternion is used for 3D rotation, initialize the quaternion with
// the half of the rotation angle (radian), because of double multiplication by
// its inverse, qpq'.
//
//  AUTHOR: Song Ho Ahn (song.ahn@gmail.com)
// CREATED: 2011-12-04
// UPDATED: 2016-05-17
//
// Copyright (C) 2011 Song Ho Ahn
///////////////////////////////////////////////////////////////////////////////
// Lazarus port by WP
///////////////////////////////////////////////////////////////////////////////

unit oglMath;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Math,
  gl,
  oglTypes;

type
  { ToglVector2f }
  ToglVector2f = record
  private
    function GetItem(AIndex: Integer): GLfloat;
    procedure SetItem(AIndex: Integer; AValue: GLfloat);
  public
    procedure Init(a, b: GLfloat);
    function Equal(a: ToglVector2f; Eps: GLfloat): Boolean;
    function Length: GLfloat;
    function Normalize: ToglVector2f;
    class operator +(a, b: ToglVector2f): ToglVector2f;
    class operator -(a, b: ToglVector2f): ToglVector2f;
    class operator -(a: ToglVector2f): ToglVector2f;
    class operator *(a: ToglVector2f; b: GLfloat): ToglVector2f;
    class operator *(a: GLfloat; b: ToglVector2f): ToglVector2f;
    class operator =(a, b: ToglVector2f): Boolean;
    class operator <>(a, b: ToglVector2f): Boolean;
    property Data[idx: Integer]: GLfloat read GetItem write SetItem; default;

    case Integer of
      0: (X, Y: GLfloat);
      1: (v: ToglArray2f);
  end;

  function Vector2f(a, b: Glfloat): ToglVector2f;
  function DotProduct(a, b: ToglVector2f): GLfloat;

type
  { ToglVector3f }
  ToglVector3f = record
  private
    function GetItem(AIndex: Integer): GLfloat;
    procedure SetItem(AIndex: Integer; AValue: GLfloat);
  public
    procedure Init(a, b, c: GLfloat);
    function Equal(a: ToglVector3f; Eps: GLfloat): Boolean;
    function Info(AFloatFormat: String = '0.000'): String;
    function Length: GLfloat;
    function Normalize: ToglVector3f;
    class operator +(a, b: ToglVector3f): ToglVector3f;
    class operator -(a, b: ToglVector3f): ToglVector3f;
    class operator -(a: ToglVector3f): ToglVector3f;
    class operator *(a: ToglVector3f; b: GLfloat): ToglVector3f;
    class operator *(a: GLfloat; b: ToglVector3f): ToglVector3f;
    class operator =(a, b: ToglVector3f): Boolean;    // Exact comparison, no EPS!
    class operator <>(a, b: ToglVector3f): Boolean;   // Exact comparison, no EPS!
    property Data[idx: Integer]: GLfloat read GetItem write SetItem; default;

    case Integer of
      0: (X, Y, Z: GLfloat);
      1: (v: ToglArray3f);
  end;

  function Vector3f(a, b, c: Glfloat): ToglVector3f;
  function CrossProduct(a, b: ToglVector3f): ToglVector3f;
  function DotProduct(a, b: ToglVector3f): GLfloat;
  function slerp(const AFrom, ATo: ToglVector3f; Alpha: GLfloat): ToglVector3f;

type
  { ToglVector4f }
  ToglVector4f = record
  private
    function GetItem(AIndex: Integer): GLfloat;
    procedure SetItem(AIndex: Integer; AValue: GLfloat);
  public
    procedure Init(a, b, c, d: GLfloat);
    function Equal(const a: ToglVector4f; Eps: GLfloat): Boolean;
    function Length: GLfloat;
    function Normalize: ToglVector4f;
    class operator +(a, b: ToglVector4f): ToglVector4f;
    class operator -(a, b: ToglVector4f): ToglVector4f;
    class operator -(a: ToglVector4f): ToglVector4f;
    class operator *(a: ToglVector4f; b: GLfloat): ToglVector4f;
    class operator *(a: GLfloat; b: ToglVector4f): ToglVector4f;
    class operator =(a, b: ToglVector4f): Boolean;    // Exact comparison, no EPS!
    class operator <>(a, b: ToglVector4f): Boolean;   // Exact comparison, no EPS!
    property Data[idx: Integer]: GLfloat read GetItem write SetItem; default;

    case Integer of
      0: (X, Y, Z, W: GLfloat);
      1: (v: ToglArray4f);
  end;

  function Vector4f(a, b, c, d: Glfloat): ToglVector4f;
  function DotProduct(a, b: ToglVector4f): GLfloat;

type
  { ToglMatrix2f }
  ToglMatrix2f = record
  private
    m: ToglArray4f;
    function GetItem(AIndex: Integer): GLfloat;
    procedure SetItem(AIndex: Integer; AValue: GLfloat);
  public
    procedure Init;  // with identity
    procedure Init(const src: ToglArray4f);
    procedure Init(m0, m1, m2, m3: GLfloat);
    function Equal(const AMatrix: ToglMatrix2f; Eps: GLfloat): Boolean;
    function GetAngle: GLfloat;
    function GetColumn(AIndex: Integer): ToglVector2f;
    function GetDeterminant: GLfloat;
    function GetRow(AIndex: Integer): ToglVector2f;
    function Identity: ToglMatrix2f;
    function Invert: ToglMatrix2f;
    function Transpose: ToglMatrix2f;
    class operator +(a,b : ToglMatrix2f): ToglMatrix2f;
    class operator -(a,b : ToglMatrix2f): ToglMatrix2f;
    class operator *(a: ToglMatrix2f; b: GLfloat): ToglMatrix2f;
    class operator *(a: GLfloat; b: ToglMatrix2f): ToglMatrix2f;
    class operator *(a: ToglMatrix2f; b: ToglVector2f): ToglVector2f;
    class operator *(a: ToglVector2f; b: ToglMatrix2f): ToglVector2f;
    class operator *(a,b : ToglMatrix2f): ToglMatrix2f;
    class operator =(a,b : ToglMatrix2f): boolean;     // exact compare, no epsilon!
    class operator <>(a,b : ToglMatrix2f): boolean;    // exact compare, no epsilon!
    property Item[idx: Integer]: GLfloat read GetItem write SetItem; default;
  end;

  function Matrix2f(a11, a21, a12, a22: GLfloat): ToglMatrix2f;

type
  { ToglMatrix3f }
  ToglMatrix3f = record
  private
    m: ToglArray9f;
    function GetItem(AIndex: Integer): GLfloat;
    procedure SetItem(AIndex: Integer; AValue: GLfloat);
  public
    procedure Init;  // with Identity
    procedure Init(const src: ToglArray9f);
    procedure Init(m0, m1, m2,           // first column
                   m3, m4, m5,           // second column
                   m6, m7, m8: GLfloat); // third column
    function Equal(const AMatrix: ToglMatrix3f; Eps: GLfloat): Boolean;
    function GetAngle: ToglVector3f;      // return (pitch, yaw, roll) in degrees
    function GetColumn(AIndex: Integer): ToglVector3f;
    function GetDeterminant: GLfloat;
    function GetRow(AIndex: Integer): ToglVector3f;
    function Identity: ToglMatrix3f;
    function Transpose: ToglMatrix3f;
    function Invert: ToglMatrix3f;
    class operator +(a,b : ToglMatrix3f): ToglMatrix3f;
    class operator -(a,b : ToglMatrix3f): ToglMatrix3f;
    class operator *(a: ToglMatrix3f; b: GLfloat): ToglMatrix3f;
    class operator *(a: GLfloat; b: ToglMatrix3f): ToglMatrix3f;
    class operator *(a: ToglMatrix3f; b: ToglVector3f): ToglVector3f;
    class operator *(a: ToglVector3f; b: ToglMatrix3f): ToglVector3f;
    class operator *(a,b : ToglMatrix3f): ToglMatrix3f;
    class operator =(a,b : ToglMatrix3f): boolean;     // exact compare, no epsilon!
    class operator <>(a,b : ToglMatrix3f): boolean;    // exact compare, no epsilon!
    property Item[idx: Integer]: GLfloat read GetItem write SetItem; default;
  end;

  function Matrix3f(a11, a21, a31, a12, a22, a32, a13, a23, a33: GLfloat): ToglMatrix3f;

  type
    { ToglMatrix4f }
    ToglMatrix4f = record
    private
      m: ToglArray16f;
      function GetCofactor(m0, m1, m2, m3, m4, m5, m6, m7, m8: GLfloat): GLfloat;
      function GetItem(AIndex: Integer): GLfloat;
      procedure SetItem(AIndex: Integer; AValue: GLfloat);
    public
      procedure Init;  // with Identity
      procedure Init(const src: ToglArray16f);
      procedure Init(m0,  m1,  m2,  m3,            // first column
                     m4,  m5,  m6,  m7,            // second column
                     m8,  m9,  m10, m11,           // third column
                     m12, m13, m14, m15: GLfloat); // fourth column
      function Equal(const AMatrix: ToglMatrix4f; Eps: GLfloat): Boolean;
      function Get: PGLfloat;
      function GetAngle: ToglVector3f;  // return (pitch, yaw, roll) in degrees
      function GetColumn(AIndex: Integer): ToglVector4f;
      function GetDeterminant: GLfloat;
      function GetRow(AIndex: Integer): ToglVector4f;
      function Identity: ToglMatrix4f;
      function Invert: ToglMatrix4f;
      function InvertAffine: ToglMatrix4f;
      function InvertEuclidean: ToglMatrix4f;
      function InvertGeneral: ToglMatrix4f;
      function InvertProjective: ToglMatrix4f;
      function LookAt(const Target: ToglVector3f): ToglMatrix4f;
      function LookAt(const Target, UpVec: ToglVector3f): ToglMatrix4f;
      function LookAt(tx, ty, tz: GLfloat): ToglMatrix4f;
      function LookAt(tx, ty, tz, ux, uy, uz: GLfloat): ToglMatrix4f;
      function Rotate(Angle: GLfloat; Axis: ToglVector3f): ToglMatrix4f;
      function Rotate(Angle, x, y, z: GLfloat): ToglMatrix4f;
      function RotateX(Angle: GLfloat): ToglMatrix4f;
      function RotateY(Angle: GLfloat): ToglMatrix4f;
      function RotateZ(Angle: GLfloat): ToglMatrix4f;
      function Scale(s: GLfloat): ToglMatrix4f;
      function Scale(x, y, z: GLfloat): ToglMatrix4f;
      procedure SetColumn(AIndex: Integer; AValue: ToglVector3f);
      procedure SetColumn(AIndex: Integer; AValue: ToglVector4f);
      procedure SetRow(AIndex: Integer; AValue: ToglVector3f);
      procedure SetRow(AIndex: Integer; AValue: ToglVector4f);
      function Translate(const v: ToglVector3f): ToglMatrix4f;
      function Translate(x, y, z: GLfloat): ToglMatrix4f;
      function Transpose: ToglMatrix4f;
      class operator +(a, b: ToglMatrix4f): ToglMatrix4f;
      class operator -(a, b: ToglMatrix4f): ToglMatrix4f;
      class operator *(a: ToglMatrix4f; b: GLfloat): ToglMatrix4f;
      class operator *(a: GLfloat; b: ToglMatrix4f): ToglMatrix4f;
      class operator *(a: ToglMatrix4f; b: ToglVector4f): ToglVector4f;
      class operator *(a: ToglVector4f; b: ToglMatrix4f): ToglVector4f;
      class operator *(a,b : ToglMatrix4f): ToglMatrix4f;
      class operator =(a,b : ToglMatrix4f): boolean;     // exact compare, no epsilon!
      class operator <>(a,b : ToglMatrix4f): boolean;    // exact compare, no epsilon!
      property Item[idx: Integer]: GLfloat read GetItem write SetItem; default;
    end;

    function Matrix4f(
      a11, a21, a31, a41,
      a12, a22, a32, a42,
      a13, a23, a33, a43,
      a14, a24, a34, a44: GLfloat): ToglMatrix4f;

type
  ToglQuaternion = record
  public
    S: GLfloat;        // Scalar part
    X, Y, Z: GLfloat;  // Vector part
    procedure Init(_s, _x, _y, _z: GLfloat);
    procedure Init(const Axis: ToglVector3f; Angle: GLfloat);  // Rotation axis and HALF of rotation (radians)

    function Conjugate: ToglQuaternion;
    function Equal(const q: ToglQuaternion; Eps: GLfloat): Boolean;
    function GetMatrix: ToglMatrix4f;
    function Info(ADecimalPlaces: Integer = 3): String;
    function Invert: ToglQuaternion;
    function Length: GLfloat;
    function Normalize: ToglQuaternion;

    class operator + (q1, q2: ToglQuaternion): ToglQuaternion;
    class operator - (q1, q2: ToglQuaternion): ToglQuaternion;
    class operator - (q: ToglQuaternion): ToglQuaternion;
    class operator * (q: ToglQuaternion; a: GLfloat): ToglQuaternion;
    class operator * (a: GLfloat; q: ToglQuaternion): ToglQuaternion;
    class operator * (q1, q2: ToglQuaternion): ToglQuaternion;
    class operator = (q1, q2: ToglQuaternion): Boolean;
    class operator <>(q1, q2: ToglQuaternion): Boolean;
  end;

  function Quaternion(s, x, y, z: GLfloat): ToglQuaternion;
  function Quaternion(const Axis: ToglVector3f; Angle: GLfloat): ToglQuaternion;
  function GetQuaternion(v1, v2: ToglVector3f): ToglQuaternion;
  function GetQuaternion(const Angles: ToglVector2f): ToglQuaternion;
  function GetQuaternion(const Angles: ToglVector3f): ToglQuaternion;

implementation

const
  DEG2RAD = pi / 180.0;
  RAD2DEG = 180.0 / pi;
  EPSILON = 0.00001;

procedure Swap(var a, b: GLfloat);
var
  tmp: GLfloat;
begin
  tmp := a;
  a := b;
  b := tmp;
end;


{===============================================================================
                              ToglVector2f
===============================================================================}
function Vector2f(a, b: GLfloat): ToglVector2f;
begin
  Result.Init(a, b);
end;

procedure ToglVector2f.Init(a, b: GLfloat);
begin
  v[0] := a;
  v[1] := b;
end;

function ToglVector2f.Equal(a: ToglVector2f; Eps: GLfloat): Boolean;
begin
  Result := SameValue(a.v[0], v[0], Eps) and SameValue(a.v[1], v[1], Eps);
end;

function ToglVector2f.GetItem(AIndex: Integer): GLfloat;
begin
  Result := v[AIndex];
end;

function ToglVector2f.Length: GLfloat;
begin
  Result := sqrt(sqr(v[0]) + sqr(v[1]));
end;

function ToglVector2f.Normalize: ToglVector2f;
var
  L2: GLfloat;
  invLength: GLfloat;
begin
  L2 := v[0]*v[0] + v[1]*v[1];
  if L2 < EPSILON then
    Exit(self);
  invLength := 1.0/sqrt(L2);
  v[0] := v[0] * invLength;
  v[1] := v[1] * invLength;
  Result := self;
end;

procedure ToglVector2f.SetItem(AIndex: Integer; AValue: GLfloat);
begin
  v[AIndex] := AValue;
end;

class operator ToglVector2f.+(a, b: ToglVector2f): ToglVector2f;
begin
  Result.Init(a.v[0] + b.v[0], a.v[1] + b.v[1]);
end;

class operator ToglVector2f.-(a, b: ToglVector2f): ToglVector2f;
begin
  Result.Init(a.v[0] - b.v[0], a.v[1] - b.v[1]);
end;

class operator ToglVector2f.-(a: ToglVector2f): ToglVector2f;
begin
  Result.Init(-a.v[0], -a.v[1]);
end;

class operator ToglVector2f.*(a: ToglVector2f; b: GLfloat): ToglVector2f;
begin
  Result.Init(a.v[0] * b, a.v[1] * b);
end;

class operator ToglVector2f.*(a: GLfloat; b: ToglVector2f): ToglVector2f;
begin
  Result.Init(a * b.v[0], a * b.v[1]);
end;

class operator ToglVector2f.=(a, b: ToglVector2f): Boolean;
begin
  Result := (a.v[0] = b.v[0]) and (a.v[1] = b.v[1]);
end;

class operator ToglVector2f.<>(a, b: ToglVector2f): Boolean;
begin
  Result := not (a = b);
end;

function DotProduct(a, b: ToglVector2f): GLfloat;
begin
  Result := a.v[0] * b.v[0] + a.v[1] * b.v[1];
end;


{===============================================================================
                              ToglVector3f
===============================================================================}
function Vector3f(a, b, c: GLfloat): ToglVector3f;
begin
  Result.v[0] := a;
  Result.v[1] := b;
  Result.v[2] := c;
end;

function CrossProduct(a, b: ToglVector3f): ToglVector3f;
begin
  Result.v[0] := a.v[1]*b.v[2] - a.v[2]*b.v[1];
  Result.v[1] := a.v[2]*b.v[0] - a.v[0]*b.v[2];
  Result.v[2] := a.v[0]*b.v[1] - a.v[1]*b.v[0];
end;

function DotProduct(a, b: ToglVector3f): GLfloat;
begin
  Result := a.v[0] * b.v[0] + a.v[1] * b.v[1] + a.v[2] * b.v[2];
end;

procedure ToglVector3f.Init(a, b, c: GLfloat);
begin
  v[0] := a;
  v[1] := b;
  v[2] := c;
end;

function ToglVector3f.Equal(a: ToglVector3f; Eps: GLfloat): Boolean;
begin
  Result := SameValue(a.v[0], v[0], Eps) and
            SameValue(a.v[1], v[1], Eps) and
            Samevalue(a.v[2], v[2], Eps);
end;

function ToglVector3f.GetItem(AIndex: Integer): GLfloat;
begin
  Result := v[AIndex];
end;

function ToglVector3f.Info(AFloatFormat: String = '0.000'): String;
begin
  Result := '(' + FormatFloat(AFloatFormat, x) + ', ' +
                  FormatFloat(AFloatFormat, y) + ', ' +
                  FormatFloat(AFloatFormat, z) + ')';
end;

function ToglVector3f.Length: Glfloat;
begin
  Result := sqrt(sqr(v[0]) + sqr(v[1]) + sqr(v[2]));
end;

function ToglVector3f.Normalize: ToglVector3f;
var
  L2: GLfloat;
  invLength: GLfloat;
begin
  L2 := sqr(v[0]) + sqr(v[1]) + sqr(v[2]);
  if L2 < EPSILON then
    exit(self);

  invLength := 1.0 / sqrt(L2);
  v[0] := v[0] * invLength;
  v[1] := v[1] * invLength;
  v[2] := v[2] * invLength;
  Result := self;
end;

procedure ToglVector3f.SetItem(AIndex: Integer; AValue: GLfloat);
begin
  v[AIndex] := AValue;
end;

class operator ToglVector3f.+(a, b: ToglVector3f): ToglVector3f;
begin
  Result.Init(a.v[0] + b.v[0], a.v[1] + b.v[1], a.v[2] + b.v[2]);
end;

class operator ToglVector3f.-(a, b: ToglVector3f): ToglVector3f;
begin
  Result.Init(a.v[0] - b.v[0], a.v[1] - b.v[1], a.v[2] - b.v[2]);
end;

class operator ToglVector3f.-(a: ToglVector3f): ToglVector3f;
begin
  Result.Init(-a.v[0], -a.v[1], -a.v[2]);
end;

class operator ToglVector3f.*(a: ToglVector3f; b: GLfloat): ToglVector3f;
begin
  Result.Init(a.v[0] * b, a.v[1] * b, a.v[2] * b);
end;

class operator ToglVector3f.*(a: GLfloat; b: ToglVector3f): ToglVector3f;
begin
  Result.Init(a * b.v[0], a * b.v[1], a * b.v[2]);
end;

class operator ToglVector3f.=(a, b: ToglVector3f): Boolean;
begin
  Result := (a.v[0] = b.v[0]) and (a.v[1] = b.v[1]) and (a.v[2] = b.v[2]);
end;

class operator ToglVector3f.<>(a, b: ToglVector3f): Boolean;
begin
  Result := not (a = b);
end;

{-------------------------------------------------------------------------------
  Spherical linear interpolation between 2 3D vectors
  alpha value should be 0 ~ 1
  NOTE: If angle between 2 vectors are 180, the rotation axis cannot be
  determined.
-------------------------------------------------------------------------------}
function slerp(const AFrom, ATo: ToglVector3f; Alpha: GLfloat): ToglVector3f;
var
  cosine, angle, invSine: GLfloat;
  scale1, scale2: GLfloat;
begin
  // determine the angle between
  //@@ FIXME: handle if angle is ~180 degree
  //float dot = from.dot(to);
  cosine := DotProduct(AFrom, ATo) / (AFrom.Length * ATo.Length);
  angle := arccos(cosine);
  invSine := 1.0 / sin(angle);

  // compute the scale factors
  scale1 := sin((1 - Alpha)*angle) * invSine;
  scale2 := sin(Alpha*angle) * invSine;

  // compute slerp-ed vector
  Result := scale1*AFrom + scale2*ATo;
end;

{===============================================================================
                              ToglVector4f
===============================================================================}
function Vector4f(a, b, c,d : GLfloat): ToglVector4f;
begin
  Result.v[0] := a;
  Result.v[1] := b;
  Result.v[2] := c;
  Result.v[3] := d;
end;

function DotProduct(a, b: ToglVector4f): GLfloat;
begin
  Result := a.v[0] * b.v[0] + a.v[1] * b.v[1] + a.v[2] * b.v[2] + a.v[3] * b.v[3];
end;

procedure ToglVector4f.Init(a, b, c, d: GLfloat);
begin
  v[0] := a;
  v[1] := b;
  v[2] := c;
  v[3] := d;
end;

function ToglVector4f.Equal(const a: ToglVector4f; Eps: GLfloat): Boolean;
begin
  Result := SameValue(a.v[0], v[0], Eps) and
            SameValue(a.v[1], v[1], Eps) and
            SameValue(a.v[2], v[2], Eps) and
            Samevalue(a.v[3], v[3], Eps);
end;

function ToglVector4f.GetItem(AIndex: Integer): GLfloat;
begin
  Result := v[AIndex];
end;

function ToglVector4f.Length: Glfloat;
begin
  Result := sqrt(sqr(v[0]) + sqr(v[1]) + sqr(v[2]) + sqr(v[3]));
end;

function ToglVector4f.Normalize: ToglVector4f;
var
  L2: GLfloat;
  invLength: GLfloat;
begin
  L2 := sqr(v[0]) + sqr(v[1]) + sqr(v[2]) + sqr(v[3]);
  if L2 < EPSILON then
    exit(self);

  invLength := 1.0 / sqrt(L2);
  v[0] := v[0] * invLength;
  v[1] := v[1] * invLength;
  v[2] := v[2] * invLength;
  v[3] := v[3] * invLength;
  Result := self;
end;

procedure ToglVector4f.SetItem(AIndex: Integer; AValue: GLfloat);
begin
  v[AIndex] := AValue;
end;

class operator ToglVector4f.+(a, b: ToglVector4f): ToglVector4f;
begin
  Result.Init(
    a.v[0] + b.v[0],
    a.v[1] + b.v[1],
    a.v[2] + b.v[2],
    a.v[3] + b.v[3]
  );
end;

class operator ToglVector4f.-(a, b: ToglVector4f): ToglVector4f;
begin
  Result.Init(
    a.v[0] - b.v[0],
    a.v[1] - b.v[1],
    a.v[2] - b.v[2],
    a.v[3] - b.v[3]
  );
end;

class operator ToglVector4f.-(a: ToglVector4f): ToglVector4f;
begin
  Result.Init(-a.v[0], -a.v[1], -a.v[2], -a.v[3]);
end;

class operator ToglVector4f.*(a: ToglVector4f; b: GLfloat): ToglVector4f;
begin
  Result.Init(a.v[0] * b, a.v[1] * b, a.v[2] * b, a.v[3] * b);
end;

class operator ToglVector4f.*(a: GLfloat; b: ToglVector4f): ToglVector4f;
begin
  Result.Init(a * b.v[0], a * b.v[1], a * b.v[2], a * b.v[3]);
end;

class operator ToglVector4f.=(a, b: ToglVector4f): Boolean;
begin
  Result := (a.v[0] = b.v[0]) and (a.v[1] = b.v[1]) and (a.v[2] = b.v[2]) and (a.v[3] = b.v[3]);
end;

class operator ToglVector4f.<>(a, b: ToglVector4f): Boolean;
begin
  Result := not (a = b);
end;


{===============================================================================
                        ToglMatrix2 (2x2 matrix)
===============================================================================}
function Matrix2f(a11, a21, a12, a22: GLfloat): ToglMatrix2f;
begin
  Result.Init(a11, a21, a12, a22);
end;

procedure ToglMatrix2f.Init;
begin
  Init(
    1.0, 0.0,   // 1st column
    0.0, 1.0    // 2nd column
  );
end;

procedure ToglMatrix2f.Init(const src: ToglArray4f);
begin
  Move(src[0], m[0], SizeOf(src));
end;

procedure ToglMatrix2f.Init(m0, m1, m2, m3: GLfloat);
begin
  m[0] := m0;
  m[1] := m1;
  m[2] := m2;
  m[3] := m3;
end;

function ToglMatrix2f.Equal(const AMatrix: ToglMatrix2f; Eps: GLfloat): Boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to 2 do
    if not SameValue(m[i], AMatrix.m[i], Eps) then
      exit;
  Result := true;
end;

// Retrieve rotation angle in degrees from rotation matrix, R
// R = | c -s |
//     | s  c |
// angle = atan(s / c)
function ToglMatrix2f.GetAngle: GLfloat;
begin
  // angle between -pi ~ +pi (-180 ~ +180)
  Result := RAD2DEG * arctan2(m[1], m[0]);
end;

function ToglMatrix2f.GetColumn(AIndex: Integer): ToglVector2f;
var
  j: Integer;
begin
  j := 2 * AIndex;
  Result := Vector2f(m[j], m[j+1]);
end;

// Return the determinant of 2x2 matrix
function ToglMatrix2f.GetDeterminant: GLfloat;
begin
  Result := m[0] * m[3] - m[1] * m[2];
end;

function ToglMatrix2f.GetItem(AIndex: Integer): GLfloat;
begin
  Result := m[AIndex];
end;

function ToglMatrix2f.GetRow(AIndex: Integer): ToglVector2f;
begin
  if AIndex = 0 then
    Result := Vector2f(m[0], m[2])
  else
    Result := Vector2f(m[1], m[3]);
end;

function ToglMatrix2f.Identity: ToglMatrix2f;
begin
  Init(
    1.0, 0.0,   // 1st column
    0.0, 1.0    // 2nd column
  );
  Result := Self;
end;

// Calculate the inverse of 2x2 matrix
// If cannot find inverse, set identity matrix
function ToglMatrix2f.Invert: ToglMatrix2f;
var
  tmp: GLfloat;
  det, invDet: GLfloat;
begin
  det := GetDeterminant;
  if abs(det) <= EPSILON then
    Init
  else
  begin
    tmp := m[0];    // Store the first element
    invDet := 1.0 / det;
    m[0] :=  invDet * m[3];
    m[1] := -invDet * m[1];
    m[2] := -invDet * m[2];
    m[3] :=  invDet * tmp;
  end;
  Result := self;
end;

procedure ToglMatrix2f.SetItem(AIndex: Integer; AValue: GLfloat);
begin
  m[AIndex] := AValue;
end;

// Transpose 2x2 matrix
function ToglMatrix2f.Transpose: ToglMatrix2f;
begin
  Swap(m[1],  m[2]);
  Result := Self;
end;

class operator ToglMatrix2f.+(a,b : ToglMatrix2f): ToglMatrix2f;
begin
  Result.m[0] := a.m[0] + b.m[0];
  Result.m[1] := a.m[1] + b.m[1];
  Result.m[2] := a.m[2] + b.m[2];
  Result.m[3] := a.m[3] + b.m[3];
end;

class operator ToglMatrix2f.-(a,b : ToglMatrix2f): ToglMatrix2f;
begin
  Result.m[0] := a.m[0] - b.m[0];
  Result.m[1] := a.m[1] - b.m[1];
  Result.m[2] := a.m[2] - b.m[2];
  Result.m[3] := a.m[3] - b.m[3];
end;

class operator ToglMatrix2f.*(a, b: ToglMatrix2f): ToglMatrix2f;
begin
  Result.m[0] := a.m[0] * b.m[0] + a.m[2] * b.m[1];
  Result.m[2] := a.m[0] * b.m[2] + a.m[2] * b.m[3];
  Result.m[1] := a.m[1] * b.m[0] + a.m[3] * b.m[1];
  Result.m[3] := a.m[1] * b.m[2] + a.m[3] * b.m[3];
end;

class operator ToglMatrix2f.*(a: GLfloat; b: ToglMatrix2f): ToglMatrix2f;
begin
  Result.m[0] := a * b.m[0];
  Result.m[1] := a * b.m[1];
  Result.m[2] := a * b.m[2];
  Result.m[3] := a * b.m[3];
end;

class operator ToglMatrix2f.*(a: ToglMatrix2f; b: GLfloat): ToglMatrix2f;
begin
  Result.m[0] := a.m[0] * b;
  Result.m[1] := a.m[1] * b;
  Result.m[2] := a.m[2] * b;
  Result.m[3] := a.m[3] * b;
end;

class operator ToglMatrix2f.*(a: ToglMatrix2f; b: ToglVector2f): ToglVector2f;
begin
  Result. Init(
    a.m[0] * b.v[0] + a.m[2] * b.v[1],
    a.m[1] * b.v[0] + a.m[3] * b.v[1]
  );
end;

class operator ToglMatrix2f.*(a: ToglVector2f; b: ToglMatrix2f): ToglVector2f;
begin
  Result.Init(
    a.v[0] * b.m[0] + a.v[1] * b.m[1],
    a.v[0] * b.m[2] + a.v[1] * b.m[3]
  );
end;

// exact compare, no epsilon!
class operator ToglMatrix2f.=(a,b : ToglMatrix2f): boolean;
begin
  Result := (a.m[0] = b.m[0]) and (a.m[1] = b.m[1]) and (a.m[2] = b.m[2]) and (a.m[3] = b.m[3]);
end;

// exact compare, no epsilon!
class operator ToglMatrix2f.<>(a,b : ToglMatrix2f): boolean;
begin
  Result := not (a = b);
end;


{===============================================================================
                        ToglMatrix3f (3x3 matrix)
===============================================================================}
function Matrix3f(a11, a21, a31, a12, a22, a32, a13, a23, a33: GLfloat): ToglMatrix3f;
begin
  Result.Init(
    a11, a21, a31,     // 1st column
    a12, a22, a32,     // 2nd column
    a13, a32, a33      // 3rd column
  );
end;

procedure ToglMatrix3f.Init;
begin
  Init(
    1.0, 0.0, 0.0,
    0.0, 1.0, 0.0,
    0.0, 0.0, 1.0
  );
end;

procedure ToglMatrix3f.Init(const src: ToglArray9f);
begin
  Move(src[0], m[0], SizeOf(ToglArray9f));
end;

procedure ToglMatrix3f.Init(m0, m1, m2, m3, m4, m5, m6, m7, m8: GLfloat);
begin
  m[0] := m0;
  m[1] := m1;
  m[2] := m2;
  m[3] := m3;
  m[4] := m4;
  m[5] := m5;
  m[6] := m6;
  m[7] := m7;
  m[8] := m8;
end;

function ToglMatrix3f.Equal(const AMatrix: ToglMatrix3f; Eps: GLfloat): Boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to 8 do
    if not SameValue(m[i], AMatrix.m[i], EPS) then
      exit;
  Result := true;
end;

// retrieve angles in degree from rotation matrix, M = Rx*Ry*Rz
//   Rx: rotation about X-axis, pitch
//   Ry: rotation about Y-axis, yaw(heading)
//   Rz: rotation about Z-axis, roll
//    Rx           Ry          Rz
// |1  0   0| | Cy  0 Sy| |Cz -Sz 0|   | CyCz        -CySz         Sy  |
// |0 Cx -Sx|*|  0  1  0|*|Sz  Cz 0| = | SxSyCz+CxSz -SxSySz+CxCz -SxCy|
// |0 Sx  Cx| |-Sy  0 Cy| | 0   0 1|   |-CxSyCz+SxSz  CxSySz+SxCz  CxCy|
//
// Pitch: atan(-m[7] / m[8]) = atan(SxCy/CxCy)
// Yaw  : asin(m[6]) = asin(Sy)
// Roll : atan(-m[3] / m[0]) = atan(SzCy/CzCy)
function ToglMatrix3f.GetAngle: ToglVector3f;
var
  pitch, roll, yaw: GLfloat;     // 3 angles
begin
  // find yaw (around y-axis) first
  // NOTE: arcsin() returns -90~+90, so correct the angle range -180~+180
  // using z value of forward vector
  yaw := RAD2DEG * arcsin(m[6]);
  if (m[8] < 0) then
  begin
    if (yaw >= 0) then
      yaw :=  180.0 - yaw
    else
      yaw := -180.0 - yaw;
  end;

  // find roll (around z-axis) and pitch (around x-axis)
  // if forward vector is (1,0,0) or (-1,0,0), then m[0]=m[4]=m[9]=m[10]=0
  if (m[0] > -EPSILON) and (m[0] < EPSILON) then
  begin
    roll := 0;  // Assume roll=0
    pitch := RAD2DEG * arctan2(m[1], m[4]);
  end else
  begin
    roll := RAD2DEG * arctan2(-m[3], m[0]);
    pitch := RAD2DEG * arctan2(-m[7], m[8]);
  end;

  Result.Init(pitch, yaw, roll);
end;

function ToglMatrix3f.GetColumn(AIndex: Integer): ToglVector3f;
var
  j: Integer;
begin
  j := (AIndex div 3) * 3;
  Result := Vector3f(m[j], m[j+1], m[j+2]);
end;

// Return the determinant of 3x3 matrix
function ToglMatrix3f.GetDeterminant: GLfloat;
begin
  Result := m[0] * (m[4] * m[8] - m[5] * m[7]) -
            m[1] * (m[3] * m[8] - m[5] * m[6]) +
            m[2] * (m[3] * m[7] - m[4] * m[6]);
end;

function ToglMatrix3f.GetItem(AIndex: Integer): GLfloat;
begin
  Result := m[AIndex];
end;

function ToglMatrix3f.GetRow(AIndex: Integer): ToglVector3f;
begin
  Result := Vector3f(m[AIndex], m[AIndex+3], m[AIndex+6]);
end;

function ToglMatrix3f.Identity: ToglMatrix3f;
begin
  Init(
    1.0, 0.0, 0.0,
    0.0, 1.0, 0.0,
    0.0, 0.0, 1.0
  );
  Result := Self;
end;

// Calculate the inverse of 3x3 matrix
// If cannot find inverse, set identity matrix
// M^-1 = adj(M) / det(M)
//        | m4m8-m5m7  m5m6-m3m8  m3m7-m4m6 |
//      = | m7m2-m8m1  m0m8-m2m6  m6m1-m7m0 | / det(M)
//        | m1m5-m2m4  m2m3-m0m5  m0m4-m1m3 |
function ToglMatrix3f.Invert: ToglMatrix3f;
var
  det, invDet: GLfloat;
  tmp: ToglArray9f;
  i: Integer;
begin
  tmp[0] := m[4] * m[8] - m[5] * m[7];
  tmp[1] := m[7] * m[2] - m[8] * m[1];
  tmp[2] := m[1] * m[5] - m[2] * m[4];
  tmp[3] := m[5] * m[6] - m[3] * m[8];
  tmp[4] := m[0] * m[8] - m[2] * m[6];
  tmp[5] := m[2] * m[3] - m[0] * m[5];
  tmp[6] := m[3] * m[7] - m[4] * m[6];
  tmp[7] := m[6] * m[1] - m[7] * m[0];
  tmp[8] := m[0] * m[4] - m[1] * m[3];

  det := m[0] * tmp[0] + m[1] * tmp[3] + m[2] * tmp[6];;
  if abs(det) <= EPSILON then
    Init
  else
  begin
    invDet := 1.0 / det;
    for i:=0 to 8 do
      m[i] :=  invDet * tmp[i];
  end;
  Result := self;
end;

procedure ToglMatrix3f.SetItem(AIndex: Integer; AValue: GLfloat);
begin
  m[AIndex] := AValue;
end;

// Transpose 3x3 matrix
function ToglMatrix3f.Transpose: ToglMatrix3f;
begin
  Swap(m[1], m[3]);
  Swap(m[2], m[6]);
  Swap(m[5], m[7]);
  Result := Self;
end;

class operator ToglMatrix3f.+(a,b : ToglMatrix3f): ToglMatrix3f;
var
  i: Integer;
begin
  for i := 0 to 8 do
    Result.m[i] := a.m[i] + b.m[i];
end;

class operator ToglMatrix3f.-(a,b : ToglMatrix3f): ToglMatrix3f;
var
  i: Integer;
begin
  for i := 0 to 8 do
    Result.m[i] := a.m[i] - b.m[i];
end;

class operator ToglMatrix3f.*(a, b: ToglMatrix3f): ToglMatrix3f;
begin
  // 1st column
  Result.m[0] := a.m[0] * b.m[0] + a.m[3] * b.m[1] + a.m[6] * b.m[2];
  Result.m[1] := a.m[1] * b.m[0] + a.m[4] * b.m[1] + a.m[7] * b.m[2];
  Result.m[2] := a.m[2] * b.m[0] + a.m[5] * b.m[1] + a.m[8] * b.m[2];

  // 2nd column
  Result.m[3] := a.m[0] * b.m[3] + a.m[3] * b.m[4] + a.m[6] * b.m[5];
  Result.m[4] := a.m[1] * b.m[3] + a.m[4] * b.m[4] + a.m[7] * b.m[5];
  Result.m[5] := a.m[2] * b.m[3] + a.m[5] * b.m[4] + a.m[8] * b.m[5];

  // 3rd column
  Result.m[6] := a.m[0] * b.m[6] + a.m[3] * b.m[7] + a.m[6] * b.m[8];
  Result.m[7] := a.m[1] * b.m[6] + a.m[4] * b.m[7] + a.m[7] * b.m[8];
  Result.m[8] := a.m[2] * b.m[6] + a.m[5] * b.m[7] + a.m[8] * b.m[8];
end;

class operator ToglMatrix3f.*(a: GLfloat; b: ToglMatrix3f): ToglMatrix3f;
var
  i: Integer;
begin
  for i := 0 to 8 do
    Result.m[i] := a * b.m[i];
end;

class operator ToglMatrix3f.*(a: ToglMatrix3f; b: GLfloat): ToglMatrix3f;
var
  i: Integer;
begin
  for i := 0 to 8 do
    Result.m[i] := a.m[i] * b;
end;

class operator ToglMatrix3f.*(a: ToglMatrix3f; b: ToglVector3f): ToglVector3f;
begin
  Result := Vector3f(
    a.m[0]*b.v[0] + a.m[3]*b.v[1] + a.m[6]*b.v[2],
    a.m[1]*b.v[0] + a.m[4]*b.v[1] + a.m[7]*b.v[2],
    a.m[2]*b.v[0] + a.m[5]*b.v[1] + a.m[8]*b.v[2]
  );
end;

class operator ToglMatrix3f.*(a: ToglVector3f; b: ToglMatrix3f): ToglVector3f;
begin
  Result := Vector3f(
    a.v[0]*b.m[0] + a.v[1]*b.m[1] + a.v[2]*b.m[2],
    a.v[0]*b.m[3] + a.v[1]*b.m[4] + a.v[2]*b.m[5],
    a.v[0]*b.m[6] + a.v[1]*b.m[7] + a.v[2]*b.m[8]
  );
end;

// exact compare, no epsilon!
class operator ToglMatrix3f.=(a,b : ToglMatrix3f): boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to 8 do
    if a.m[i] <> b.m[i] then
      exit;
  Result := true;
end;

// exact compare, no epsilon!
class operator ToglMatrix3f.<>(a,b : ToglMatrix3f): boolean;
begin
  Result := not (a = b);
end;


{===============================================================================
                        ToglMatrix4f (4x4 matrix)
===============================================================================}
function Matrix4f(
  a11, a21, a31, a41,
  a12, a22, a32, a42,
  a13, a23, a33, a43,
  a14, a24, a34, a44: GLfloat): ToglMatrix4f;
begin
  Result.Init(
    a11, a21, a31, a41,    // 1st column
    a12, a22, a32, a42,    // 2nd column
    a13, a23, a33, a43,    // 3rd column
    a14, a24, a34, a44     // 4th column
  );
end;

procedure ToglMatrix4f.Init;
begin
  Init(
    1.0, 0.0, 0.0, 0.0,
    0.0, 1.0, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    0.0, 0.0, 0.0, 1.0
    );
end;

procedure ToglMatrix4f.Init(const src: ToglArray16f);
begin
  Move(src[0], m[0], SizeOf(ToglArray16f));
end;

procedure ToglMatrix4f.Init(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15: GLfloat);
begin
  m[0] := m0;
  m[1] := m1;
  m[2] := m2;
  m[3] := m3;
  m[4] := m4;
  m[5] := m5;
  m[6] := m6;
  m[7] := m7;
  m[8] := m8;
  m[9] := m9;
  m[10] := m10;
  m[11] := m11;
  m[12] := m12;
  m[13] := m13;
  m[14] := m14;
  m[15] := m15;
end;

function ToglMatrix4f.Equal(const AMatrix: ToglMatrix4f; Eps: GLfloat): Boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to 15 do
    if not SameValue(m[i], AMatrix.m[i], EPS) then
      exit;
  Result := true;
end;

function ToglMatrix4f.Get: PGLfloat;
begin
  Result := @m[0];
end;

//------------------------------------------------------------------------------
// Retrieve angles in degree from rotation matrix, M = Rx*Ry*Rz
//   Rx: rotation about X-axis, pitch
//   Ry: rotation about Y-axis, yaw(heading)
//   Rz: rotation about Z-axis, roll
//    Rx           Ry          Rz
// |1  0   0| | Cy  0 Sy| |Cz -Sz 0|   | CyCz        -CySz         Sy  |
// |0 Cx -Sx|*|  0  1  0|*|Sz  Cz 0| = | SxSyCz+CxSz -SxSySz+CxCz -SxCy|
// |0 Sx  Cx| |-Sy  0 Cy| | 0   0 1|   |-CxSyCz+SxSz  CxSySz+SxCz  CxCy|
//
// Pitch: atan(-m[9] / m[10]) = atan(SxCy/CxCy)
// Yaw  : asin(m[8]) = asin(Sy)
// Roll : atan(-m[4] / m[0]) = atan(SzCy/CzCy)
///////////////////////////////////////////////////////////////////////////////
function ToglMatrix4f.GetAngle: ToglVector3f;
var
  pitch, yaw, roll: GLfloat;         // 3 angles
begin
  // Find yaw (around y-axis) first
  // NOTE: asin() returns -90~+90, so correct the angle range -180~+180
  // using z value of forward vector
  yaw := RAD2DEG * arcsin(m[8]);
  if (m[10] < 0) then
  begin
    if (yaw >= 0) then
      yaw := 180.0 - yaw
    else
      yaw := -180.0 - yaw;
  end;

  // Find roll (around z-axis) and pitch (around x-axis)
  // If forward vector is (1,0,0) or (-1,0,0), then m[0]=m[4]=m[9]=m[10]=0
  if (m[0] > -EPSILON) and (m[0] < EPSILON) then
  begin
    roll := 0;  // Assume roll=0
    pitch := RAD2DEG * arctan2(m[1], m[5]);
  end else
  begin
    roll := RAD2DEG * arctan2(-m[4], m[0]);
    pitch := RAD2DEG * arctan2(-m[9], m[10]);
  end;

  Result.Init(pitch, yaw, roll);
end;

function ToglMatrix4f.GetColumn(AIndex: Integer): ToglVector4f;
var
  j: Integer;
begin
  j := (AIndex div 4) * 4;
  Result := Vector4f(m[j], m[j+1], m[j+2], m[j+3]);
end;

// Compute cofactor of 3x3 minor matrix without sign
// input params are 9 elements of the minor matrix
// NOTE: The caller must know its sign.
function ToglMatrix4f.GetCofactor(m0, m1, m2, m3, m4, m5, m6, m7, m8: GLfloat): GLfloat;
begin
  Result := m0 * (m4 * m8 - m5 * m7) -
            m1 * (m3 * m8 - m5 * m6) +
            m2 * (m3 * m7 - m4 * m6);
end;

// Return the determinant of 4x4 matrix
function ToglMatrix4f.GetDeterminant: GLfloat;
begin
  Result :=
    m[0] * getCofactor(m[5],m[6],m[7], m[9],m[10],m[11], m[13],m[14],m[15]) -
    m[1] * getCofactor(m[4],m[6],m[7], m[8],m[10],m[11], m[12],m[14],m[15]) +
    m[2] * getCofactor(m[4],m[5],m[7], m[8],m[9], m[11], m[12],m[13],m[15]) -
    m[3] * getCofactor(m[4],m[5],m[6], m[8],m[9], m[10], m[12],m[13],m[14]
  );
end;

function ToglMatrix4f.GetItem(AIndex: Integer): GLfloat;
begin
  Result := m[AIndex];
end;

function ToglMatrix4f.GetRow(AIndex: Integer): ToglVector4f;
begin
  Result := Vector4f(m[AIndex], m[AIndex+4], m[AIndex+8], m[AIndex+12]);
end;

function ToglMatrix4f.Identity: ToglMatrix4f;
begin
  Init(
    1.0, 0.0, 0.0, 0.0,
    0.0, 1.0, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    0.0, 0.0, 0.0, 1.0
  );
  Result := Self;
end;

//------------------------------------------------------------------------------
// Invert 4x4 matrix
//------------------------------------------------------------------------------
function ToglMatrix4f.Invert: ToglMatrix4f;
begin
  // If the 4th row is [0,0,0,1] then it is an affine matrix and
  // it has no projective transformation.
  if (m[3] = 0) and (m[7] = 0) and (m[11] = 0) and (m[15] = 1) then
    Result := InvertAffine
  else
    Result := InvertGeneral;
    {
    // InvertProjective() is not optimized (slower than generic one)
    if not SameValue((m[0]*m[5] - m[1]*m[4], 0.0, EPSILON) then
      Result := InvertProjective()   // inverse using matrix partition
    else
      Result := InvertGeneral();     // generalized inverse
    }
end;

//------------------------------------------------------------------------------
// Compute the inverse of a 4x4 affine transformation matrix
//
// Affine transformations are generalizations of Euclidean transformations.
// Affine transformation includes translation, rotation, reflection, scaling,
// and shearing. Length and angle are NOT preserved.
// M = [ R | T ]
//     [ --+-- ]    (R denotes 3x3 rotation/scale/shear matrix)
//     [ 0 | 1 ]    (T denotes 1x3 translation matrix)
//
// y = M*x  ->  y = R*x + T  ->  x = R^-1*(y - T)  ->  x = R^-1*y - R^-1*T
//
//  [ R | T ]-1   [ R^-1 | -R^-1 * T ]
//  [ --+-- ]   = [ -----+---------- ]
//  [ 0 | 1 ]     [  0   +     1     ]
//------------------------------------------------------------------------------
function ToglMatrix4f.InvertAffine: ToglMatrix4f;
var
  r: ToglMatrix3f;
  x, y, z: GLfloat;
begin
  // R^-1
  r.Init(m[0],m[1],m[2], m[4],m[5],m[6], m[8],m[9],m[10]);
  r.Invert();
  m[0] := r[0];   m[1] := r[1];   m[2] := r[2];
  m[4] := r[3];   m[5] := r[4];   m[6] := r[5];
  m[8] := r[6];   m[9] := r[7];   m[10]:= r[8];

  // -R^-1 * T
  x := m[12];
  y := m[13];
  z := m[14];
  m[12] := -(r[0] * x + r[3] * y + r[6] * z);
  m[13] := -(r[1] * x + r[4] * y + r[7] * z);
  m[14] := -(r[2] * x + r[5] * y + r[8] * z);

  // last row should be unchanged (0,0,0,1)
  m[3] := 0.0;  m[7] := 0.0;  m[11] := 0.0;  m[12] := 1.0;

  Result := self;
end;

//------------------------------------------------------------------------------
// Compute the inverse of 4x4 Euclidean transformation matrix
//
// Euclidean transformation is translation, rotation, and reflection.
// With Euclidean transform, only the position and orientation of the object
// will be changed. Euclidean transform does not change the shape of an object
// (no scaling). Length and angle are reserved.
//
// Use inverseAffine() if the matrix has scale and shear transformation.
//
// M = [ R | T ]
//     [ --+-- ]    (R denotes 3x3 rotation/reflection matrix)
//     [ 0 | 1 ]    (T denotes 1x3 translation matrix)
//
// y = M*x  ->  y = R*x + T  ->  x = R^-1*(y - T)  ->  x = R^T*y - R^T*T
// (R is orthogonal,  R^-1 = R^T)
//
//  [ R | T ]-1    [ R^T | -R^T * T ]    (R denotes 3x3 rotation matrix)
//  [ --+-- ]   =  [ ----+--------- ]    (T denotes 1x3 translation)
//  [ 0 | 1 ]      [  0  |     1    ]    (R^T denotes R-transpose)
//------------------------------------------------------------------------------
function ToglMatrix4f.InvertEuclidean(): ToglMatrix4f;
var
  x, y, z: GLfloat;
begin
  // transpose 3x3 rotation matrix part
  // | R^T | 0 |
  // | ----+-- |
  // |  0  | 1 |
  Swap(m[1], m[4]);
  Swap(m[2], m[8]);
  Swap(m[6], m[9]);

  // compute translation part -R^T * T
  // | 0 | -R^T x |
  // | --+------- |
  // | 0 |   0    |
  x := m[12];
  y := m[13];
  z := m[14];
  m[12] := -(m[0] * x + m[4] * y + m[8] * z);
  m[13] := -(m[1] * x + m[5] * y + m[9] * z);
  m[14] := -(m[2] * x + m[6] * y + m[10]* z);

  // last row should be unchanged (0,0,0,1)

  Result := Self;
end;

//------------------------------------------------------------------------------
// Compute the inverse of a general 4x4 matrix using Cramer's Rule
// If cannot find inverse, return identity matrix
//   M^-1 = adj(M) / det(M)
//------------------------------------------------------------------------------
function ToglMatrix4f.InvertGeneral: ToglMatrix4f;
var
  det, invDet: GLfloat;
  cofactor0, cofactor1, cofactor2, cofactor3,
  cofactor4, cofactor5, cofactor6, cofactor7,
  cofactor8, cofactor9, cofactor10, cofactor11,
  cofactor12, cofactor13, cofactor14, cofactor15: GLfloat;
begin
  // Get cofactors of minor matrices
  cofactor0 := GetCofactor(m[5],m[6],m[7], m[9],m[10],m[11], m[13],m[14],m[15]);
  cofactor1 := GetCofactor(m[4],m[6],m[7], m[8],m[10],m[11], m[12],m[14],m[15]);
  cofactor2 := GetCofactor(m[4],m[5],m[7], m[8],m[9], m[11], m[12],m[13],m[15]);
  cofactor3 := GetCofactor(m[4],m[5],m[6], m[8],m[9], m[10], m[12],m[13],m[14]);

  // Get determinant
  det := m[0] * cofactor0 - m[1] * cofactor1 + m[2] * cofactor2 - m[3] * cofactor3;
  if SameValue(det, 0.0, EPSILON) then
  begin
    Init;  // Return identity
  end else
  begin
    // Get rest of cofactors for adj(M)
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

    // Build inverse matrix = adj(M) / det(M)
    // Adjugate of M is the transpose of the cofactor matrix of M
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
  end;

  Result := self;
end;

//------------------------------------------------------------------------------
// Inverse matrix using matrix partitioning (blockwise inverse)
// It devides a 4x4 matrix into 4 of 2x2 matrices. It works in case of where
// det(A) <> 0. If not, use the generic inverse method
// inverse formula.
// M = [ A | B ]    A, B, C, D are 2x2 matrix blocks
//     [ --+-- ]    det(M) = |A| * |D - ((C * A^-1) * B)|
//     [ C | D ]
//
// M^-1 = [ A' | B' ]   A' = A^-1 - (A^-1 * B) * C'
//        [ ---+--- ]   B' = (A^-1 * B) * -D'
//        [ C' | D' ]   C' = -D' * (C * A^-1)
//                      D' = (D - ((C * A^-1) * B))^-1
//
// NOTE: I wrap with () if it it used more than once.
//       The matrix is invertable even if det(A)=0, so must check det(A) before
//       calling this function, and use invertGeneric() instead.
//------------------------------------------------------------------------------
function ToglMatrix4f.InvertProjective: ToglMatrix4f;
var
  a, a1, b, b1, c, c1, d, d1, d2, ab, ca, cab, dcab: ToglMatrix2f;
  det: GLfloat;
begin
  // partition
  a.Init(m[0], m[1], m[4], m[5]);
  b.Init(m[8], m[9], m[12], m[13]);
  c.Init(m[2], m[3], m[6], m[7]);
  d.Init(m[10], m[11], m[14], m[15]);

  // pre-compute repeated parts
  a.Invert();        // A^-1
  ab := a * b;       // A^-1 * B
  ca := c * a;       // C * A^-1
  cab := ca * b;     // C * A^-1 * B
  dcab := d - cab;   // D - C * A^-1 * B

  // Check determinant if |D - C * A^-1 * B| = 0
  //NOTE: this function assumes det(A) is already checked.
  //      If |A|=0 then, this function cannot be used.
  det := dcab.m[0] * dcab.m[3] - dcab.m[1] * dcab.m[2];
  if SameValue(det, 0.0, EPSILON) then
  begin
    Init;  // return identity();
  end else
  begin
    // compute D' and -D'
    d1 := dcab;      //  (D - C * A^-1 * B)
    d1.Invert();     //  (D - C * A^-1 * B)^-1
    d2 := d1*(-1);   // -(D - C * A^-1 * B)^-1

    // compute C'
    c1 := d2 * ca;   // -D' * (C * A^-1)

    // compute B'
    b1 := ab * d2;   // (A^-1 * B) * -D'

    // compute A'
    a1 := a - (ab * c1); // A^-1 - (A^-1 * B) * C'

    // Assemble inverse matrix
    m[0] := a1[0];  m[4] := a1[2]; {|}  m[8] := b1[0];   m[12] := b1[2];
    m[1] := a1[1];  m[5] := a1[3]; {|}  m[9] := b1[1];   m[13] := b1[3];
    //------------------------------+-----------------------------------
    m[2] := c1[0];  m[6] := c1[2]; {|}  m[10] := d1[0];  m[14] := d1[2];
    m[3] := c1[1];  m[7] := c1[3]; {|}  m[11] := d1[1];  m[15] := d1[3];
  end;

  Result := self;
end;

//------------------------------------------------------------------------------
// Rotate matrix to face along the target direction
// NOTE: This function will clear the previous rotation and scale info and
// rebuild the matrix with the target vector. But it will keep the previous
// translation values.
// NOTE: It is for rotating object to look at the target, NOT for camera
//------------------------------------------------------------------------------
function ToglMatrix4f.LookAt(const Target: ToglVector3f): ToglMatrix4f;
var
  position, forward, up, left: ToglVector3f;
begin
  // Compute forward vector and normalize
  position := Vector3f(m[12], m[13], m[14]);
  forward := Target - position;
  forward.Normalize();

  // Compute temporal up vector
  // If forward vector is near Y-axis, use up vector (0,0,-1) or (0,0,1)
  if SameValue(forward.x, 0.0, EPSILON) and SameValue(forward.z, 0.0, EPSILON) then
  begin
    // forward vector is pointing +Y axis
    if (forward.y > 0) then
      up.Init(0, 0, -1)
    else
      // forward vector is pointing -Y axis
      up.Init(0, 0, 1);
  end else
    // Assume up vector is +Y axis
    up.Init(0, 1, 0);

  // compute left vector
  left := CrossProduct(up, forward);
  left.Normalize();

  // Re-compute up vector
  up := CrossProduct(forward, left);

  // NOTE: overwrite rotation and scale info of the current matrix
  SetColumn(0, left);
  SetColumn(1, up);
  SetColumn(2, forward);

  Result := self;
end;

function ToglMatrix4f.LookAt(const Target, UpVec: ToglVector3f): ToglMatrix4f;
var
  position, forward, left, up: ToglVector3f;
begin
  // Compute forward vector and normalize
  position := Vector3f(m[12], m[13], m[14]);
  forward := Target - position;
  forward.Normalize();

  // compute left vector
  left := CrossProduct(upVec, forward);
  left.Normalize();

  // Compute orthonormal up vector
  up := CrossProduct(forward, left);
  up.Normalize();

  // NOTE: overwrite rotation and scale info of the current matrix
  SetColumn(0, left);
  SetColumn(1, up);
  SetColumn(2, forward);

  Result := Self;
end;

function ToglMatrix4f.LookAt(tx, ty, tz: GLfloat): ToglMatrix4f;
begin
  Result := LookAt(Vector3f(tx, ty, tz));
end;

function ToglMatrix4f.LookAt(tx, ty, tz, ux, uy, uz: GLfloat): ToglMatrix4f;
begin
  Result := LookAt(Vector3f(tx, ty, tz), Vector3f(ux, uy, uz));
end;

//------------------------------------------------------------------------------
// Build a rotation matrix with given angle (degree) and rotation axis, then
// multiply it with this object
//------------------------------------------------------------------------------
function ToglMatrix4f.Rotate(Angle: GLfloat; Axis: ToglVector3f): ToglMatrix4f;
begin
  Result := Rotate(Angle, Axis.x, Axis.y, Axis.z);
end;

function ToglMatrix4f.Rotate(Angle, x, y, z: GLfloat): ToglMatrix4f;
var
  c, s: double;
  c1: GLFloat;
  m0,m1,m2, m4,m5,m6, m8,m9,m10, m12,m13,m14: GLfloat;
  r0,r1,r2, r4,r5,r6, r8,r9,r10: GLfloat;
begin
  SinCos(Angle * DEG2RAD, s, c);
  c1 := 1.0 - c;                // 1 - c
  m0 := m[0];   m4 := m[4];   m8 := m[8];   m12 := m[12];
  m1 := m[1];   m5 := m[5];   m9 := m[9];   m13 := m[13];
  m2 := m[2];   m6 := m[6];   m10:= m[10];  m14 := m[14];

  // Build rotation matrix
  r0 := x*x*c1 + c;
  r1 := x*y*c1 + z*s;
  r2 := x*z*c1 - y*s;
  r4 := x*y*c1 - z*s;
  r5 := y*y*c1 + c;
  r6 := y*z*c1 + x*s;
  r8 := x*z*c1 + y*s;
  r9 := y*z*c1 - x*s;
  r10:= z*z*c1 + c;

  // Multiply rotation matrix
  m[0] := r0*m0 + r4*m1 + r8*m2;
  m[1] := r1*m0 + r5*m1 + r9*m2;
  m[2] := r2*m0 + r6*m1 + r10*m2;
  m[4] := r0*m4 + r4*m5 + r8*m6;
  m[5] := r1*m4 + r5*m5 + r9*m6;
  m[6] := r2*m4 + r6*m5 + r10*m6;
  m[8] := r0*m8 + r4*m9 + r8*m10;
  m[9] := r1*m8 + r5*m9 + r9*m10;
  m[10]:= r2*m8 + r6*m9 + r10*m10;
  m[12]:= r0*m12+ r4*m13+ r8*m14;
  m[13]:= r1*m12+ r5*m13+ r9*m14;
  m[14]:= r2*m12+ r6*m13+ r10* m14;

  Result := self;
end;

function ToglMatrix4f.RotateX(Angle: GLfloat): ToglMatrix4f;
var
  c, s: Double;
  m1, m2, m5, m6, m9, m10, m13, m14: GLfloat;
begin
  SinCos(Angle*DEG2RAD, s, c);

  m1 := m[1];  m2 := m[2];
  m5 := m[5];  m6 := m[6];
  m9 := m[9];  m10:= m[10];
  m13:= m[13]; m14:= m[14];

  m[1] := m1*c - m2*s;
  m[2] := m1*s + m2*c;
  m[5] := m5*c - m6*s;
  m[6] := m5*s + m6*c;
  m[9] := m9*c - m10*s;
  m[10]:= m9*s + m10*c;
  m[13]:= m13*c - m14*s;
  m[14]:= m13*s + m14*c;

  Result := Self;
end;

function ToglMatrix4f.RotateY(Angle: GLfloat): ToglMatrix4f;
var
  c, s: Double;
  m0, m2, m4, m6, m8, m10, m12, m14: GLfloat;
begin
  SinCos(Angle*DEG2RAD, s, c);

  m0 := m[0];  m2 := m[2];
  m4 := m[4];  m6 := m[6];
  m8 := m[8];  m10:= m[10];
  m12:= m[12]; m14:= m[14];

  m[0] :=  m0*c  + m2*s;
  m[2] := -m0*s  + m2*c;
  m[4] :=  m4*c  + m6*s;
  m[6] := -m4*s  + m6*c;
  m[8] :=  m8*c  + m10*s;
  m[10]:= -m8*s  + m10*c;
  m[12]:=  m12*c + m14*s;
  m[14]:= -m12*s + m14*c;

  Result := self;
end;

function ToglMatrix4f.RotateZ(Angle: TGLfloat): ToglMatrix4f;
var
  c, s: Double;
  m0, m1, m4, m5, m8, m9, m12, m13: GLfloat;
begin
  SinCos(Angle*DEG2RAD, s, c);

  m0 := m[0];   m1 := m[1];
  m4 := m[4];   m5 := m[5];
  m8 := m[8];   m9 := m[9];
  m12:= m[12];  m13:= m[13];

  m[0] := m0*c  - m1*s;
  m[1] := m0*s  + m1*c;
  m[4] := m4*c  - m5*s;
  m[5] := m4*s  + m5*c;
  m[8] := m8*c  - m9*s;
  m[9] := m8*s  + m9*c;
  m[12]:= m12*c - m13*s;
  m[13]:= m12*s + m13*c;

  Result := self;
end;

//------------------------------------------------------------------------------
// Uniform scale
//------------------------------------------------------------------------------
function ToglMatrix4f.Scale(s: GLfloat): ToglMatrix4f;
begin
  Result := Scale(s, s, s);
end;

function ToglMatrix4f.Scale(x, y, z: GLfloat): ToglMatrix4f;
begin
  m[0] *= x;   m[4] *= x;   m[8] *= x;   m[12] *= x;
  m[1] *= y;   m[5] *= y;   m[9] *= y;   m[13] *= y;
  m[2] *= z;   m[6] *= z;   m[10]*= z;   m[14] *= z;
  Result := self;
end;

procedure ToglMatrix4f.SetItem(AIndex: Integer; AValue: GLfloat);
begin
  m[AIndex] := AValue;
end;

procedure ToglMatrix4f.SetColumn(AIndex: Integer; AValue: ToglVector3f);
var
  j: Integer;
begin
  j := AIndex * 4;
  m[j] := AValue.v[0];
  m[j+1] := AValue.v[1];
  m[j+2] := AValue.v[2];
end;

procedure ToglMatrix4f.SetColumn(AIndex: Integer; AValue: ToglVector4f);
var
  j: Integer;
begin
  j := AIndex * 4;
  m[j] := AValue.v[0];
  m[j+1] := AValue.v[1];
  m[j+2] := AValue.v[2];
  m[j+3] := AValue.v[3];
end;

procedure ToglMatrix4f.SetRow(AIndex: Integer; AValue: ToglVector3f);
begin
  m[AIndex] := AValue[0];
  m[AIndex+4] := AValue[1];
  m[AIndex+8] := AValue[2];
end;

procedure ToglMatrix4f.SetRow(AIndex: Integer; AValue: ToglVector4f);
begin
  m[AIndex] := AValue[0];
  m[AIndex+4] := AValue[1];
  m[AIndex+8] := AValue[2];
  m[AIndex+12] := AValue[3];
end;

//------------------------------------------------------------------------------
// Translate this matrix by (x, y, z)
//------------------------------------------------------------------------------
function ToglMatrix4f.Translate(const v: ToglVector3f): ToglMatrix4f;
begin
  Result := Translate(v.x, v.y, v.z);
end;

function ToglMatrix4f.Translate(x, y, z: GLfloat): ToglMatrix4f;
begin
  m[0] += m[3]*x;   m[4] += m[7]*x;   m[8] += m[11]*x;   m[12]+= m[15]* x;
  m[1] += m[3]*y;   m[5] += m[7]*y;   m[9] += m[11]*y;   m[13]+= m[15]* y;
  m[2] += m[3]*z;   m[6] += m[7]*z;   m[10]+= m[11]*z;   m[14]+= m[15]* z;
  Result := self;
end;

// Transpose 4x4 matrix
function ToglMatrix4f.Transpose: ToglMatrix4f;
begin
  Swap( m[1], m[4]);
  Swap( m[2], m[8]);
  Swap( m[3], m[12]);
  Swap( m[6], m[9]);
  Swap( m[7], m[13]);
  Swap(m[11], m[14]);
  Result := Self;
end;

class operator ToglMatrix4f.+(a,b : ToglMatrix4f): ToglMatrix4f;
var
  i: Integer;
begin
  for i := 0 to 15 do
    Result.m[i] := a.m[i] + b.m[i];
end;

class operator ToglMatrix4f.-(a,b : ToglMatrix4f): ToglMatrix4f;
var
  i: Integer;
begin
  for i := 0 to 15 do
    Result.m[i] := a.m[i] - b.m[i];
end;

class operator ToglMatrix4f.*(a, b: ToglMatrix4f): ToglMatrix4f;
begin
  // 1st column
  Result.m[0] := a.m[0]*b.m[0] + a.m[4]*b.m[1] + a.m[8]*b.m[2]  + a.m[12]*b.m[3];
  Result.m[1] := a.m[1]*b.m[0] + a.m[5]*b.m[1] + a.m[9]*b.m[2]  + a.m[13]*b.m[3];
  Result.m[2] := a.m[2]*b.m[0] + a.m[6]*b.m[1] + a.m[10]*b.m[2] + a.m[14]*b.m[3];
  Result.m[3] := a.m[3]*b.m[0] + a.m[7]*b.m[1] + a.m[11]*b.m[2] + a.m[15]*b.m[3];

  // 2nd column
  Result.m[4] := a.m[0]*b.m[4] + a.m[4]*b.m[5] + a.m[8]*b.m[6]  + a.m[12]*b.m[7];
  Result.m[5] := a.m[1]*b.m[4] + a.m[5]*b.m[5] + a.m[9]*b.m[6]  + a.m[13]*b.m[7];
  Result.m[6] := a.m[2]*b.m[4] + a.m[6]*b.m[5] + a.m[10]*b.m[6] + a.m[14]*b.m[7];
  Result.m[7] := a.m[3]*b.m[4] + a.m[7]*b.m[5] + a.m[11]*b.m[6] + a.m[15]*b.m[7];

  // 3rd column
  Result.m[8] := a.m[0]*b.m[8] + a.m[4]*b.m[9] + a.m[8]*b.m[10]  + a.m[12]*b.m[11];
  Result.m[9] := a.m[1]*b.m[8] + a.m[5]*b.m[9] + a.m[9]*b.m[10]  + a.m[13]*b.m[11];
  Result.m[10]:= a.m[2]*b.m[8] + a.m[6]*b.m[9] + a.m[10]*b.m[10] + a.m[14]*b.m[11];
  Result.m[11]:= a.m[3]*b.m[8] + a.m[7]*b.m[9] + a.m[11]*b.m[10] + a.m[15]*b.m[11];

  // 4th column
  Result.m[12]:= a.m[0]*b.m[12] + a.m[4]*b.m[13] + a.m[8]*b.m[14]  + a.m[12]*b.m[15];
  Result.m[13]:= a.m[1]*b.m[12] + a.m[5]*b.m[13] + a.m[9]*b.m[14]  + a.m[13]*b.m[15];;
  Result.m[14]:= a.m[2]*b.m[12] + a.m[6]*b.m[13] + a.m[10]*b.m[14] + a.m[14]*b.m[15];;
  Result.m[15]:= a.m[3]*b.m[12] + a.m[7]*b.m[13] + a.m[11]*b.m[14] + a.m[15]*b.m[15];;
end;

class operator ToglMatrix4f.*(a: GLfloat; b: ToglMatrix4f): ToglMatrix4f;
var
  i: Integer;
begin
  for i := 0 to 15 do
    Result.m[i] := a * b.m[i];
end;

class operator ToglMatrix4f.*(a: ToglMatrix4f; b: GLfloat): ToglMatrix4f;
var
  i: Integer;
begin
  for i := 0 to 15 do
    Result.m[i] := a.m[i] * b;
end;

class operator ToglMatrix4f.*(a: ToglMatrix4f; b: ToglVector4f): ToglVector4f;
begin
  Result := Vector4f(
    a.m[0]*b.v[0] + a.m[4]*b.v[1] +  a.m[8]*b.v[2] + a.m[12]*b.v[3],
    a.m[1]*b.v[0] + a.m[5]*b.v[1] +  a.m[9]*b.v[2] + a.m[13]*b.v[3],
    a.m[2]*b.v[0] + a.m[6]*b.v[1] + a.m[10]*b.v[2] + a.m[14]*b.v[3],
    a.m[3]*b.v[0] + a.m[7]*b.v[1] + a.m[11]*b.v[2] + a.m[15]*b.v[3]
  );
end;

class operator ToglMatrix4f.*(a: ToglVector4f; b: ToglMatrix4f): ToglVector4f;
begin
  Result := Vector4f(
    a.v[0]*b.m[0]  + a.v[1]*b.m[1]  + a.v[2]*b.m[2]  + a.v[3]*b.m[3],
    a.v[0]*b.m[4]  + a.v[1]*b.m[5]  + a.v[2]*b.m[6]  + a.v[3]*b.m[7],
    a.v[0]*b.m[8]  + a.v[1]*b.m[9]  + a.v[2]*b.m[10] + a.v[3]*b.m[11],
    a.v[0]*b.m[12] + a.v[1]*b.m[13] + a.v[2]*b.m[14] + a.v[3]*b.m[15]
  );
end;

// exact compare, no epsilon!
class operator ToglMatrix4f.=(a,b : ToglMatrix4f): boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to 15 do
    if a.m[i] <> b.m[i] then
      exit;
  Result := true;
end;

// exact compare, no epsilon!
class operator ToglMatrix4f.<>(a,b : ToglMatrix4f): boolean;
begin
  Result := not (a = b);
end;


//==============================================================================
//                               ToglQuaternion
//==============================================================================
function Quaternion(s, x, y, z: GLfloat): ToglQuaternion;
begin
  Result.S := s;
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Quaternion(const Axis: ToglVector3f; Angle: GLfloat): ToglQuaternion;
begin
  Result.Init(Axis, Angle);
end;

procedure ToglQuaternion.Init(_s, _x, _y, _z: GLfloat);
begin
  S := _s;
  X := _x;
  Y := _y;
  Z := _z;
end;

procedure ToglQuaternion.Init(const Axis: ToglVector3f; Angle: GLfloat);
var
  v: ToglVector3f;
  sinAngle, cosAngle: double;
begin
  v := Axis;
  v.Normalize();                  // convert to unit vector

  SinCos(Angle, sinAngle, cosAngle);  // Angle is radian!

  S := cosAngle;
  x := v.X * sinAngle;
  y := v.Y * sinAngle;
  z := v.Z * sinAngle;
end;

function ToglQuaternion.Conjugate: ToglQuaternion;
begin
  X := -X;
  Y := -Y;
  Z := -Z;
  Result := Self;
end;

function ToglQuaternion.Equal(const q: ToglQuaternion; Eps: GLfloat): Boolean;
begin
  Result := SameValue(S, q.S, Eps) and
            SameValue(X, q.X, Eps) and
            SameValue(Y, q.Y, Eps) and
            SameValue(Z, q.Z, Eps);
end;

function ToglQuaternion.GetMatrix: ToglMatrix4f;
var
  x2, y2, z2, xx2, xy2, xz2, yy2, yz2, zz2, sx2, sy2, sz2: GLfloat;
begin
  // NOTE: We assume that the quaternion is unit length

  // Compute common values
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

  // Build 4x4 matrix (column-major) and return
  Result := Matrix4f(
    1 - (yy2 + zz2),  xy2 + sz2,        xz2 - sy2,        0,
    xy2 - sz2,        1 - (xx2 + zz2),  yz2 + sx2,        0,
    xz2 + sy2,        yz2 - sx2,        1 - (xx2 + yy2),  0,
    0,                0,                0,                1
  );

  // for non-unit quaternion
  // ss+xx-yy-zz, 2xy+2sz,     2xz-2sy,     0
  // 2xy-2sz,     ss-xx+yy-zz, 2yz-2sx,     0
  // 2xz+2sy,     2yz+2sx,     ss-xx-yy+zz, 0
  // 0,           0,           0,           1
end;

function ToglQuaternion.Info(ADecimalPlaces: Integer = 3): String;
var
  decs: Integer;
begin
  decs := ADecimalPlaces;
  Result := Format('(S=%*.f, X=%*.f, Y=%*.Y, Z=%*.f', [decs, S, decs, X, decs, Y, decs, Z]);
end;

function ToglQuaternion.Invert: ToglQuaternion;
var
  d: GLfloat;
  q: ToglQuaternion;
begin
  d := S*S + X*X + Y*Y + Z*Z;
  if (d > EPSILON) then
  begin
    q := Self;
    Self := q.Conjugate * (1.0 / d);   // q* / |q||q|
  end;
  Result := Self;
end;

function ToglQuaternion.Length: GLfloat;
begin
  Result := sqrt(S*S + X*X + Y*Y + Z*Z);
end;

function ToglQuaternion.Normalize: ToglQuaternion;
var
  d: GLfloat;
  invLength: GLfloat;
begin
  d := S*S + X*X + Y*Y + Z*Z;
  if (d > EPSILON) then
  begin
    invLength := 1.0 / sqrt(d);
    S := S * invLength;
    X := X * invLength;
    Y := Y * invLength;
    Z := Z * invLength;
  end;
  Result := Self;
end;

class operator ToglQuaternion.+(q1, q2: ToglQuaternion): ToglQuaternion;
begin
  Result := Quaternion(q1.S + q2.S, q1.X + q2.X, q1.Y + q2.Y, q1.Z + q2.Z);
end;

class operator ToglQuaternion.-(q1, q2: ToglQuaternion): ToglQuaternion;
begin
  Result := Quaternion(q1.S - q2.S, q1.X - q2.X, q1.Y - q2.Y, q1.Z - q2.Z);
end;

class operator ToglQuaternion.-(q: ToglQuaternion): ToglQuaternion;
begin
  Result := Quaternion(-q.S, -q.X, -q.Y, -q.Z);
end;

class operator ToglQuaternion.*(q: ToglQuaternion; a: GLfloat): ToglQuaternion;
begin
  Result := Quaternion(q.S*a, q.X*a, q.Y*a, q.Z*a);
end;

class operator ToglQuaternion.*(a: GLfloat; q: ToglQuaternion): ToglQuaternion;
begin
  Result := Quaternion(a*q.S, a*q.X, a*q.Y, a*q.Z);
end;

class operator ToglQuaternion.*(q1, q2: ToglQuaternion): ToglQuaternion;
var
  v1, v2, v3, cross: ToglVector3f;
  dot: GLfloat;
begin
  v1 := Vector3f(q1.X, q1.Y, q1.Z);
  v2 := Vector3f(q2.X, q2.Y, q2.Z);

  cross := CrossProduct(v1, v2);                 // v x v'
  dot := DotProduct(v1, v2);                     // v . v'
  v3 := cross + (q1.S * v2) + (q2.S * v1);       // v x v' + s v' + s' v

  Result := Quaternion(q1.S*q2.S - dot, v3.X, v3.Y, v3.Z);
end;

class operator ToglQuaternion.=(q1, q2: ToglQuaternion): boolean;
begin
  Result := (q1.S = q2.S) and (q1.X = q2.X) and (q1.Y = q2.Y) and (q1.Z = q2.Z);
end;

class operator ToglQuaternion.<>(q1, q2: ToglQuaternion): boolean;
begin
  Result := not (q1 = q2);
end;

//------------------------------------------------------------------------------

function GetQuaternion(v1, v2: ToglVector3f): ToglQuaternion;
const
  HALF_PI = pi * 0.5;
  EPSILON = 0.001;
var
  v: ToglVector3f;
  u1, u2: ToglVector3f;
  angle: GLfloat;
begin
  // If two vectors are equal return the vector with 0 rotation
  if v1.Equal(v2, EPSILON) then
  begin
    Result.Init(v1, 0);
    exit;
  end;

  // If two vectors are opposite return a perpendicular vector with 180 angle
  if v1.Equal(-v2, EPSILON) then
  begin
    if SameValue(v1.X, 0.0, EPSILON) then        // if x ~= 0
      v.Init(1, 0, 0)
    else
    if SameValue(v1.Y, 0.0, EPSILON) then        // if y ~= 0
      v.Init(0, 1, 0)
    else
    if SameValue(v1.Z, 0.0, EPSILON) then        // if z ~= 0
      v.Init(0, 0, 1);
    Result.Init(v, HALF_PI);
  end;

  u1 := v1;                              // convert to normal vector
  u2 := v2;
  u1.Normalize();
  u2.Normalize();

  v := CrossProduct(u1, u2);             // compute rotation axis
  angle := arccos(DotProduct(u1, u2));   // rotation angle
  Result.Init(v, angle*0.5);             // return half angle for quaternion
end;

function GetQuaternion(const Angles: ToglVector2f): ToglQuaternion;
var
  qx, qy: ToglQuaternion;
begin
  qx.Init(Vector3f(1,0,0), Angles.x);   // rotate along X
  qy.Init(Vector3f(0,1,0), Angles.y);   // rotate along Y
  Result := qx * qy;               // order: y->x
end;

function GetQuaternion(const Angles: ToglVector3f): ToglQuaternion;
var
  qx, qy, qz: ToglQuaternion;
begin
  qx.Init(Vector3f(1,0,0), Angles.x);   // rotate along X
  qy.Init(Vector3f(0,1,0), Angles.y);   // rotate along Y
  qz.Init(Vector3f(0,0,1), Angles.z);   // rotate along Z
  Result := qx * qy * qz;    // order: z->y->x
end;


end.

