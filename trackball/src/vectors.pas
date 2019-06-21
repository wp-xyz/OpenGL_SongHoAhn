unit vectors;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, gl;

type
  TVector2 = record
  public
    x, y: GLfloat;
    function Distance(V: TVector2): GLfloat;
    function Dot(V: TVector2): GLfloat;
    function Equal(V: TVector2; Epsilon: GLFloat): Boolean;
    function Length: GLfloat;
    function Normalize: TVector2;
  end;

  function Vector2(x, y: GLfloat): TVector2;

type
  TVector3 = record
  public
    x, y, z: GLfloat;
    function Cross(V: TVector3): TVector3;
    function Distance(V: TVector3): GLfloat;
    function Dot(V: TVector3): GLfloat;
    function Equal(V: TVector3; Epsilon: GLFloat): Boolean;
    function Info(AFloatFormat: String = '0.000'): String;
    function Length: GLfloat;
    function Normalize: TVector3;
  end;

  function Vector3(x, y, z: GLfloat): TVector3;
  function slerp(const AFrom, ATo: TVector3; Alpha: GLfloat): TVector3;

  operator +(const a, b: TVector3): TVector3;
  operator -(const a, b: TVector3): TVector3;
  operator -(const v: TVector3): TVector3;
  operator *(const v: TVector3; const a: GLfloat): TVector3;
  operator *(const a: GLfloat; const v: TVector3): TVector3;
  operator =(const a, b: TVector3): Boolean;

type
  TVector4 = record
  public
    x, y, z, w: GLfloat;
    function Distance(V: TVector4): GLfloat;
    function Dot(V: TVector4): GLfloat;
    function Equal(V: TVector4; Epsilon: GLFloat): Boolean;
    function Length: GLfloat;
    function Normalize: TVector4;
  end;

  function Vector4(x, y, z, w: GLfloat): TVector4;

  procedure Exchange(var a, b: GLfloat);


implementation

uses
  Math;

procedure Exchange(var a, b: GLfloat);
var
  c: GLfloat;
begin
  c := a;
  a := b;
  b := c;
end;


{===============================================================================
                                Vector2
===============================================================================}

function Vector2(x, y: GLfloat): TVector2;
begin
  Result.x := x;
  Result.y := y;
end;

function TVector2.Distance(V: TVector2): GLfloat;
begin
  Result := sqrt(sqr(x - V.x) + sqr(y - V.y));
end;

function TVector2.Dot(V: TVector2): GLfloat;
begin
  Result := x * V.x + y * V.y;
end;

function TVector2.Equal(V: TVector2; Epsilon: GLFloat): Boolean;
begin
  Result := (abs(x - V.x) < Epsilon) and
            (abs(y - V.y) < Epsilon);
end;

function TVector2.Length: GLfloat;
begin
  Result := sqrt(x * x + y * y);
end;

function TVector2.Normalize: TVector2;
const
  EPSILON = 0.000001;
var
  xxyy: GLfloat;
  invLength: GLfloat;
begin
  xxyy := x*x + y*y;
  if xxyy < EPSILON then
    exit;
  invLength := 1.0 / sqrt(xxyy);
  x := x * invLength;
  y := y * invLength;
  Result := self;
end;


{===============================================================================
                                TVector3
===============================================================================}

function TVector3.Cross(V: TVector3): TVector3;
begin
  Result := Vector3(
    y*V.z - z*V.y,
    z*V.x - x*V.z,
    x*V.y - y*V.x
  );
end;

function TVector3.Distance(V: TVector3): GLfloat;
begin
  Result := sqrt(sqr(x - V.x) + sqr(y - V.y) + sqr(z - V.z));
end;

function TVector3.Dot(V: TVector3): GLfloat;
begin
  Result := x*V.x + y*V.y + z*V.z;
end;

function TVector3.Equal(V: TVector3; Epsilon: GLFloat): Boolean;
begin
  Result := (abs(x - V.x) < Epsilon) and
            (abs(y - V.y) < Epsilon) and
            (abs(z - V.z) < Epsilon);
end;

function TVector3.Info(AFloatFormat: String = '0.000'): String;
begin
  Result := '(' + FormatFloat(AFloatFormat, x) + ', ' +
                  FormatFloat(AFloatFormat, y) + ', ' +
                  FormatFloat(AFloatFormat, z) + ')';
end;

function TVector3.Length: GLfloat;
begin
  Result := sqrt(x*x + y*y + z*z);
end;

function TVector3.Normalize: TVector3;
const
  EPSILON = 0.000001;
var
  xxyyzz: GLfloat;
  invLength: GLfloat;
begin
  xxyyzz := x*x + y*y + z*z;
  if xxyyzz < EPSILON then
    exit;
  invLength := 1.0 / sqrt(xxyyzz);
  x := x * invLength;
  y := y * invLength;
  z := z * invLength;
  Result := self;
end;

function Vector3(x, y, z: GLfloat): TVector3;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

{-------------------------------------------------------------------------------
  Spherical linear interpolation between 2 3D vectors
  alpha value should be 0 ~ 1
  NOTE: If angle between 2 vectors are 180, the rotation axis cannot be
  determined.
-------------------------------------------------------------------------------}
function slerp(const AFrom, ATo: TVector3; Alpha: GLfloat): TVector3;
var
  cosine, angle, invSine: GLfloat;
  scale1, scale2: GLfloat;
begin
  // determine the angle between
  //@@ FIXME: handle if angle is ~180 degree
  //float dot = from.dot(to);
  cosine := AFrom.dot(ATo) / (AFrom.Length * ATo.Length);
  angle := arccos(cosine);
  invSine := 1.0 / sin(angle);

  // compute the scale factors
  scale1 := sin((1 - Alpha)*angle) * invSine;
  scale2 := sin(Alpha*angle) * invSine;

  // compute slerp-ed vector
  Result := scale1*AFrom + scale2*ATo;
end;

{-------------------------------------------------------------------------------
   Operators
-------------------------------------------------------------------------------}
operator +(const a, b: TVector3): TVector3;
begin
  Result := Vector3(a.x + b.x, a.y + b.y, a.z + b.z);
end;

operator -(const a, b: TVector3): TVector3;
begin
  Result := Vector3(a.x - b.x, a.y - b.y, a.z - b.z);
end;

operator -(const v: TVector3): TVector3;
begin
  Result := Vector3(-v.x, -v.y, -v.z);
end;

operator *(const v: TVector3; const a: GLfloat): TVector3;
begin
  Result := Vector3(v.x*a, v.y*a, v.z*a);
end;

operator *(const a: GLfloat; const v: TVector3): TVector3;
begin
  Result := Vector3(v.x*a, v.y*a, v.z*a);
end;

operator =(const a, b: TVector3): boolean;
begin
  Result := (a.x = b.x) and (a.y = b.y) and (a.z = b.z);
end;


{===============================================================================
                                TVector3
===============================================================================}

function Vector4(x, y, z, w: GLfloat): TVector4;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
  Result.w := w;
end;

function TVector4.Distance(V: TVector4): GLfloat;
begin
  Result := sqrt(sqr(x - V.x) + sqr(y - V.y) + sqr(z - V.z) + sqr(w - V.w));
end;

function TVector4.Dot(V: TVector4): GLfloat;
begin
  Result := x * V.x + y * V.y + z * V.z + w * V.w;
end;

function TVector4.Equal(V: TVector4; Epsilon: GLFloat): Boolean;
begin
  Result := (abs(x - V.x) < Epsilon) and
            (abs(y - V.y) < Epsilon) and
            (abs(z - V.z) < Epsilon) and
            (abs(w - V.w) < Epsilon);
end;

function TVector4.Length: GLfloat;
begin
  Result := sqrt(x*x + y*y + z*z + w*w);
end;

function TVector4.Normalize: TVector4;
const
  EPSILON = 0.000001;
var
  xxyyzzww: GLfloat;
  invLength: GLfloat;
begin
  xxyyzzww := x*x + y*y + z*z + w*w;
  if xxyyzzww < EPSILON then
    exit;
  invLength := 1.0 / sqrt(xxyyzzww);
  x := x * invLength;
  y := y * invLength;
  z := z * invLength;
  w := w * invLength;
  Result := self;
end;

end.

