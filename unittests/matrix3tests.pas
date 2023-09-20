unit matrix3tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TVector3fTests = class(TTestCase)
  published
    procedure ElementTest;
    procedure EqualTest;
    procedure NotEqualTest;
    procedure AddTest;
    procedure SubtractTest;
    procedure ProductTest_ScalarLeft;
    procedure ProductTest_ScalarRight;
    procedure DotProductTest;
    procedure CrossProductTest;
    procedure LengthTest;
  end;

  TMatrix3fTests= class(TTestCase)
  published
    procedure EqualTest;
    procedure NotEqualTest;
    procedure AddTest;
    procedure SubtractTest;
    procedure ProductTest_ScalarLeft;
    procedure ProductTest_ScalarRight;
    procedure ProductTest_VectorLeft;
    procedure ProductTest_VectorRight;
    procedure ProductTest_MatrixMatrix;
    procedure DeterminantTest;
    procedure InvertTest;
    procedure TransposeTest;
    {
    procedure AngleTest;
    }
  end;

implementation

uses
  Math, gl, oglTypes, oglMath;

{ TVector3fTests }

procedure TVector3fTests.EqualTest;
var
  v1, v2, v3: ToglVector3f;
begin
  v1 := Vector3f(1, 2, 3);
  v2 := Vector3f(1, 2, 3);
  v3 := Vector3f(1, 0, 3);
  AssertEquals('Equal vector3f mismatch x', true, v1.X=v2.X);
  AssertEquals('Equal vector3f mismatch y', true, v1.Y=v2.Y);
  AssertEquals('Equal vector3f mismatch z', true, v1.Z=v2.Z);
  AssertEquals('Equal vector3f mismatch (equal)', true, v1=v2);
  AssertEquals('Equal vector3f mismatch (different)', false, v1=v3);
end;

procedure TVector3fTests.NotEqualTest;
var
  v1, v2: ToglVector3f;
begin
  v1 := Vector3f(1, 2, 3);
  v2 := Vector3f(1, 3, 3);
  AssertEquals('Not equal vector3f mismatch (different)', true, v1<>v2);
  AssertEquals('Not equal vector3f mismatch (same)', false, v1<>v1);
end;

procedure TVector3fTests.AddTest;
var
  v1, v2: ToglVector3f;
  expected: ToglVector3f;
begin
  v1 := Vector3f(1, 2, 3);
  v2 := Vector3f(1, 3, -3);
  expected := Vector3f(2, 5, 0);
  AssertEquals('Add vector3f mismatch', true, v1+v2 = expected);
end;

procedure TVector3fTests.SubtractTest;
var
  v1, v2: ToglVector3f;
  actual, expected: ToglVector3f;
begin
  v1 := Vector3f(1, 2, 3);
  v2 := Vector3f(1, 3, -3);
  actual := v1 - v2;
  expected := Vector3f(0, -1, 6);
  AssertEquals('Subtract vector3f mismatch', true, actual = expected);
end;

procedure TVector3fTests.ProductTest_ScalarLeft;
var
  v: ToglVector3f;
  expected, actual: ToglVector3f;
begin
  v := Vector3f(1, 2, 3);
  actual := 2*v;
  expected := Vector3f(2, 4, 6);
  AssertEquals('Product with scalar left mismatch', true, expected = actual);
end;

procedure TVector3fTests.ProductTest_ScalarRight;
var
  v: ToglVector3f;
  expected, actual: ToglVector3f;
begin
  v := Vector3f(1, 2, 3);
  actual := v*2;
  expected := Vector3f(2, 4, 6);
  AssertEquals('Product with scalar left mismatch', true, expected = actual);
end;

procedure TVector3fTests.DotProductTest;
var
  v1, v2: ToglVector3f;
begin
  v1 := Vector3f(1, 2, 3);
  v2 := Vector3f(1, 3, -3);
  AssertEquals('Dot product vector3f mismatch', -2, DotProduct(v1, v2));
end;

procedure TVector3fTests.CrossProductTest;
var
  v1, v2: ToglVector3f;
  actual, expected: ToglVector3f;
begin
  v1 := Vector3f(1, 0, 0);
  v2 := Vector3f(0, 1, 0);
  actual := CrossProduct(v1, v2);
  expected := Vector3f(0, 0, 1);
  AssertEquals('Cross product vector3f mismatch', true, expected=actual);
end;

procedure TVector3fTests.LengthTest;
var
  v: ToglVector3f;
  expected, actual: GLfloat;
begin
  v := Vector3f(1, 2, 3);
  actual := v.Length;
  expected := sqrt(1+4+9);
  AssertEquals('Length vector3f mismatch (pos)', expected, actual);

  v := Vector3f(1, 2, -3);
  actual := v.Length;
  expected := sqrt(1+4+9);
  AssertEquals('Length vector3f mismatch (neg)', expected, actual);
end;


{ TMatrix3fTests }

procedure TVector3fTests.ElementTest;
var
  v: ToglVector3f;
begin
  v := Vector3f(1, 2, 3);
  AssertEquals('Element 0 vx X mismatch', true, v.X = v[0]);
  AssertEquals('Element 1 vs Y mismatch', true, v.Y = v[1]);
  AssertEquals('Element 2 vs Z mismatch', true, v.Z = v[2]);
end;

procedure TMatrix3fTests.EqualTest;
var
  m1, m2, m3: ToglMatrix3f;
begin
  m1.Init(1, 2, 3, 4, 5, 6, 7, 8, 9);
  m2.Init(1, 2, 3, 4, 5, 6, 7, 8, 9);
  m3.Init(1, 0, 3, 4, 5, 6, 7, 8, 9);
  AssertEquals('Equal matrix3f mismatch x', true, m1[0]=m2[0]);
  AssertEquals('Equal matrix3f mismatch y', true, m1[1]=m2[1]);
  AssertEquals('Equal matrix3f mismatch z', true, m1[2]=m2[2]);
  AssertEquals('Equal matrix3f mismatch', true, m1=m2);
  AssertEquals('Equal matrix3f mismatch', false, m1=m3);
end;

procedure TMatrix3fTests.NotEqualTest;
var
  m1, m2: ToglMatrix3f;
begin
  m1.Init(1, 2, 3, 4, 5, 6, 7, 8, 9);
  m2.Init(1, 1, 3, 4, 5, 6, 7, 8, 9);
  AssertEquals('Not equal matrix3f mismatch (different)', true, m1 <> m2);
  AssertEquals('Not equal matrix3f mismatch (same)', false, m1 <> m1);
end;

procedure TMatrix3fTests.AddTest;
var
  m1, m2: ToglMatrix3f;
  expected: ToglMatrix3f;
begin
  m1.Init(1,  2, 3, 4, 5, 6,  7, 8,  9);
  m2.Init(2, -2, 3, 0, 6, 3, -1, 4, -3);
  expected.Init(3, 0, 6, 4, 11, 9, 6, 12, 6);
  AssertEquals('Add matrix3f mismatch.', true, expected = m1+m2);
end;

procedure TMatrix3fTests.SubtractTest;
var
  m1, m2: ToglMatrix3f;
  expected: ToglMatrix3f;
begin
  m1.Init(1,  2, 3, 4, 5, 6,  7, 8,  9);
  m2.Init(2, -2, 3, 0, 6, 3, -1, 4, -3);
  expected.Init(-1, 4, 0, 4, -1, 3, 8, 4, 12);
  AssertEquals('Subtract matrix3f mismatch.', true, expected = m1-m2);
end;

procedure TMatrix3fTests.ProductTest_ScalarLeft;
var
  m: ToglMatrix3f;
  actual, expected: ToglMatrix3f;
begin
  m.Init(1, 2, 3, 4, 5, 6, 7, 8, 9);
  actual := 2*m;
  expected.Init(2, 4, 6, 8, 10, 12, 14, 16, 18);
  AssertEquals('Product matrix * scalar mismatch.', true, expected = actual);
end;

procedure TMatrix3fTests.ProductTest_ScalarRight;
var
  m: ToglMatrix3f;
  actual, expected: ToglMatrix3f;
begin
  m.Init(1, 2, 3, 4, 5, 6, 7, 8, 9);
  actual := m*2;
  expected.Init(2, 4, 6, 8, 10, 12, 14, 16, 18);
  AssertEquals('Product matrix * scalar mismatch.', true, expected = actual);
end;

procedure TMatrix3fTests.ProductTest_VectorLeft;
var
  m: ToglMatrix3f;
  v: ToglVector3f;
  actual: ToglVector3f;
  expected: ToglVector3f;
begin
  m.Init(1, 2, 3, 4, 5, 6, 7, 8, 9);
  v := Vector3f(2, -1, 0);
  expected := Vector3f(0, 3, 6);
  actual := v*m;
  AssertEquals('Product vector * matrix mismatch.', true, expected = actual);
end;

procedure TMatrix3fTests.ProductTest_VectorRight;
var
  m: ToglMatrix3f;
  v: ToglVector3f;
  actual: ToglVector3f;
  expected: ToglVector3f;
begin
  m.Init(1, 2, 3, 4, 5, 6, 7, 8, 9);
  v := Vector3f(2, -1, 0);
  actual := m * v;
  expected := Vector3f(-2, -1, 0);
  AssertEquals('Product matrix * vector mismatch.', true, expected = actual);
end;

procedure TMatrix3fTests.ProductTest_MatrixMatrix;
var
  m1, m2: ToglMatrix3f;
  actual: ToglMatrix3f;
  expected: ToglMatrix3f;
begin
  m1.Init(       1, 2, 3,   4, 5, 6,   7,  8, 9);
  m2.Init(       0,-2, 1,   3, 0, 0,   1, -1, 2);
  expected.Init(-1,-2,-3,   3, 6, 9,  11, 13, 15);
  actual := m1 * m2;
  AssertEquals('Product matrix * vector mismatch.', true, expected = actual);
end;

procedure TMatrix3fTests.DeterminantTest;
var
  m: ToglMatrix3f;
  actual, expected: GLfloat;
begin
  m.Init(1, 2, 3, 4, 5, 6, 7, 8, 9);
  expected := 0;
  actual := m.GetDeterminant;
  AssertEquals('Determinant mismatch.', expected, actual);
end;

procedure TMatrix3fTests.InvertTest;
var
  m: ToglMatrix3f;
  actual, expected: ToglMatrix3f;
begin
  m.Init(1, 0, 2,   4, 5, 4,   7, 8, 8);
  actual := m.Invert;
  expected.Init(4, 8, -5,  -2, -3, 2,   -1.5, -4, 2.5); // SciLab result
  AssertEquals('Invert mismatch.', true, actual=expected);
end;

procedure TMatrix3fTests.TransposeTest;
var
  m: ToglMatrix3f;
  actual, expected: ToglMatrix3f;
begin
  m.Init(1, 2, 3, 4, 5, 6, 7, 8, 9);
  actual := m.Transpose;
  expected.Init(1, 4, 7,  2, 5, 8,  3, 6, 9);
  AssertEquals('Transpose mismatch.', true, actual=expected);
end;

(*
procedure TMatrix3fTests.AngleTest;
var
  expected: GLfloat = 30.0;
  m: ToglMatrix3f;
begin
  m.Init(cos(DegToRad(expected)), sin(DegToRad(expected)), 3, 4);
  AssertEquals('Angle mismatch.', expected, m.GetAngle);
end;
*)

initialization
  RegisterTest(TVector3fTests);
  RegisterTest(TMatrix3fTests);

end.

