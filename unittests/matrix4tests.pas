unit matrix4tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TVector4fTests = class(TTestCase)
  published
    procedure ElementTest;
    procedure EqualTest;
    procedure NotEqualTest;
    procedure AddTest;
    procedure SubtractTest;
    procedure ProductTest_ScalarLeft;
    procedure ProductTest_ScalarRight;
    procedure DotProductTest;
    procedure LengthTest;
  end;

  TMatrix4fTests= class(TTestCase)
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

{ TVector4fTests }

procedure TVector4fTests.ElementTest;
var
  v: ToglVector4f;
begin
  v := Vector4f(1, 2, 3, 4);
  AssertEquals('Element 0 vs X mismatch', true, v.X = v[0]);
  AssertEquals('Element 1 vs Y mismatch', true, v.Y = v[1]);
  AssertEquals('Element 2 vs Z mismatch', true, v.Z = v[2]);
  AssertEquals('Element 3 vs W mismatch', true, v.W = v[3]);
end;

procedure TVector4fTests.EqualTest;
var
  v1, v2, v3: ToglVector4f;
begin
  v1 := Vector4f(1, 2, 3, 4);
  v2 := Vector4f(1, 2, 3, 4);
  v3 := Vector4f(1, 0, 3, 4);
  AssertEquals('Equal vector4f mismatch x', true, v1.X=v2.X);
  AssertEquals('Equal vector4f mismatch y', true, v1.Y=v2.Y);
  AssertEquals('Equal vector4f mismatch z', true, v1.Z=v2.Z);
  AssertEquals('Equal vector4f mismatch 2', true, v1.W=v2.W);
  AssertEquals('Equal vector4f mismatch (equal)', true, v1=v2);
  AssertEquals('Equal vector4f mismatch (different)', false, v1=v3);
end;

procedure TVector4fTests.NotEqualTest;
var
  v1, v2: ToglVector4f;
begin
  v1 := Vector4f(1, 2, 3, 4);
  v2 := Vector4f(1, 3, 3, 4);
  AssertEquals('Not equal vector4f mismatch (different)', true, v1<>v2);
  AssertEquals('Not equal vector4f mismatch (same)', false, v1<>v1);
end;

procedure TVector4fTests.AddTest;
var
  v1, v2: ToglVector4f;
  expected: ToglVector4f;
begin
  v1 := Vector4f(1, 2, 3, 4);
  v2 := Vector4f(1, 3, -3, 1);
  expected := Vector4f(2, 5, 0, 5);
  AssertEquals('Add vector4f mismatch', true, v1+v2 = expected);
end;

procedure TVector4fTests.SubtractTest;
var
  v1, v2: ToglVector4f;
  actual, expected: ToglVector4f;
begin
  v1 := Vector4f(1, 2, 3, 4);
  v2 := Vector4f(1, 3, -3, 1);
  actual := v1 - v2;
  expected := Vector4f(0, -1, 6, 3);
  AssertEquals('Subtract vector4f mismatch', true, actual = expected);
end;

procedure TVector4fTests.ProductTest_ScalarLeft;
var
  v: ToglVector4f;
  expected, actual: ToglVector4f;
begin
  v := Vector4f(1, 2, 3, 4);
  actual := 2*v;
  expected := Vector4f(2, 4, 6, 8);
  AssertEquals('Product with scalar left mismatch', true, expected = actual);
end;

procedure TVector4fTests.ProductTest_ScalarRight;
var
  v: ToglVector4f;
  expected, actual: ToglVector4f;
begin
  v := Vector4f(1, 2, 3, 4);
  actual := v*2;
  expected := Vector4f(2, 4, 6, 8);
  AssertEquals('Product with scalar left mismatch', true, expected = actual);
end;

procedure TVector4fTests.DotProductTest;
var
  v1, v2: ToglVector4f;
begin
  v1 := Vector4f(1, 2, 3, 4);
  v2 := Vector4f(1, 3, -3, 1);
  AssertEquals('Dot product vector4f mismatch', 2, DotProduct(v1, v2));
end;

procedure TVector4fTests.LengthTest;
var
  v: ToglVector4f;
  expected, actual: GLfloat;
begin
  v := Vector4f(1, 2, 3, 4);
  actual := v.Length;
  expected := sqrt(1+4+9+16);
  AssertEquals('Length vector4f mismatch (pos)', expected, actual);

  v := Vector4f(1, 2, 3, -4);
  actual := v.Length;
  expected := sqrt(1+4+9+16);
  AssertEquals('Length vector4f mismatch (neg)', expected, actual);
end;


{ TMatrix4fTests }

procedure TMatrix4fTests.EqualTest;
var
  m1, m2, m3: ToglMatrix4f;
begin
  m1.Init(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
  m2.Init(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
  m3.Init(1, 0, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
  AssertEquals('Equal matrix4f mismatch x', true, m1[0]=m2[0]);
  AssertEquals('Equal matrix4f mismatch y', true, m1[1]=m2[1]);
  AssertEquals('Equal matrix4f mismatch z', true, m1[2]=m2[2]);
  AssertEquals('Equal matrix4f mismatch w', true, m1[3]=m2[3]);
  AssertEquals('Equal matrix4f mismatch', true, m1=m2);
  AssertEquals('Equal matrix4f mismatch', false, m1=m3);
end;

procedure TMatrix4fTests.NotEqualTest;
var
  m1, m2: ToglMatrix4f;
begin
  m1.Init(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
  m2.Init(1, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);
  AssertEquals('Not equal matrix4f mismatch (different)', true, m1 <> m2);
  AssertEquals('Not equal matrix4f mismatch (same)', false, m1 <> m1);
end;

procedure TMatrix4fTests.AddTest;
var
  m1, m2: ToglMatrix4f;
  expected: ToglMatrix4f;
begin
  m1.Init(1,  2, 3, 4, 5, 6,  7, 8,  9, 10, 11, 12, 13, 14, 15, 16);
  m2.Init(2, -2, 3, 0, 6, 3, -1, 4, -3,  0,  2, -2, -3, -4, -5, -6);
  expected.Init(3, 0, 6, 4, 11, 9, 6, 12, 6, 10, 13, 10, 10, 10, 10, 10);
  AssertEquals('Add matrix4f mismatch.', true, expected = m1+m2);
end;

procedure TMatrix4fTests.SubtractTest;
var
  m1, m2: ToglMatrix4f;
  expected: ToglMatrix4f;
begin
  m1.Init      ( 1, 2, 3, 4, 5, 6,  7, 8,  9, 10, 11, 12, 13, 14, 15, 16);
  m2.Init      ( 2,-2, 3, 0, 6, 3, -1, 4, -3,  0,  2, -2, -3, -4, -5, -6);
  expected.Init(-1, 4, 0, 4,-1, 3,  8, 4, 12, 10,  9, 14, 16, 18, 20, 22);
  AssertEquals('Subtract matrix3f mismatch.', true, expected = m1-m2);
end;

procedure TMatrix4fTests.ProductTest_ScalarLeft;
var
  m: ToglMatrix4f;
  actual, expected: ToglMatrix4f;
begin
  m.Init      (1, 2, 3, 4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16);
  actual := 2*m;
  expected.Init(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32);
  AssertEquals('Product matrix * scalar mismatch.', true, expected = actual);
end;

procedure TMatrix4fTests.ProductTest_ScalarRight;
var
  m: ToglMatrix4f;
  actual, expected: ToglMatrix4f;
begin
  m.Init      (1, 2, 3, 4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16);
  actual := m*2;
  expected.Init(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32);
  AssertEquals('Product matrix * scalar mismatch.', true, expected = actual);
end;

procedure TMatrix4fTests.ProductTest_VectorLeft;
var
  m: ToglMatrix4f;
  v: ToglVector4f;
  actual: ToglVector4f;
  expected: ToglVector4f;
begin
  v := Vector4f(0, -2, 1, 0);
  m.Init(1, 2, 3, 1,  -1,-2, 0, 1,   2, 3,-1,-1,   3, 0, 1, 0);
  actual := v*m;
  expected := Vector4f(-1, 4, -7, 1);
  AssertEquals('Product vector * matrix mismatch.', true, expected = actual);
end;

procedure TMatrix4fTests.ProductTest_VectorRight;
var
  m: ToglMatrix4f;
  v: ToglVector4f;
  actual: ToglVector4f;
  expected: ToglVector4f;
begin
  m.Init(1, 2, 3, 1,  -1,-2, 0, 1,   2, 3,-1,-1,   3, 0, 1, 0);
  v := Vector4f(0, -2, 1, 0);
  actual := m * v;
  expected := Vector4f(4, 7, -1, -3);
  AssertEquals('Product matrix * vector mismatch.', true, expected = actual);
end;

procedure TMatrix4fTests.ProductTest_MatrixMatrix;
var
  m1, m2: ToglMatrix4f;
  actual: ToglMatrix4f;
  expected: ToglMatrix4f;
begin
  m1.Init(       1, 2, 3, 1,  -1,-2, 0, 1,   2, 3,-1,-1,   3, 0, 1, 0);
  m2.Init(       0,-2, 1, 0,   3, 0, 0, 1,   1,-1, 2, 0,   4, 1,-2, 2);
  actual := m1 * m2;
  expected.Init( 4, 7,-1,-3,   6, 6,10, 3,   6,10, 1,-2,   5, 0,16, 7);
  AssertEquals('Product matrix * vector mismatch.', true, expected = actual);
end;

procedure TMatrix4fTests.DeterminantTest;
var
  m: ToglMatrix4f;
  actual, expected: GLfloat;
begin
  m.Init(1, -2, 3, 4, 5, 6, 7, 8, 9, -10, 11, 12, 13, 14, 1, 1);
  expected := -1152;
  actual := m.GetDeterminant;
  AssertEquals('Determinant mismatch.', expected, actual);
end;

procedure TMatrix4fTests.InvertTest;
var
  m: ToglMatrix4f;
  actual, expected: ToglMatrix4f;
begin
  m.Init(1, 2, 1, -1,   -2, 2, 1, 1,   3, 1, 0, 1,   2, 0, 1, 0);
  actual := m.Invert;
  expected.Init(                   // values from SciLab
    0.0434783, -0.1304348,  0.1739130,  0.0869565,
    0.3043478,  0.0869565,  0.2173913, -0.3913043,
   -0.0869565,  0.2608696, -0.3478261,  0.8260870,
   -0.4347826,  0.3043478,  0.2608696,  0.1304348
  );
  AssertEquals('Invert mismatch.', true, actual.Equal(expected, 1E-7));
end;

procedure TMatrix4fTests.TransposeTest;
var
  m: ToglMatrix4f;
  actual, expected: ToglMatrix4f;
begin
  m.Init(1, 2, 3, 4,   5, 6, 7, 8,   9, 10, 11, 12,   13, 14, 15, 16);
  actual := m.Transpose;
  expected.Init(1, 5, 9, 13,  2, 6, 10, 14,  3, 7, 11, 15,  4, 8, 12, 16);
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
  RegisterTest(TVector4fTests);
  RegisterTest(TMatrix4fTests);

end.

