unit matrix2tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TVector2fTests = class(TTestCase)
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

  TMatrix2fTests= class(TTestCase)
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
    procedure AngleTest;
  end;

implementation

uses
  Math, gl, oglTypes, oglMath;

{ TVector2fTests }

procedure TVector2fTests.ElementTest;
var
  v: ToglVector2f;
begin
  v := Vector2f(1, 2);
  AssertEquals('Element 0 vx X mismatch', true, v.X = v[0]);
  AssertEquals('Element 1 vs Y mismatch', true, v.Y = v[1]);
end;

procedure TVector2fTests.EqualTest;
var
  v1, v2, v3: ToglVector2f;
begin
  v1 := Vector2f(1, 2);
  v2 := Vector2f(1, 2);
  v3 := Vector2f(1, 0);
  AssertEquals('Equal vector2f mismatch x', true, v1.X=v2.X);
  AssertEquals('Equal vector2f mismatch y', true, v1.Y=v2.Y);
  AssertEquals('Equal vector2f mismatch x', true, v1=v2);
  AssertEquals('Equal vector2f mismatch x', false, v1=v3);
end;

procedure TVector2fTests.NotEqualTest;
var
  v1, v2: ToglVector2f;
begin
  v1 := Vector2f(1, 2);
  v2 := Vector2f(1, 3);
  AssertEquals('Not equal vector2f mismatch (different)', true, v1<>v2);
  AssertEquals('Not equal vector2f mismatch (same)', false, v1<>v1);
end;

procedure TVector2fTests.AddTest;
var
  v1, v2: ToglVector2f;
  expected: ToglVector2f;
begin
  v1 := Vector2f(1, 2);
  v2 := Vector2f(1, 3);
  expected := Vector2f(2, 5);
  AssertEquals('Add vector2f mismatch', true, v1+v2 = expected);
end;

procedure TVector2fTests.SubtractTest;
var
  v1, v2: ToglVector2f;
  expected: ToglVector2f;
begin
  v1 := Vector2f(1, 2);
  v2 := Vector2f(1, 3);
  expected := Vector2f(0, -1);
  AssertEquals('Subtract vector2f mismatch', true, v1-v2 = expected);
end;

procedure TVector2fTests.ProductTest_ScalarLeft;
var
  v: ToglVector2f;
  expected: ToglVector2f;
begin
  v := Vector2f(1, 2);
  expected := Vector2f(2, 4);
  AssertEquals('Product with scalar left mismatch', true, expected = 2*v);
end;

procedure TVector2fTests.ProductTest_ScalarRight;
var
  v: ToglVector2f;
  expected: ToglVector2f;
begin
  v := Vector2f(1, 2);
  expected := Vector2f(2, 4);
  AssertEquals('Product with scalar left mismatch', true, expected = v*2);
end;

procedure TVector2fTests.DotProductTest;
var
  v1, v2: ToglVector2f;
begin
  v1 := Vector2f(1, 2);
  v2 := Vector2f(1, 3);
  AssertEquals('Dot product vector2f mismatch', 7, DotProduct(v1, v2));
end;

procedure TVector2fTests.LengthTest;
var
  v: ToglVector2f;
  expected, actual: GLfloat;
begin
  v := Vector2f(1, 2);
  actual := v.Length;
  expected := sqrt(1+4);
  AssertEquals('Length vector2f mismatch (pos)', expected, actual);

  v := Vector2f(1, -2);
  actual := v.Length;
  expected := sqrt(1+4);
  AssertEquals('Length vector2f mismatch (neg)', expected, actual);
end;


{ TMatrix2fTests }

procedure TMatrix2fTests.EqualTest;
var
  m1, m2, m3: ToglMatrix2f;
begin
  m1.Init(1, 2, 3, 4);
  m2.Init(1, 2, 3, 4);
  m3.Init(1, 0, 3, 4);
  AssertEquals('Equal matrix2f mismatch x', true, m1[0]=m2[0]);
  AssertEquals('Equal matrix2f mismatch y', true, m1[1]=m2[1]);
  AssertEquals('Equal matrix2f mismatch z', true, m1[2]=m2[2]);
  AssertEquals('Equal matrix2f mismatch', true, m1=m2);
  AssertEquals('Equal matrix2f mismatch', false, m1=m3);
end;

procedure TMatrix2fTests.NotEqualTest;
var
  m1, m2: ToglMatrix2f;
begin
  m1.Init(1, 2, 3, 4);
  m2.Init(1, 1, 3, 4);
  AssertEquals('Not equal matrix2f mismatch (different)', true, m1 <> m2);
  AssertEquals('Not equal matrix2f mismatch (same)', false, m1 <> m1);
end;

procedure TMatrix2fTests.AddTest;
var
  m1, m2: ToglMatrix2f;
  expected: ToglMatrix2f;
begin
  m1.Init(1, 2, 3, 4);
  m2.Init(2, -2, 3, 0);
  expected.Init(3, 0, 6, 4);
  AssertEquals('Add matrix2f mismatch.', true, expected = m1+m2);
end;

procedure TMatrix2fTests.SubtractTest;
var
  m1, m2: ToglMatrix2f;
  expected: ToglMatrix2f;
begin
  m1.Init(1, 2, 3, 4);
  m2.Init(2, -2, 3, 0);
  expected.Init(-1, 4, 0, 4);
  AssertEquals('Subtract matrix2f mismatch.', true, expected = m1-m2);
end;

procedure TMatrix2fTests.ProductTest_ScalarLeft;
var
  m: ToglMatrix2f;
  actual, expected: ToglMatrix2f;
begin
  m.Init(1, 2, 3, 4);
  actual := 2*m;
  expected.Init(2, 4, 6, 8);
  AssertEquals('Product matrix * scalar mismatch.', true, expected = actual);
end;

procedure TMatrix2fTests.ProductTest_ScalarRight;
var
  m: ToglMatrix2f;
  actual, expected: ToglMatrix2f;
begin
  m.Init(1, 2, 3, 4);
  actual := m*2;
  expected.Init(2, 4, 6, 8);
  AssertEquals('Product matrix * scalar mismatch.', true, expected = actual);
end;

procedure TMatrix2fTests.ProductTest_VectorLeft;
var
  m: ToglMatrix2f;
  v: ToglVector2f;
  actual: ToglVector2f;
  expected: ToglVector2f;
begin
  m.Init(1, 2, 3, 4);
  v := Vector2f(2, -1);
  expected := Vector2f(0, 2);
  actual := v*m;
  AssertEquals('Product vector * matrix mismatch.', true, expected = actual);
end;

procedure TMatrix2fTests.ProductTest_VectorRight;
var
  m: ToglMatrix2f;
  v: ToglVector2f;
  actual: ToglVector2f;
  expected: ToglVector2f;
begin
  m.Init(1, 2, 3, 4);
  v := Vector2f(2, -1);
  expected := Vector2f(-1, 0);
  actual := m * v;
  AssertEquals('Product matrix * vector mismatch.', true, expected = actual);
end;

procedure TMatrix2fTests.ProductTest_MatrixMatrix;
var
  m1, m2: ToglMatrix2f;
  actual: ToglMatrix2f;
  expected: ToglMatrix2f;
begin
  m1.Init(1, 2, 3, 4);
  m2.Init(0, -2, 1, 3);
  expected.Init(-6, -8, 10, 14);
  actual := m1 * m2;
  AssertEquals('Product matrix * vector mismatch.', true, expected = actual);
end;

procedure TMatrix2fTests.DeterminantTest;
var
  m: ToglMatrix2f;
  actual, expected: GLfloat;
begin
  m.Init(1, 2, 3, 4);
  expected := -2;
  actual := m.GetDeterminant;
  AssertEquals('Determinant mismatch.', expected, actual);
end;

procedure TMatrix2fTests.InvertTest;
var
  m: ToglMatrix2f;
  actual, expected: ToglMatrix2f;
begin
  m.Init(1, 2, 3, 4);
  expected.Init(-2, 1, 1.5, -0.5);  // SciLab result
  actual := m.Invert;
  AssertEquals('Invert mismatch.', true, actual=expected);
end;

procedure TMatrix2fTests.TransposeTest;
var
  m: ToglMatrix2f;
  actual, expected: ToglMatrix2f;
begin
  m.Init(1, 2, 3, 4);
  expected.Init(1, 3, 2, 4);
  actual := m.Transpose;
  AssertEquals('Transpose mismatch.', true, actual=expected);
end;

procedure TMatrix2fTests.AngleTest;
var
  expected: GLfloat = 30.0;
  m: ToglMatrix2f;
begin
  m.Init(cos(DegToRad(expected)), sin(DegToRad(expected)), 3, 4);
  AssertEquals('Angle mismatch.', expected, m.GetAngle);
end;

initialization
  RegisterTest(TVector2fTests);
  RegisterTest(TMatrix2fTests);

end.

