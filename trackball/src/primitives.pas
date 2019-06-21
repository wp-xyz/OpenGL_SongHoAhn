unit primitives;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gl,
  vectors;

type
  TPrimitive = class
  private
    FColor: TVector4;
    FPosition: TVector3;
    FRotation: TVector3;
  protected
  public
    property Color: TVector4 read FColor write FColor;
    property Position: TVector3 read FPosition write FPosition;
    property Rotation: TVector3 read FPosition write FRotation;
  end;

  TPlane = class(TPrimitive)
  private
    FWidth: GLfloat;
    FHeight: GLfloat;
  protected
  public
    constructor Create(AWidth: GLfloat = 1.0; AHeight: GLfloat = 1.0);
    property Width: GLfloat read FWidth write FWidth;
    property Height: GLfloat read FHeight write FHeight;
  end;

  TSphere = class(TPrimitive)
  private
    FVertices: array of GLfloat;
    FNormals: array of GLfloat;
    FIndices: array of Integer;
    FRadius: GLfloat;
    FSliceCount: integer;
    FStackCount: Integer;
//    procedure GetVertices
    procedure SetRadius(r: GLfloat);
    procedure SetSliceCount(n: Integer);
    procedure SetStackCount(n: Integer);
  protected
    procedure BuildSphere;
  public
    constructor Create(ARadius: GLfloat = 1.0; ASlices: Integer = 10; AStacks: Integer = 10);
    procedure SetParams(ARadius: GLfloat; ASlices, AStacks: Integer);
    property Radius: GLfloat read FRadius write SetRadius;
    property SliceCount: Integer read FSliceCount write SetSliceCount;
    property StackCount: Integer read FStackCount write SetStackCount;
  end;


implementation

constructor TPlane.Create(AWidth: GLfloat = 1.0; AHeight: GLfloat = 1.0);
begin
  FWidth := AWidth;
  FHeight := AHeight;
end;

constructor TSphere.Create(ARadius: GLfloat = 1.0; ASlices: Integer = 10;
  AStacks: Integer = 10);
begin
  SetParams(ARadius, ASlices, AStacks);
end;

procedure TSphere.BuildSphere;
const
  TWO_PI = pi * 2.0;
  PI_HALF = pi * 0.5;
var
  x, y, z: GLfloat;
  a, b: GLfloat;
  deltaA, deltaB: GLfloat;
  i, j: Integer;
begin
  SetLength(FVertices, 0);
  SetLength(FNormals, 0);
  SetLength(FIndices, 0);

  a := 0;                            // angle for slices (0 ... 2pi)
  b := -PI_HALF;                     // angle for stack  (-pi/2 ... pi/2)
  deltaA := TWO_PI / (FSliceCount - 1);
  deltaB := pi / (FStackCount - 1);
  for i := 0 to FStackCount do begin
    b := pi / FSliceCount * i + PI_HALF;
    for j := 0 to FSliceCount do
      a := j * TWO_PI / (FSliceCount - 1);
  end;

  ////// NONSENSE !!!!
end;

procedure TSphere.SetParams(ARadius: GLFloat; ASlices, AStacks: Integer);
begin
  if ARadius < 0 then FRadius := 0.0 else FRadius := ARadius;
  if ASlices < 2 then FSliceCount := 2 else FSliceCount := ASlices;
  if AStacks < 2 then FStackCount := 2 else FStackCount := AStacks;
  BuildSphere;
end;

procedure TSphere.SetRadius(r: GLfloat);
begin
  if r < 0 then FRadius := 0.0 else FRadius := r;
  BuildSphere;
end;

procedure TSphere.SetSliceCount(n: Integer);
begin
  if n < 2 then FSliceCount := 2 else FSliceCount := n;
  BuildSphere;
end;

procedure TSphere.SetStackCount(n: Integer);
begin
  if n < 2 then FStackCount := 2 else FStackCount := n;
  BuildSphere;
end;


end.

