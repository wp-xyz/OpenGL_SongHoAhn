unit oglSphere;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, graphics, gl, oglTypes;

const
  MIN_SECTOR_COUNT = 3;
  MIN_STACK_COUNT  = 2;

type
  ToglSphere = class
  private
    FVertices: array of GLFloat;
    FNormals: array of GLFloat;
    FTexCoords: array of GLFloat;
    FIndices: array of Integer;
    FLineIndices: array of Integer;
    FInterleavedVertices: array of GLFloat;
    FInterleavedStride: Integer;
    FRadius: GLFloat;
    FSectorCount: Integer;
    FSmooth: Boolean;
    FStackCount: Integer;
    function GetIndexCount: Integer;
    function GetLineIndexCount: Integer;
    function GetNormalCount: Integer;
    function GetTexCoordCount: Integer;
    function GetTriangleCount: Integer;
    function GetVertexCount: Integer;
    procedure SetRadius(const AValue: GLFloat);
    procedure SetSectorCount(const AValue: Integer);
    procedure SetSmooth(const AValue: Boolean);
    procedure SetStackCount(const AValue: Integer);

  protected
    procedure AddIndices(i1, i2, i3: Integer);
    procedure AddInterleavedVertex(x, y, z, nx, ny, nz, s, t: GLFloat);
    procedure AddLineIndices(i1, i2: Integer);
    procedure AddNormal(nx, ny, nz: GLFloat);
    procedure AddTexCoord(s, t: GLFloat);
    procedure AddVertex(x, y, z: GLFloat);
    procedure BuildInterleavedVertices;
    procedure BuildVerticesFlat;
    procedure BuildVerticesSmooth;
    procedure ClearArrays;
    function ComputeFaceNormal(x1,y1,z1, x2,y2,z2, x3,y3,z3: GLFloat): ToglVector3f;
    procedure SetParams(ARadius: GLFloat; ASectorCount: Integer;
      AStackCount: Integer; ASmooth: Boolean);
    procedure UpdateRadius;

  public
    constructor Create(ARadius: GLFloat = 1.0; ASectorCount: Integer = 36;
      AStackCount: Integer = 18; ASmooth: Boolean = true);
    procedure Draw;
    procedure DrawLines(const ALineColor: ToglVector4f);
    procedure DrawWithLines(const ALineColor: ToglVector4f);
    function Info(AFloatFormat: String = '0.000'): String;

    property Radius: GLFloat read FRadius write SetRadius;
    property SectorCount: Integer read FSectorCount write SetSectorCount;
    property Smooth: Boolean read FSmooth;
    property StackCount: Integer read FStackCount write SetStackCount;

    property IndexCount: Integer read GetIndexCount;
    property LineIndexCount: Integer read GetLineIndexCount;
    property NormalCount: Integer read GetNormalCount;
    property TexCoordCount: Integer read GetTexCoordCount;
    property TriangleCount: Integer read GetTriangleCount;
    property VertexCount: Integer read GetVertexCount;
  end;


implementation

{-------------------------------------------------------------------------------
  Constructor of ToglSphere
-------------------------------------------------------------------------------}
constructor ToglSphere.Create(ARadius: GLFloat = 1.0; ASectorCount: Integer = 36;
  AStackCount: Integer = 18; ASmooth: Boolean = true);
begin
  FInterleavedStride := 32;
  SetParams(ARadius, ASectorCount, AStackCount, ASmooth);
end;


{-------------------------------------------------------------------------------
  Add 3 indices to array
  -------------------------------------------------------------------------------}
procedure ToglSphere.AddIndices(i1, i2, i3: Integer);
var
  n: Integer;
begin
  n := Length(FIndices);
  SetLength(FIndices, n + 3);
  FIndices[n] := i1;
  FIndices[n+1] := i2;
  FIndices[n+2] := i3;
end;


{-------------------------------------------------------------------------------
  Adds an interleaved vertex to array
-------------------------------------------------------------------------------}
procedure ToglSphere.AddInterleavedVertex(x, y, z, nx, ny, nz, s, t: GLFloat);
var
  n: Integer;
begin
  n := Length(FInterleavedVertices);
  SetLength(FInterleavedVertices, n + 3 + 3 + 2);
  FInterleavedVertices[n  ] := x;
  FInterleavedVertices[n+1] := y;
  FInterleavedVertices[n+2] := z;
  FInterleavedVertices[n+3] := nx;
  FInterleavedVertices[n+4] := ny;
  FInterleavedVertices[n+5] := nz;
  FInterleavedVertices[n+6] := s;
  FInterleavedVertices[n+7] := t;
end;


{-------------------------------------------------------------------------------
  Add 2 line indices to array
  -------------------------------------------------------------------------------}
procedure ToglSphere.AddLineIndices(i1, i2: Integer);
var
  n: Integer;
begin
  n := Length(FLineIndices);
  SetLength(FLineIndices, n + 2);
  FLineIndices[n] := i1;
  FLineIndices[n+1] := i2;
end;


{-------------------------------------------------------------------------------
  Add single normal to array
-------------------------------------------------------------------------------}
procedure ToglSphere.AddNormal(nx, ny, nz: GLFloat);
var
  n: Integer;
begin
  n := Length(FNormals);
  SetLength(FNormals, n + 3);
  FNormals[n] := nx;
  FNormals[n+1] := ny;
  FNormals[n+2] := nz;
end;


{-------------------------------------------------------------------------------
  Add single texture coordinate to array
-------------------------------------------------------------------------------}
procedure ToglSphere.AddTexCoord(s, t: GLFloat);
var
  n: Integer;
begin
  n := Length(FTexCoords);
  SetLength(FTexCoords, n + 2);
  FTexCoords[n] := s;
  FTexCoords[n+1] := t;
end;


{-------------------------------------------------------------------------------
  Add single vertex to array
-------------------------------------------------------------------------------}
procedure ToglSphere.AddVertex(x, y, z: GLFloat);
var
  n: Integer;
begin
  n := Length(FVertices);
  SetLength(FVertices, n + 3);
  FVertices[n] := x;
  FVertices[n+1] := y;
  FVertices[n+2] := z;
end;


{-------------------------------------------------------------------------------
  Generate interleaved vertices: vertex - normal- texture coordinate
  Stride must be 32 bytes  ("stride" = periodicity)
-------------------------------------------------------------------------------}
procedure ToglSphere.BuildInterleavedVertices;
var
  i, j: Integer;
  count: Integer;
begin
  SetLength(FInterleavedVertices, 0);

  count := Length(FVertices);
  i := 0;
  j := 0;
  while (i < count) do begin
    AddInterleavedVertex(
      FVertices[i], FVertices[i+1], FVertices[i+2],
      FNormals[i], FNormals[i+1], FNormals[i+2],
      FTexCoords[j], FTexCoords[j+1]
    );
    inc(i, 3);
    inc(j, 2);
  end;
end;


{-------------------------------------------------------------------------------
  Generate vertices with flat shading
  Each triangle is independent (no shared vertices)
-------------------------------------------------------------------------------}
procedure ToglSphere.BuildVerticesFlat;
type
  TTmpVertex = record                // temporary vertex definition (x,y,z,s,t)
    x, y, z, s, t: GLFloat;
  end;
var
  sectorStep: GLFloat;
  stackStep: GLFloat;
  sectorAngle, stackAngle: GLFloat;
  v, v1, v2, v3, v4: TTmpVertex;
  tmpVertices: array of TTmpVertex;
  tmpVertexCount: Integer;
  tmpVerticesCapacity: Integer;
  n: ToglVector3f;  // face normal
  index: Integer;
  i, j, k: Integer;
  vi1, vi2, vi3, vi4: Integer;
  xy, z: GLFloat;
begin
  sectorStep := 2 * pi / FSectorCount;
  stackStep := pi / FStackCount;
  tmpVertexCount := 0;
  tmpVerticesCapacity := FSectorCount * FStackCount;

  // compute all vertices first, each vertex contains (x,y,z,s,t) except normal
  for i := 0 to FStackCount do begin
    stackAngle := pi / 2 - i * stackStep;      // starting from pi/2 to -pi/2
    xy := FRadius * cos(stackAngle);           // r * cos(u)
    z := FRadius * sin(stackAngle);            // r * sin(u)

    // Add (sectorCount+1) vertices per stack
    // The first and last vertices have same position and normal, but different tex coords
    for j := 0 to FSectorCount do begin
      sectorAngle := j * sectorStep;           // starting from 0 to 2pi
      v.x := xy * cos(sectorAngle);            // x = r * cos(u) * cos(v)
      v.y := xy * sin(sectorAngle);            // y = r * cos(u) * sin(v)
      v.z := z;                                // z = r * sin(u)
      v.s := j / FSectorCount;                 // s
      v.t := i / FStackCount;                  // t
      if tmpVertexCount mod tmpVerticesCapacity = 0 then
        SetLength(tmpVertices, Length(tmpVertices) + tmpVerticesCapacity);
      tmpVertices[tmpVertexCount] := v;
      inc(tmpVertexCount);
    end;
  end;
  SetLength(tmpVertices, tmpVertexCount);

  // clear memory of prev arrays
  ClearArrays();
  index := 0;
  for i := 0 to FStackCount -1 do begin
    vi1 := i * (FSectorCount + 1);                // index of tmpVertices
    vi2 := (i + 1) * (FSectorCount + 1);

    for j := 0 to FSectorCount + 1 do begin     //; j < sectorCount; ++j, ++vi1, ++vi2)
      // get 4 vertices per sector
      //  v1--v3
      //  |    |
      //  v2--v4
      v1 := tmpVertices[vi1];
      v2 := tmpVertices[vi2];
      v3 := tmpVertices[vi1 + 1];
      v4 := tmpVertices[vi2 + 1];

      // If 1st stack and last stack, store only 1 triangle per sector
      // otherwise, store 2 triangles (quad) per sector
      if (i = 0) then begin // a triangle for first stack ====================
        // put a triangle
        AddVertex(v1.x, v1.y, v1.z);
        AddVertex(v2.x, v2.y, v2.z);
        AddVertex(v4.x, v4.y, v4.z);

        // put tex coords of triangle
        AddTexCoord(v1.s, v1.t);
        AddTexCoord(v2.s, v2.t);
        AddTexCoord(v4.s, v4.t);

        // put normal
        n := ComputeFaceNormal(v1.x, v1.y, v1.z, v2.x, v2.y, v2.z, v4.x, v4.y, v4.z);
        for k := 0 to 2 do      // same normals for 3 vertices
          AddNormal(n[0], n[1], n[2]);

        // put indices of 1 triangle
        AddIndices(index, index+1, index+2);

        // indices for line (first stack requires only vertical line)
        AddLineIndices(index, index+1);

        inc(index, 3);     // for next
      end else
      if (i = FStackCount-1) then begin  // a triangle for last stack ========
        // put a triangle
        AddVertex(v1.x, v1.y, v1.z);
        AddVertex(v2.x, v2.y, v2.z);
        AddVertex(v3.x, v3.y, v3.z);

        // put tex coords of triangle
        AddTexCoord(v1.s, v1.t);
        AddTexCoord(v2.s, v2.t);
        AddTexCoord(v3.s, v3.t);

        // put normal
        n := ComputeFaceNormal(v1.x, v1.y, v1.z, v2.x, v2.y, v2.z, v3.x, v3.y, v3.z);
        for k := 0 to 2 do   // same normals for 3 vertices
          AddNormal(n[0], n[1], n[2]);

        // put indices of 1 triangle
        AddIndices(index, index+1, index+2);

        // indices for lines (last stack requires both vert/hori lines)
        AddLineIndices(index, index+1);
        AddLineIndices(index, index+2);

        inc(index, 3);     // for next
      end else
      begin     // 2 triangles for others ====================================
        // put quad vertices: v1-v2-v3-v4
        AddVertex(v1.x, v1.y, v1.z);
        AddVertex(v2.x, v2.y, v2.z);
        AddVertex(v3.x, v3.y, v3.z);
        AddVertex(v4.x, v4.y, v4.z);

        // put tex coords of quad
        AddTexCoord(v1.s, v1.t);
        AddTexCoord(v2.s, v2.t);
        AddTexCoord(v3.s, v3.t);
        AddTexCoord(v4.s, v4.t);

        // put normal
        n := ComputeFaceNormal(v1.x, v1.y, v1.z, v2.x, v2.y, v2.z, v3.x, v3.y, v3.z);
        for k := 0 to 3 do    // same normals for 4 vertices
          AddNormal(n[0], n[1], n[2]);

        // put indices of quad (2 triangles)
        AddIndices(index, index+1, index+2);
        AddIndices(index+2, index+1, index+3);

        // indices for lines
        AddLineIndices(index, index + 1);
        AddLineIndices(index , index + 2);

        inc(index, 4);     // for next
      end;
      inc(vi1);
      inc(vi2);
    end;
  end;

  // generate interleaved vertex array as well
  BuildInterleavedVertices();
end;


{-------------------------------------------------------------------------------
  Build vertices of sphere with smooth shading using parametric equation
    x = r * cos(u) * cos(v)
    y = r * cos(u) * sin(v)
    z = r * sin(u)
  where u: stack(latitude) angle (-90 <= u <= 90)
        v: sector(longitude) angle (0 <= v <= 360)
-------------------------------------------------------------------------------}
procedure ToglSphere.BuildVerticesSmooth;
const
  TWO_PI = 2.0 * pi;
var
  x, y, z, xy: GLFloat;      // vertex position
  nx, ny, nz: GLFloat;       // normal
  lengthInv: GLFloat;
  s, t: GLFloat;             // texture coordinate
  sectorStep: GLFloat;
  stackStep: GLFloat;
  sectorAngle, stackAngle: GLFloat;
  i, j: Integer;
  k1, k2: Integer;
begin
  // clear memory of prev arrays
  ClearArrays();

  lengthInv := 1.0 / FRadius;
  sectorStep := TWO_PI / FSectorCount;
  stackStep := pi / FStackCount;

  for i := 0 to stackCount do begin
    stackAngle := pi/2 - i * stackStep;  // starting from pi/2 to -pi/2
    xy := FRadius * cos(stackAngle);     // r * cos(u)
    z := FRadius * sin(stackAngle);      // r * sin(u);

    // add (sectorCount+1) vertices per stack
    // the first and last vertices have same position and normal, but different tex coords
    for j := 0 to sectorCount do begin
      sectorAngle := j * sectorStep;    // starting from 0 to 2pi

      // vertex position
      x := xy * cos(sectorAngle);       // r * cos(u) * cos(v)
      y := xy * sin(sectorAngle);       // r * cos(u) * sin(v)
      AddVertex(x, y, z);

      // normalized vertex normal
      nx := x * lengthInv;
      ny := y * lengthInv;
      nz := z * lengthInv;
      AddNormal(nx, ny, nz);

      // vertex tex coord between [0, 1]
      s := j / FSectorCount;
      t := i / FStackCount;
      AddTexCoord(s, t);
    end;
   end;

  // indices
  //  k1--k1+1
  //  |  / |
  //  | /  |
  //  k2--k2+1
  for i := 0 to FStackCount - 1 do begin
    k1 := i * (FSectorCount + 1);     // beginning of current stack
    k2 := k1 + FSectorCount + 1;      // beginning of next stack

    for j := 0 to FSectorCount-1 do begin
      // 2 triangles per sector excluding 1st and last stacks
      if (i <> 0) then
        AddIndices(k1, k2, k1+1);   // k1---k2---k1+1
      if(i <> (FStackCount-1)) then
        AddIndices(k1+1, k2, k2+1); // k1+1---k2---k2+1

      // vertical lines for all stacks
      AddLineIndices(k1, k2);
      if (i <> 0) then  // horizontal lines except 1st stack
        AddLineIndices(k1, k1 + 1);

      inc(k1);
      inc(k2);
    end;
  end;

  // generate interleaved vertex array as well
  BuildInterleavedVertices();
end;


{-------------------------------------------------------------------------------
  Clear vectors
-------------------------------------------------------------------------------}
procedure ToglSphere.ClearArrays;
begin
  SetLength(FVertices, 0);
  Setlength(FNormals, 0);
  SetLength(FTexCoords, 0);
  SetLength(FIndices, 0);
  SetLength(FLineIndices, 0);
end;


{-------------------------------------------------------------------------------
  Returns face normal of a triangle v1-v2-v3
  If a triangle has no surface (normal length = 0), then  a zero vector is
  returned
-------------------------------------------------------------------------------}
function ToglSphere.ComputeFaceNormal(
  {v1} x1, y1, z1,
  {v2} x2, y2, z2,
  {v3} x3, y3, z3: GLFloat): ToglVector3f;
const
  EPSILON: GLfloat = 0.000001;
var
  nx, ny, nz: GLFloat;
  ex1, ey1, ez1, ex2, ey2, ez2: GLFloat;
  len, invLen: GLFloat;
begin
  // find 2 edge vectors: v1-v2, v1-v3
  ex1 := x2 - x1;
  ey1 := y2 - y1;
  ez1 := z2 - z1;
  ex2 := x3 - x1;
  ey2 := y3 - y1;
  ez2 := z3 - z1;

  // cross product: e1 x e2
  nx := ey1 * ez2 - ez1 * ey2;
  ny := ez1 * ex2 - ex1 * ez2;
  nz := ex1 * ey2 - ey1 * ex2;

  // normalize only if the length is > 0
  len := sqrt(sqr(nx) + sqr(ny) + sqr(nz));
  if len > EPSILON then begin
    // normalize
    invLen := 1.0 / len;
    Result[0] := nx * invLen;
    Result[1] := ny * invLen;
    Result[2] := nz * invLen;
  end else begin
    Result[0] := 0;
    Result[1] := 0;
    Result[2] := 0;
  end;
end;


{-------------------------------------------------------------------------------
  Draw a sphere in VertexArray mode
  OpenGL RC must be set before calling it
-------------------------------------------------------------------------------}
procedure ToglSphere.Draw;
begin
  // interleaved array
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glVertexPointer(3, GL_FLOAT, FInterleavedStride, @FInterleavedVertices[0]);
  glNormalPointer(GL_FLOAT, FInterleavedStride, @FInterleavedVertices[3]);
  glTexCoordPointer(2, GL_FLOAT, FInterleavedStride, @FInterleavedVertices[6]);

  glDrawElements(GL_TRIANGLES, Length(FIndices), GL_UNSIGNED_INT, @FIndices[0]);

  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
end;


{-------------------------------------------------------------------------------
  Draw lines only
  The caller must set the line width before call this
-------------------------------------------------------------------------------}
procedure ToglSphere.DrawLines(const ALineColor: ToglVector4f);
begin
  // set line colour
  glColor4fv(ALineColor);
  glMaterialfv(GL_FRONT, GL_DIFFUSE, ALineColor);

  // draw lines with VA
  glDisable(GL_LIGHTING);
  glDisable(GL_TEXTURE_2D);
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, 0, @FVertices[0]);

  glDrawElements(GL_LINES, Length(FLineIndices), GL_UNSIGNED_INT, @FLineIndices[0]);

  glDisableClientState(GL_VERTEX_ARRAY);
  glEnable(GL_LIGHTING);
  glEnable(GL_TEXTURE_2D);
end;


{-------------------------------------------------------------------------------
  Draw a sphere surfaces and lines on top of it
  The caller must set the line width before call this
-------------------------------------------------------------------------------}
procedure ToglSphere.DrawWithLines(const ALineColor: ToglVector4f);
begin
  glEnable(GL_POLYGON_OFFSET_FILL);
  glPolygonOffset(1.0, 1.0);   // move polygon backward
  Draw;
  glDisable(GL_POLYGON_OFFSET_FILL);

  // draw lines with VA
  DrawLines(ALineColor);
end;


function ToglSphere.GetIndexCount: Integer;
begin
  Result := Length(FIndices) div 2;
end;

function ToglSphere.GetLineIndexCount: Integer;
begin
  Result := Length(FLineIndices) div 2;
end;

function ToglSphere.GetNormalCount: Integer;
begin
  Result := Length(FNormals) div 3;
end;

function ToglSphere.GetTexCoordCount: Integer;
begin
  Result := Length(FTexCoords) div 3;
end;

function ToglSphere.GetTriangleCount: Integer;
begin
  Result := GetIndexCount div 3;
end;

function ToglSphere.GetVertexCount: Integer;
begin
  Result := Length(FVertices) div 3;
end;


{-------------------------------------------------------------------------------
  Print itself
-------------------------------------------------------------------------------}
function ToglSphere.Info(AFloatFormat: String = '0.000'): String;
begin
  Result := '===== Sphere =====' + LineEnding +
            '        Radius: ' + FormatFloat(AFloatFormat, FRadius) + LineEnding +
            '  Sector Count: ' + IntToStr(FSectorCount) + LineEnding +
            '   Stack Count: ' + IntToStr(FStackCount) +  LineEnding +
            'Smooth Shading: ' + BoolToStr(FSmooth, true) + LineEnding +
            'Triangle Count: ' + IntToStr(TriangleCount) + LineEnding +
            '   Index Count: ' + IntToStr(IndexCount) + LineEnding +
            '  Vertex Count: ' + IntToStr(VertexCount) + LineEnding +
            '  Normal Count: ' + IntToStr(NormalCount) + LineEnding +
            'TexCoord Count: ' + IntToStr(TexCoordCount);
end;


{-------------------------------------------------------------------------------
  Setter for radius, sector count, stack count and smooth flag
-------------------------------------------------------------------------------}
procedure ToglSphere.SetParams(ARadius: GLFloat; ASectorCount: Integer;
  AStackCount: Integer; ASmooth: Boolean);
begin
  FRadius := ARadius;
  FSmooth := ASmooth;

  if ASectorCount < MIN_SECTOR_COUNT then
    FSectorCount := MIN_SECTOR_COUNT
  else
    FSectorCount := ASectorCount;

  if AStackCount < MIN_STACK_COUNT then
    FStackCount := MIN_STACK_COUNT
  else
    FStackCount := AStackCount;

  if FSmooth then
    BuildVerticesSmooth
  else
    BuildVerticesFlat;
end;


procedure ToglSphere.SetRadius(const AValue: GLFloat);
begin
  FRadius := AValue;
  UpdateRadius;
end;


procedure ToglSphere.SetSectorCount(const AValue: Integer);
begin
  SetParams(FRadius, AValue, FStackCount, FSmooth);
end;


procedure ToglSphere.SetSmooth(const AValue: Boolean);
begin
  if FSmooth = AValue then exit;
  FSmooth := AValue;
  if FSmooth then
    BuildVerticesSmooth
  else
    BuildVerticesFlat;
end;


procedure ToglSphere.SetStackCount(const AValue: Integer);
begin
  SetParams(FRadius, FSectorCount, AValue, FSmooth);
end;


{-------------------------------------------------------------------------------
  Update vertex positions only
-------------------------------------------------------------------------------}
procedure ToglSphere.UpdateRadius;
var
  i, j: Integer;
  scale: GLFloat;
  count: Integer;
begin
  scale := FRadius / sqrt(sqr(FVertices[0]) +  sqr(FVertices[1]) + sqr(FVertices[2]));
  i := 0;
  j := 0;
  while i < count do begin
    FVertices[i] := FVertices[i] * scale;
    FVertices[i+1] := FVertices[i+1] * scale;
    FVertices[i+2] := FVertices[i+2] * scale;

    // for interleaved array
    FInterleavedVertices[j] := FInterleavedVertices[j]* scale;
    FInterleavedVertices[j+1] := FInterleavedVertices[j+1]* scale;
    FInterleavedVertices[j+2] := FInterleavedVertices[j+2]* scale;

    inc(i, 3);
    inc(j, 8);
  end;
end;


end.

