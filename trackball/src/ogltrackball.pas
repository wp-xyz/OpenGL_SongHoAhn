///////////////////////////////////////////////////////////////////////////////
// Trackball class
//
// This class takes the current mouse cursor position (x,y), and map it to the
// point (x,y,z) on the trackball(sphere) surface. Since the cursor point is in
// screen space, this class depends on the current screen width and height in
// order to compute the vector on the sphere.
//
// There are 2 modes (Arc and Project) to compute the vector on the sphere: Arc
// mode is that the length of the mouse position from the centre of screen
// becomes arc length moving on the sphere, and Project mode is that directly
// projects the mouse position to the sphere.
//
// The default mode is Arc because it allows negative z-value (a point on the
// back of the sphere). On the other hand, Project mode is limited to
// front hemisphere rotation (z-value is always positive).
//
//  AUTHOR: Song Ho Ahn (song.ahn@gmail.com)
// CREATED: 2011-12-09
// UPDATED: 2016-04-01
//
// Copyright (C) 2011. Song Ho Ahn
///////////////////////////////////////////////////////////////////////////////

unit oglTrackball;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gl, vectors;

type
  TTrackballMode = (tmArc, tmProject);

  TTrackball = class
  private
    FRadius: GLfloat;
    FScreenWidth: Integer;
    FScreenHeight: Integer;
    FHalfScreenWidth: GLfloat;
    FHalfScreenHeight: GLfloat;
    FMode: TTrackballMode;
    procedure SetScreenHeight(h: Integer);
    procedure SetScreenWidth(w: Integer);
  protected
    function ClampX(x: GLfloat): GLfloat;
    function ClampY(y: GLfloat): GLfloat;
    function GetVectorWithArc(x, y: GLfloat): TVector3;
    function GetVectorWithProject(x, y: GLfloat): TVector3;
  public
    constructor Create(ARadius: GLfloat; AWidth, AHeight: Integer);

    // return a point on sphere for given mouse (x,y)
    function GetVector(x, y: Integer): TVector3;
    // return normalized point for mouse (x, y)
    function GetUnitVector(x, y: Integer): TVector3;
    function Info(AFloatFormat: String = '0.000'): String;
    procedure SetParams(ARadius: GLfloat; AWidth, AHeight: Integer);


    property Mode: TTrackballMode read FMode write FMode;
    property Radius: GLfloat read FRadius write FRadius;
    property ScreenHeight: Integer read FScreenHeight write SetScreenHeight;
    property ScreenWidth: Integer read FScreenWidth write SetScreenWidth;
  end;

implementation

uses
  Math;

function IfThen(b: Boolean; i, j: Integer): Integer;
begin
  if b then Result := i else result := j;
end;

{-------------------------------------------------------------------------------
  Constructor
-------------------------------------------------------------------------------}
constructor TTrackball.Create(ARadius: GLfloat; AWidth, AHeight: Integer);
begin
  SetParams(ARadius, AWidth, AHeight);
  FMode := tmArc;
end;


{-------------------------------------------------------------------------------
  Clamp x and y
  The params of getVector() must be inside the window: -half < (x,y) < +half
-------------------------------------------------------------------------------}
function TTrackBall.ClampX(x: GLfloat): GLfloat;
begin
  if x <= -FHalfScreenWidth then
    x := -FHalfScreenWidth + 1
  else
  if x >= FHalfScreenWidth then
    x := FHalfScreenWidth - 1;
  Result := x;
end;

function TTrackball.ClampY(y: GLfloat): GLfloat;
begin
  if y <= -FHalfScreenHeight then
    y := -FHalfScreenHeight + 1
  else
  if y >= FHalfScreenHeight then
    y := FHalfScreenHeight - 1;
  Result := y;
end;


{-------------------------------------------------------------------------------
  Returns the point on the sphere as a unit vector
-------------------------------------------------------------------------------}
function TTrackBall.GetUnitVector(x, y: Integer): TVector3;
begin
  Result := GetVector(x, y);
  Result.Normalize;
end;


{-------------------------------------------------------------------------------
  Returns the point corrds on the sphere
-------------------------------------------------------------------------------}
function TTrackBall.GetVector(x, y: Integer): TVector3;
var
  mx, my: GLfloat;
begin
  if (FRadius = 0) or (FScreenWidth = 0) or (FScreenHeight = 0) then begin
    Result := Vector3(0, 0, 0);
    exit;
  end;

  // compute mouse position from the centre of screen (-half ~ +half)
  mx := x - FHalfScreenWidth;
  my := FHalfScreenHeight - y;    // OpenGL uses bottom to up orientation
  // mx := ClampX(x - FHalfScreenWidth);
  // my := ClampY(FHalfScreenHeight - y);    // OpenGL uses bottom to up orientation

  if FMode = tmProject then
    Result := GetVectorWithProject(mx, my)
  else
    Result := GetVectorWithArc(mx, my); // default mode
end;


{-------------------------------------------------------------------------------
  Use the mouse distance from the centre of screen as arc length on the sphere
    x = R * sin(a) * cos(b)
    y = R * sin(a) * sin(b)
    z = R * cos(a)
  where a = angle on x-z plane, b = angle on x-y plane

  NOTE: the calculation of arc length is an estimation using linear distance
  from screen center (0,0) to the cursor position
-------------------------------------------------------------------------------}
function TTrackball.GetVectorWithArc(x, y: GLfloat): TVector3;
var
  arc: GLfloat;
  a, b, x2: GLfloat;
begin
  arc := sqrt(x*x + y*y);      // length between cursor and screen center
  a := arc / FRadius;          // arc = r * a
  b := arctan2(y, x);          // angle on x-y plane
  x2 := FRadius * sin(a);      // x rotated by "a" on x-z plane

  Result.x := x2 * cos(b);
  Result.y := x2 * sin(b);
  Result.z := FRadius * cos(a);
end;


{-------------------------------------------------------------------------------
  Project the mouse coords to the sphere to find the point coord
  return the point on the sphere using hyperbola where x^2 + y^2 > r^2/2
-------------------------------------------------------------------------------}
function TTrackball.GetVectorWithProject(x, y: GLfloat): TVector3;
var
  vec: TVector3;
  d: GLfloat;
  rr: GLfloat;
  x2, y2, a: GLfloat;
begin
  vec := Vector3(x, y, 0);
  d := x*x + y*y;
  rr := FRadius * FRadius;

  if d <= 0.5*rr then
    // use sphere if d<=0.5*r^2:  z = sqrt(r^2 - (x^2 + y^2))
    vec.z := sqrt(rr - d)
  else
  // use hyperbolic sheet if d > 0.5*r^2:  z = (r^2 / 2) / sqrt(x^2 + y^2)
  // referenced from trackball.c by Gavin Bell at SGI
  begin
    // compute z first using hyperbola
    vec.z := 0.5 * rr / sqrt(d);

    // Scale x and y down so that the vector can be on the sphere
    // y = ax => x^2 + (ax)^2 + z^2 = r^2 => (1 + a^2)*x^2 = r^2 - z^2
    // => x = sqrt((r^2 - z^2) / (1 - a^2)
    if(x = 0.0) then begin   // avoid dividing by 9
      x2 := 0.0;
      y2 := sqrt(rr - vec.z*vec.z);
      if y < 0  then        // correct sign
        y2 := -y2;
    end else begin
      a := y / x;
      x2 := sqrt((rr - vec.z*vec.z) / (1 + a*a));
      if x < 0 then        // correct sign
        x2 := -x2;
      y2 := a * x2;
    end;

    vec.x := x2;
    vec.y := y2;
  end;
end;


{-------------------------------------------------------------------------------
  Create parameter info as string
-------------------------------------------------------------------------------}
function TTrackball.Info(AFloatFormat: String = '0.000'): String;
var
  s: String;
begin
  if FMode = tmARC then s := 'ARC' else s := 'PROJECT';
  Result := '===== Trackball =====' + LineEnding +
            '     Radius: ' + FormatFloat(AFloatFormat, FRadius) + LineEnding +
            'Screen Size: (' + IntToStr(FScreenWidth) + ', ' + IntToStr(FScreenHeight) + ')' + LineEnding +
            '       Mode: ' + s;
end;


{-------------------------------------------------------------------------------
  Setters
-------------------------------------------------------------------------------}
procedure TTrackball.SetParams(ARadius: GLFloat; AWidth, AHeight: Integer);
begin
  FRadius := ARadius;
  FScreenWidth := AWidth;
  FScreenHeight := AHeight;
  FHalfScreenWidth := AWidth * 0.5;
  FHalfScreenHeight := AHeight * 0.5;
end;

procedure TTrackball.SetScreenHeight(h: Integer);
begin
  FScreenHeight := h;
  FHalfScreenHeight := h * 0.5;
end;

procedure TTrackball.SetScreenWidth(w: Integer);
begin
  FScreenWidth := w;
  FHalFScreenWidth := w * 0.5;
end;

end.

