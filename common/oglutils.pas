unit oglUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, Graphics, GL;

type
  TGlyphMetricsArray = array[0..255] of GLYPHMETRICSFLOAT;
  PGlyphMetricsArray = ^TGlyphMetricsArray;

  ToglFont = record
    Base: GLuint;                      // Display list number of first character
    Widths: array[0..255] of Integer;  // Width of each character in pixels
    Height: Integer;                   // Height of characters
    Metrics: PGlyphMetricsArray;
  end;

// OpenGL Font Library
function  oglCreateBitmapFont(DC: HDC; AFontName: string; AHeight: integer;
  AFontStyle: TFontStyles=[]): ToglFont;
function oglCreateOutlineFont(DC: HDC; AFontName: string; AHeight: integer;
  AFontStyle: TFontStyles=[]): ToglFont;
procedure oglDestroyFont(AFont:ToglFont);
procedure oglTextSize(AFont: ToglFont; const AText: ansistring;
  var AWidth, AHeight: integer);
procedure oglTextOut(AFont: ToglFont; x,y:integer; const AText: ansistring);

// Export to bitmap
procedure oglToBitmap(ABitmap: TBitmap);

// Primitives as display lists

// Sphere with unit radius at the origin
procedure CreateSphereDisplayList(AListID, ANumLatitudes, ANumLongitudes: Integer);
// Cube with coordinates at -1 and +1, located at origin
procedure CreateCubeDisplayList(AListID: Integer);


implementation

uses
  Math, GraphType, IntfGraphics;

procedure oglToBitmap(ABitmap: TBitmap);
const
  GL_BGRA = $80E1;
var
  intfImg: TLazIntfImage;
  viewport: array[0..3] of GLInt;
  rawImg: TRawImage;
begin
  if ABitmap = nil then
    raise Exception.Create('[OpenGLToBitmap] Bitmap must not be nil.');

  // Query size of the viewport
  glGetIntegerv(GL_VIEWPORT, @viewport);

  // Prepare a raw image
  rawImg.Init;
  rawImg.Description.Init_BPP32_B8G8R8A8_M1_BIO_TTB(viewport[2], viewport[3]);
  rawImg.Description.LineOrder := riloBottomToTop;
  rawImg.CreateData(false);

  // Query image data from OpenGL
  glReadPixels(0, 0, viewport[2], viewport[3], GL_BGRA, GL_UNSIGNED_BYTE, rawImg.Data);

  // Create LazIntfImage from raw image
  intfImg := TLazIntfImage.Create(viewport[2], viewport[3]);
  try
    intfImg.SetRawImage(rawImg);

    // Convert LazIntfImage to Bitmap
    ABitmap.LoadFromIntfImage(intfImg);
  finally
    intfImg.Free;
  end;
end;


//------------------------------------------------------------------------------
//                         OpenGL Font Library
// Ref.: groups.google.de/group/borland.public.cppbuilder.graphics/browse_frm/thread/e652559fd6d8ab20/6d71327f9b224ef4?lnk=st&q=opengl+change+font&rnum=17#6d71327f9b224ef4
//------------------------------------------------------------------------------

function oglCreateOutlineFont(DC: HDC; AFontName: string; AHeight: integer;
  AFontStyle: TFontStyles=[]): ToglFont;
{ Load Windows font bitmaps into OpenGL display lists
  - DC ............ Device Context (input)
  - AFontName ..... Font specification, name (input)
  - AHeight ....... Font height/size, in pixels
  - AFontStyle .... Font style a la Delphy TFont }
var
  fontID: HFONT;
  charset: integer;
begin
  // Allocate display lists
  result.base := glGenLists(256);
  if result.base=0 then begin
    FillChar(result, SizeOf(ToglFont), 0);
    exit;
  end;
  // Select a character set
  if (CompareText(AFontName, 'symbol')=0)
     then charset := SYMBOL_CHARSET
     else charset := ANSI_CHARSET;
  // Load the font
  fontid := CreateFont(
    AHeight,                            // Height of Font
    0,                                  // Width of Font
    0,                                  // Angle of Escapement
    0,                                  // Orientation Angle
    700*byte(fsBold in AFontStyle),     // Schrift Fett?
    byte(fsItalic in AFontStyle),       // Schrift Kursiv?
    byte(fsUnderline in AFontStyle),    // Unterstrichen?
    byte(fsStrikeout in AFontStyle),    // Durchgestrichen?
    charset,                            // Character Set Identifier
    OUT_TT_PRECIS,                      // Output Precision
    CLIP_DEFAULT_PRECIS,                // Clipping Precision
    ANTIALIASED_QUALITY,                // Output Quality --> or: DRAFT_QUALITY
    FF_DONTCARE or DEFAULT_PITCH,       // Family And Pitch
    PChar(AFontName));                  // Font Name
  SelectObject(DC, fontid);
  // Create bitmaps for each character
  GetMem(result.metrics, 256*SizeOf(GLYPHMETRICSFLOAT));
  wglUseFontOutlines(DC,                // Select The Current DC
    0,                                  // Starting Character
    255,                                // Number Of Display Lists To Build
    result.base,                        // Starting Display Lists
    0.0,                                // Deviation From The True Outlines
    0.2,                                // Font Thickness In The Z Direction
    WGL_FONT_POLYGONS,                  // Use Polygons, Not Lines
    pointer(result.metrics));           // Address Of Buffer To Recieve Data
  GetCharWidth(DC, 0, 255, result.Widths);
  result.Height := AHeight;
end;


function oglCreateBitmapFont(DC:HDC; AFontName:string; AHeight:integer;
  AFontStyle:TFontStyles=[]): ToglFont;
{ Load Windows font bitmaps into OpenGL display lists
  - DC ............ Device Context (input)
  - AFontName ..... Font specification, name (input)
  - AHeight ....... Font height/size, in pixels
  - AFontStyle .... Font style a la Delphy TFont }
var
  fontID : HFONT;
  charset : integer;
begin
  // Allocate display lists
  result.base := glGenLists(256);
  if result.base=0 then begin
    FillChar(result, SizeOf(ToglFont), 0);
    exit;
  end;
  // Select a character set
  if (CompareText(AFontName, 'symbol')=0)
     then charset := SYMBOL_CHARSET
     else charset := ANSI_CHARSET;
  // Load the font
  fontid := CreateFont(
    AHeight,                            // Height of Font
    0,                                  // Width of Font
    0,                                  // Angle of Escapement
    0,                                  // Orientation Angle
    700*byte(fsBold in AFontStyle),     // Schrift Fett?
    byte(fsItalic in AFontStyle),       // Schrift Kursiv?
    byte(fsUnderline in AFontStyle),    // Unterstrichen?
    byte(fsStrikeout in AFontStyle),    // Durchgestrichen?
    charset,                            // Character Set Identifier
    OUT_TT_PRECIS,                      // Output Precision
    CLIP_DEFAULT_PRECIS,                // Clipping Precision
    ANTIALIASED_QUALITY,                // Output Quality --> or: DRAFT_QUALITY
    FF_DONTCARE or DEFAULT_PITCH,       // Family And Pitch
    PChar(AFontName));                  // Font Name
  SelectObject(DC, fontid);
  // Create bitmaps for each character
  wglUseFontBitmaps(DC, 0,256, result.base);
  // Get width and height information for each character
  GetCharWidth(DC, 0, 255, result.Widths);
  result.Height := AHeight;
  result.Metrics := nil;
end;


procedure oglDestroyFont(AFont:ToglFont);
// Delete the specified font
begin
  if AFont.base <> 0 then begin
    if AFont.metrics <> nil then
      FreeMem(AFont.metrics, 256*SizeOf(GLYPHMETRICSFLOAT));
    glDeleteLists(AFont.base, 256);
    AFont.Base := 0;
  end;
end;


procedure oglTextOut(AFont:ToglFont; x,y:integer; const AText:ansistring);
{ Display a string using the specified font
  - AFont .... Font to use
  - AText .... String to display }
begin
  if not ((AFont.base=0) or (AText = '')) then begin
    glPushAttrib(GL_LIST_BIT);
      glRasterPos2i(X, Y);
      glListBase(AFont.Base);
      glCallLists(Length(AText), GL_UNSIGNED_BYTE, PAnsiChar(AText));
    glPopAttrib;
  end;
end;


procedure oglTextSize(AFont:ToglFont; const AText:ansistring;
  var AWidth,AHeight:integer);
var
  i : integer;
  L : integer;
begin
  AWidth := 0;
  AHeight := 0;
  L := 0;
  if AText <> '' then begin
    AHeight := AFont.Height;
    for i:=1 to Length(AText) do begin
      if AText[i]=#13 then begin
        AHeight := AHeight + AFont.Height;
        if L > AWidth then AWidth := L;
        L := 0;
      end else begin
        L := L + AFont.Widths[ord(AText[i])];
        if L > AWidth then AWidth := L;
      end;
    end;
  end;
end;


{ Creates a display list for a cube of -1...+1 at origin }
procedure CreateCubeDisplayList(AListID: Integer);
begin
  glNewList(AListID, GL_COMPILE);

    glBegin(GL_POLYGON);
      glColor3f(1, 0, 0);
      glNormal3f(0.0, 0.0, 1.0);
      glVertex3f(1.0, 1.0, 1.0);
      glVertex3f(-1.0, 1.0, 1.0);
      glVertex3f(-1.0, -1.0, 1.0);
      glVertex3f(1.0, -1.0, 1.0);
    glEnd;

    glBegin(GL_POLYGON);
      glColor3f(0, 1, 0);
      glNormal3f(0.0, 0.0, -1.0);
      glVertex3f(1.0, 1.0, -1.0);
      glVertex3f(1.0, -1.0, -1.0);
      glVertex3f(-1.0, -1.0, -1.0);
      glVertex3f(-1.0, 1.0, -1.0);
    glEnd;

    glBegin(GL_POLYGON);
      glColor3f(0, 0, 1);
      glNormal3f(-1.0, 0.0, 0.0);
      glVertex3f(-1.0, 1.0, 1.0);
      glVertex3f(-1.0, 1.0, -1.0);
      glVertex3f(-1.0, -1.0, -1.0);
      glVertex3f(-1.0, -1.0, 1.0);
    glEnd;

    glBegin(GL_POLYGON);
      glColor3f(1, 1, 0);
      glNormal3f(1.0, 0.0, 0.0);
      glVertex3f(1.0, 1.0, 1.0);
      glVertex3f(1.0, -1.0, 1.0);
      glVertex3f(1.0, -1.0, -1.0);
      glVertex3f(1.0, 1.0, -1.0);
    glEnd;

    glBegin(GL_POLYGON);
      glColor3f(1, 0, 1);
      glNormal3f(0.0, 1.0, 0.0);
      glVertex3f(-1.0, 1.0, -1.0);
      glVertex3f(-1.0, 1.0, 1.0);
      glVertex3f(1.0, 1.0, 1.0);
      glVertex3f(1.0, 1.0, -1.0);
    glEnd;

    glBegin(GL_POLYGON);
      glColor3f(0, 1, 1);
      glNormal3f(0.0, -1.0, 0.0);
      glVertex3f(-1.0, -1.0, -1.0);
      glVertex3f(1.0, -1.0, -1.0);
      glVertex3f(1.0, -1.0, 1.0);
      glVertex3f(-1.0, -1.0, 1.0);
    glEnd;

  glEndList;
end;


{ Creates a display list for a sphere with radius 1 at position (0, 0, 0).

  The coordinates of a point x,y,z on the sphere surface are, given in
  spherical coordinates, theta and phi:

  x = sin(theta) * cos(phi)
  y = sin(theta) * sin(phi)
  z = cos(theta)

  Unlike in geometry, the polar angle theta is assumed to run from
  north pole to south pole. }
procedure CreateSphereDisplayList(AListID, ANumLatitudes, ANumLongitudes: Integer);
const
  TWO_PI = 2.0 * pi;
var
  latitudeStep, longitudeStep: double;
  i, j: Integer;
  x, y, z: Double;
  theta0, sin_theta0, cos_theta0: Double;
  theta1, sin_theta1, cos_theta1: Double;
  phi, sin_phi, cos_phi: Double;
begin
  latitudeStep := pi / ANumLatitudes;
  longitudeStep  := TWO_PI / ANumLongitudes;

  glNewList(AListID, GL_COMPILE);
  for i := 0 to ANumLatitudes do begin
    theta0 := i * latitudeStep;
    theta1 := theta0 + latitudeStep;
    SinCos(theta0, sin_theta0, cos_theta0);
    SinCos(theta1, sin_theta1, cos_theta1);

    glBegin(GL_TRIANGLE_STRIP);
    for j := 0 to ANumLongitudes do begin
      phi := j * longitudeStep;
      SinCos(phi, sin_phi, cos_phi);

      x := sin_theta0 * cos_phi;
      y := sin_theta0 * sin_phi;
      z := cos_theta0;
      glNormal3f(x, z, y);
      glVertex3f(x, z, y);

      x := sin_theta1 * cos_phi;
      y := sin_theta1 * sin_phi;
      z := cos_theta1;
      glNormal3f(x, z, y);
      glVertex3f(x, z, y);
    end;
    glEnd();
  end;

  glEndList;
end;

end.


