unit main;

{$mode objfpc}{$H+}
{$define USE_GLUT}
{$.define DRAW_RANGE_ELEMENTS}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  gl, glu, glext, {$ifdef USE_GLUT}glut,{$endif}
  oglTypes,
  OpenGLContext;

type

  { TMainForm }

  TMainForm = class(TForm)
    OpenGLControl: TOpenGLControl;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure OpenGLControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure OpenGLControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControlPaint(Sender: TObject);
    procedure OpenGLControlResize(Sender: TObject);
  private
    FCameraAngleX: GLfloat;
    FCameraAngleY: GLfloat;
    FCameraDistance: GLfloat;
    FDrawMode: Integer;  // 0:fill, 1: wireframe, 2:points
    FMaxVertices: Integer;
    FMaxIndices: Integer;
    FMouseX, FMouseY: Integer;
    FMouseLeftDown, FMouseRightDown: Boolean;
    {$ifdef USE_GLUT}
    FFont: Pointer;
    {$ENDIF}
    FTextHeight: Integer;
    procedure Draw1;
    procedure Draw2;
    procedure Draw3;
    {$ifdef DRAW_RANGE_ELEMENTS}
    procedure Draw4;
    {$endif}
    procedure Draw5;
    procedure DrawInfo;
    procedure DrawString(const AText: String; X, Y: Integer;
      ATextColor: ToglArray4f; AFont: Pointer);
    procedure DrawString3D(const AText: String; ATextPos: ToglArray3f;
      AColor: ToglArray4f; AFont: Pointer);
    procedure InitGL;
    procedure InitLights;
    procedure SetCamera(PosX, PosY, PosZ, TargetX, TargetY, TargetZ: GLfloat);
    procedure ToOrtho;
    procedure ToPerspective;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

const
  CAMERA_DISTANCE = 10.0;

// Cube ------------------------------------------------------------------------
//    v6----- v5
//   /|      /|
//  v1------v0|
//  | |     | |
//  | |v7---|-|v4
//  |/      |/
//  v2------v3

const
  // vertex coords array for glDrawArrays() =====================================
  // A cube has 6 sides and each side has 2 triangles, therefore, a cube consists
  // of 36 vertices (6 sides * 2 tris * 3 vertices = 36 vertices). And, each
  // vertex is 3 components (x,y,z) of floats, therefore, the size of vertex
  // array is 108 floats (36 * 3 = 108).
  VERTICES1: array[0..107] of GLfloat = (
     1, 1, 1,  -1, 1, 1,  -1,-1, 1,      // v0-v1-v2 (front)
    -1,-1, 1,   1,-1, 1,   1, 1, 1,      // v2-v3-v0

     1, 1, 1,   1,-1, 1,   1,-1,-1,      // v0-v3-v4 (right)
     1,-1,-1,   1, 1,-1,   1, 1, 1,      // v4-v5-v0

     1, 1, 1,   1, 1,-1,  -1, 1,-1,      // v0-v5-v6 (top)
    -1, 1,-1,  -1, 1, 1,   1, 1, 1,      // v6-v1-v0

    -1, 1, 1,  -1, 1,-1,  -1,-1,-1,      // v1-v6-v7 (left)
    -1,-1,-1,  -1,-1, 1,  -1, 1, 1,      // v7-v2-v1

    -1,-1,-1,   1,-1,-1,   1,-1, 1,      // v7-v4-v3 (bottom)
     1,-1, 1,  -1,-1, 1,  -1,-1,-1,      // v3-v2-v7

     1,-1,-1,  -1,-1,-1,  -1, 1,-1,      // v4-v7-v6 (back)
    -1, 1,-1,   1, 1,-1,   1,-1,-1       // v6-v5-v4
  );

  // normal array
  NORMALS1: array[0..107] of GLfloat = (
     0, 0, 1,   0, 0, 1,   0, 0, 1,      // v0-v1-v2 (front)
     0, 0, 1,   0, 0, 1,   0, 0, 1,      // v2-v3-v0

     1, 0, 0,   1, 0, 0,   1, 0, 0,      // v0-v3-v4 (right)
     1, 0, 0,   1, 0, 0,   1, 0, 0,      // v4-v5-v0

     0, 1, 0,   0, 1, 0,   0, 1, 0,      // v0-v5-v6 (top)
     0, 1, 0,   0, 1, 0,   0, 1, 0,      // v6-v1-v0

    -1, 0, 0,  -1, 0, 0,  -1, 0, 0,      // v1-v6-v7 (left)
    -1, 0, 0,  -1, 0, 0,  -1, 0, 0,      // v7-v2-v1

     0,-1, 0,   0,-1, 0,   0,-1, 0,      // v7-v4-v3 (bottom)
     0,-1, 0,   0,-1, 0,   0,-1, 0,      // v3-v2-v7

     0, 0,-1,   0, 0,-1,   0, 0,-1,      // v4-v7-v6 (back)
     0, 0,-1,   0, 0,-1,   0, 0,-1       // v6-v5-v4
  );

  // color array
  COLORS1: array[0..107] of GLfloat = (
     1, 1, 1,   1, 1, 0,   1, 0, 0,      // v0-v1-v2 (front)
     1, 0, 0,   1, 0, 1,   1, 1, 1,      // v2-v3-v0

     1, 1, 1,   1, 0, 1,   0, 0, 1,      // v0-v3-v4 (right)
     0, 0, 1,   0, 1, 1,   1, 1, 1,      // v4-v5-v0

     1, 1, 1,   0, 1, 1,   0, 1, 0,      // v0-v5-v6 (top)
     0, 1, 0,   1, 1, 0,   1, 1, 1,      // v6-v1-v0

     1, 1, 0,   0, 1, 0,   0, 0, 0,      // v1-v6-v7 (left)
     0, 0, 0,   1, 0, 0,   1, 1, 0,      // v7-v2-v1

     0, 0, 0,   0, 0, 1,   1, 0, 1,      // v7-v4-v3 (bottom)
     1, 0, 1,   1, 0, 0,   0, 0, 0,      // v3-v2-v7

     0, 0, 1,   0, 0, 0,   0, 1, 0,      // v4-v7-v6 (back)
     0, 1, 0,   0, 1, 1,   0, 0, 1       // v6-v5-v4
  );

  // Vertex array for glDrawElements() and glDrawRangeElement() =================
  // Notice that the sizes of these arrays become samller than the arrays for
  // glDrawArrays() because glDrawElements() uses an additional index array to
  // choose designated vertices with the indices. The size of vertex array is now
  // 24 instead of 36, but the index array size is 36, same as the number of
  // vertices required to draw a cube.
  VERTICES2: array[0..71] of GLfloat = (
     1, 1, 1,  -1, 1, 1,  -1,-1, 1,   1,-1, 1,   // v0,v1,v2,v3 (front)
     1, 1, 1,   1,-1, 1,   1,-1,-1,   1, 1,-1,   // v0,v3,v4,v5 (right)
     1, 1, 1,   1, 1,-1,  -1, 1,-1,  -1, 1, 1,   // v0,v5,v6,v1 (top)
    -1, 1, 1,  -1, 1,-1,  -1,-1,-1,  -1,-1, 1,   // v1,v6,v7,v2 (left)
    -1,-1,-1,   1,-1,-1,   1,-1, 1,  -1,-1, 1,   // v7,v4,v3,v2 (bottom)
     1,-1,-1,  -1,-1,-1,  -1, 1,-1,   1, 1,-1    // v4,v7,v6,v5 (back)
  );

  // normal array
  NORMALS2: array[0..71] of GLfloat = (
     0, 0, 1,   0, 0, 1,   0, 0, 1,   0, 0, 1,   // v0,v1,v2,v3 (front)
     1, 0, 0,   1, 0, 0,   1, 0, 0,   1, 0, 0,   // v0,v3,v4,v5 (right)
     0, 1, 0,   0, 1, 0,   0, 1, 0,   0, 1, 0,   // v0,v5,v6,v1 (top)
    -1, 0, 0,  -1, 0, 0,  -1, 0, 0,  -1, 0, 0,   // v1,v6,v7,v2 (left)
     0,-1, 0,   0,-1, 0,   0,-1, 0,   0,-1, 0,   // v7,v4,v3,v2 (bottom)
     0, 0,-1,   0, 0,-1,   0, 0,-1,   0, 0,-1    // v4,v7,v6,v5 (back)
  );

  // color array
  COLORS2: array[0..71] of GLfloat = (
     1, 1, 1,   1, 1, 0,   1, 0, 0,   1, 0, 1,   // v0,v1,v2,v3 (front)
     1, 1, 1,   1, 0, 1,   0, 0, 1,   0, 1, 1,   // v0,v3,v4,v5 (right)
     1, 1, 1,   0, 1, 1,   0, 1, 0,   1, 1, 0,   // v0,v5,v6,v1 (top)
     1, 1, 0,   0, 1, 0,   0, 0, 0,   1, 0, 0,   // v1,v6,v7,v2 (left)
     0, 0, 0,   0, 0, 1,   1, 0, 1,   1, 0, 0,   // v7,v4,v3,v2 (bottom)
     0, 0, 1,   0, 0, 0,   0, 1, 0,   0, 1, 1    // v4,v7,v6,v5 (back)
  );

  // Index array of vertex array for glDrawElements() & glDrawRangeElement()
  INDICES: array[0..35] of GLubyte = (
     0, 1, 2,   2, 3, 0,      // front
     4, 5, 6,   6, 7, 4,      // right
     8, 9,10,  10,11, 8,      // top
    12,13,14,  14,15,12,      // left
    16,17,18,  18,19,16,      // bottom
    20,21,22,  22,23,20       // back
  );

  // Interleaved vertex array for glDrawElements() & glDrawRangeElements() ======
  // All vertex attributes (position, normal, color) are packed together as a
  // struct or set, for example, ((V,N,C), (V,N,C), (V,N,C),...).
  // It is called an array of struct, and provides better memory locality.
  VERTICES3: array[0..215] of GLfloat = (
     1, 1, 1,   0, 0, 1,   1, 1, 1,              // v0 (front)
    -1, 1, 1,   0, 0, 1,   1, 1, 0,              // v1
    -1,-1, 1,   0, 0, 1,   1, 0, 0,              // v2
     1,-1, 1,   0, 0, 1,   1, 0, 1,              // v3

     1, 1, 1,   1, 0, 0,   1, 1, 1,              // v0 (right)
     1,-1, 1,   1, 0, 0,   1, 0, 1,              // v3
     1,-1,-1,   1, 0, 0,   0, 0, 1,              // v4
     1, 1,-1,   1, 0, 0,   0, 1, 1,              // v5

     1, 1, 1,   0, 1, 0,   1, 1, 1,              // v0 (top)
     1, 1,-1,   0, 1, 0,   0, 1, 1,              // v5
    -1, 1,-1,   0, 1, 0,   0, 1, 0,              // v6
    -1, 1, 1,   0, 1, 0,   1, 1, 0,              // v1

    -1, 1, 1,  -1, 0, 0,   1, 1, 0,              // v1 (left)
    -1, 1,-1,  -1, 0, 0,   0, 1, 0,              // v6
    -1,-1,-1,  -1, 0, 0,   0, 0, 0,              // v7
    -1,-1, 1,  -1, 0, 0,   1, 0, 0,              // v2

    -1,-1,-1,   0,-1, 0,   0, 0, 0,              // v7 (bottom)
     1,-1,-1,   0,-1, 0,   0, 0, 1,              // v4
     1,-1, 1,   0,-1, 0,   1, 0, 1,              // v3
    -1,-1, 1,   0,-1, 0,   1, 0, 0,              // v2

     1,-1,-1,   0, 0,-1,   0, 0, 1,              // v4 (back)
    -1,-1,-1,   0, 0,-1,   0, 0, 0,              // v7
    -1, 1,-1,   0, 0,-1,   0, 1, 0,              // v6
     1, 1,-1,   0, 0,-1,   0, 1, 1               // v5
  );

{ TMainForm }

//------------------------------------------------------------------------------
// Immediate mode
// 78 calls = 36 glVertex*() calls + 36 glColor*() calls + 6 glNormal*() calls
//------------------------------------------------------------------------------
procedure TMainForm.Draw1;
begin
  glPushMatrix();

  glTranslatef(-2, 2, 0); // move to upper left corner

  glBegin(GL_TRIANGLES);
      // front faces
      glNormal3f(0,0,1);
      // face v0-v1-v2
      glColor3f(1,1,1);
      glVertex3f(1,1,1);
      glColor3f(1,1,0);
      glVertex3f(-1,1,1);
      glColor3f(1,0,0);
      glVertex3f(-1,-1,1);
      // face v2-v3-v0
      glColor3f(1,0,0);
      glVertex3f(-1,-1,1);
      glColor3f(1,0,1);
      glVertex3f(1,-1,1);
      glColor3f(1,1,1);
      glVertex3f(1,1,1);

      // right faces
      glNormal3f(1,0,0);
      // face v0-v3-v4
      glColor3f(1,1,1);
      glVertex3f(1,1,1);
      glColor3f(1,0,1);
      glVertex3f(1,-1,1);
      glColor3f(0,0,1);
      glVertex3f(1,-1,-1);
      // face v4-v5-v0
      glColor3f(0,0,1);
      glVertex3f(1,-1,-1);
      glColor3f(0,1,1);
      glVertex3f(1,1,-1);
      glColor3f(1,1,1);
      glVertex3f(1,1,1);

      // top faces
      glNormal3f(0,1,0);
      // face v0-v5-v6
      glColor3f(1,1,1);
      glVertex3f(1,1,1);
      glColor3f(0,1,1);
      glVertex3f(1,1,-1);
      glColor3f(0,1,0);
      glVertex3f(-1,1,-1);
      // face v6-v1-v0
      glColor3f(0,1,0);
      glVertex3f(-1,1,-1);
      glColor3f(1,1,0);
      glVertex3f(-1,1,1);
      glColor3f(1,1,1);
      glVertex3f(1,1,1);

      // left faces
      glNormal3f(-1,0,0);
      // face  v1-v6-v7
      glColor3f(1,1,0);
      glVertex3f(-1,1,1);
      glColor3f(0,1,0);
      glVertex3f(-1,1,-1);
      glColor3f(0,0,0);
      glVertex3f(-1,-1,-1);
      // face v7-v2-v1
      glColor3f(0,0,0);
      glVertex3f(-1,-1,-1);
      glColor3f(1,0,0);
      glVertex3f(-1,-1,1);
      glColor3f(1,1,0);
      glVertex3f(-1,1,1);

      // bottom faces
      glNormal3f(0,-1,0);
      // face v7-v4-v3
      glColor3f(0,0,0);
      glVertex3f(-1,-1,-1);
      glColor3f(0,0,1);
      glVertex3f(1,-1,-1);
      glColor3f(1,0,1);
      glVertex3f(1,-1,1);
      // face v3-v2-v7
      glColor3f(1,0,1);
      glVertex3f(1,-1,1);
      glColor3f(1,0,0);
      glVertex3f(-1,-1,1);
      glColor3f(0,0,0);
      glVertex3f(-1,-1,-1);

      // back faces
      glNormal3f(0,0,-1);
      // face v4-v7-v6
      glColor3f(0,0,1);
      glVertex3f(1,-1,-1);
      glColor3f(0,0,0);
      glVertex3f(-1,-1,-1);
      glColor3f(0,1,0);
      glVertex3f(-1,1,-1);
      // face v6-v5-v4
      glColor3f(0,1,0);
      glVertex3f(-1,1,-1);
      glColor3f(0,1,1);
      glVertex3f(1,1,-1);
      glColor3f(0,0,1);
      glVertex3f(1,-1,-1);
  glEnd();

  glPopMatrix();
end;

//------------------------------------------------------------------------------
// Draw cube at upper-right corner with glDrawArrays
// A cube has only 8 vertices, but each vertex is shared for 3 different faces,
// which have different normals. Therefore, we need more than 8 vertex data to
// draw a cube. Since each face has 2 triangles, we need 6 vertices per face.
// (2 * 3 = 6) And, a cube has 6 faces, so, the total number of vertices for
// drawing a cube is 36 (= 6 faces * 6 vertices).
// Note that there are some duplicated vertex data for glDrawArray() because
// the vertex data in the vertex array must be sequentially placed in memory.
// For a cube, there are 24 unique vertex data and 12 redundant vertex data in
// the vertex array.
//------------------------------------------------------------------------------
procedure TMainForm.Draw2;
begin
  // Enable and specify pointers to vertex arrays
  glEnableClientState(GL_NORMAL_ARRAY);
  glEnableClientState(GL_COLOR_ARRAY);
  glEnableClientState(GL_VERTEX_ARRAY);

  glNormalPointer(GL_FLOAT, 0, @NORMALS1);
  glColorPointer(3, GL_FLOAT, 0, @COLORS1);
  glVertexPointer(3, GL_FLOAT, 0, @VERTICES1);

  glPushMatrix();
  glTranslatef(2, 2, 0);                  // move to upper-right corner

  glDrawArrays(GL_TRIANGLES, 0, 36);

  glPopMatrix();

  // disable vertex arrays
  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_COLOR_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
end;

//------------------------------------------------------------------------------
// Draw cube at bottom-left corner with glDrawElements
// The main advantage of glDrawElements() over glDrawArray() is that
// glDrawElements() allows hopping around the vertex array with the associated
// index values.
// In a cube, the number of vertex data in the vertex array can be reduced to
// 24 vertices for glDrawElements().
// Note that you need an additional array (index array) to store how to traverse
// the vertext data. For a cube, we need 36 entries in the index array.
//------------------------------------------------------------------------------
procedure TMainForm.Draw3;
begin
  // enable and specify pointers to vertex arrays
  glEnableClientState(GL_NORMAL_ARRAY);
  glEnableClientState(GL_COLOR_ARRAY);
  glEnableClientState(GL_VERTEX_ARRAY);

  glNormalPointer(GL_FLOAT, 0, @NORMALS2);
  glColorPointer(3, GL_FLOAT, 0, @COLORS2);
  glVertexPointer(3, GL_FLOAT, 0, @VERTICES2);

  glPushMatrix();
  glTranslatef(-2, -2, 0);                // move to bottom-left corner

  glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_BYTE, @INDICES);

  glPopMatrix();

  glDisableClientState(GL_VERTEX_ARRAY);  // disable vertex arrays
  glDisableClientState(GL_COLOR_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
end;

{$ifdef DRAW_RANGE_ELEMENTS}
//------------------------------------------------------------------------------
// Draw cube at bottom-right corner with glDrawRangeElements()
// glDrawRangeElements() has two more parameters than glDrawElements() needs;
// start and end index. These values specifies a range of vertex data to be
// loaded into OpenGL. "start" param specifies where the range starts from, and
// "end" param specifies where the range ends. All the index values to be drawn
// must be between "start" and "end".
// Note that not all vertices in the range [start, end] will be referenced.
// But, if you specify a sparsely used range, it causes unnecessary process for
// many unused vertices in that range.
//------------------------------------------------------------------------------
procedure TMainForm.Draw4;
begin
  if FOpenGLTooOld then
    exit;

  // enable and specify pointers to vertex arrays
  glEnableClientState(GL_NORMAL_ARRAY);
  glEnableClientState(GL_COLOR_ARRAY);
  glEnableClientState(GL_VERTEX_ARRAY);
  glNormalPointer(GL_FLOAT, 0, @NORMALS2);
  glColorPointer(3, GL_FLOAT, 0, @COLORS2);
  glVertexPointer(3, GL_FLOAT, 0, @VERTICES2);

  glPushMatrix();
  glTranslatef(2, -2, 0);                 // move to bottom-right

  // Draw first half (18 elements){0,1,2, 2,3,0, 4,5,6, 6,7,4, 8,9,10, 10,11,8}
  // The minimum index value in this range is 0, and the maximum index is 11,
  // therefore, "start" param is 0 and "end" param is 11.
  // Then, OpenGL will prefetch only 12 vertex data from the array prior to
  // rendering. (half of total data)
  glDrawRangeElements(GL_TRIANGLES, 0, 11, 18, GL_UNSIGNED_BYTE, @INDICES);

  // Draw last half (18 elements) {12,13,14, 14,15,12, 16,17,18, 18,19,16, 20,21,22, 22,23,20}
  // The minimum index value in this range is 12, and the maximum index is 23,
  // therefore, "start" param is 12 and "end" param is 23.
  // Then, OpenGL will prefetch only 12 vertex data from the array prior to
  // rendering.
  // Note that the last param of glDrawRangeElements(). It is the pointer to
  // the location of the first index value to be drawn.
  glDrawRangeElements(GL_TRIANGLES, 12, 23, 18, GL_UNSIGNED_BYTE, @INDICES+18);

  glPopMatrix();

  glDisableClientState(GL_VERTEX_ARRAY);  // disable vertex arrays
  glDisableClientState(GL_COLOR_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
end;
{$endif}

//------------------------------------------------------------------------------
// Draw cube at bottom-left corner with glDrawElements and interleave array
// All the vertex data (position, normal, colour) can be placed together into a
// single array, and be interleaved like (VNCVNC...). The interleave vertex data
// provides better memory locality.
// Since we are using a single interleaved vertex array to store vertex
// positions, normals and colours, we need to specify "stride" and "pointer"
// parameters properly for glVertexPointer, glNormalPointer and glColorPointer.
// Each vertex has 9 elements of floats (3 position + 3 normal + 3 color), so,
// the stride param should be 36 (= 9 * 4 bytes).
//------------------------------------------------------------------------------
procedure TMainForm.Draw5;
begin
  // enable and specify pointers to vertex arrays
  glEnableClientState(GL_NORMAL_ARRAY);
  glEnableClientState(GL_COLOR_ARRAY);
  glEnableClientState(GL_VERTEX_ARRAY);

  glNormalPointer(GL_FLOAT, 9 * SizeOf(GLfloat), @VERTICES3 + 3);
  glColorPointer(3, GL_FLOAT, 9 * SizeOf(GLfloat), @VERTICES3 + 6);
  glVertexPointer(3, GL_FLOAT, 9 * SizeOf(GLfloat), @VERTICES3);

  glPushMatrix();
  glTranslatef(-2, -2, 0);                // move to bottom-left

  glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_BYTE, @INDICES);

  glPopMatrix();

  glDisableClientState(GL_VERTEX_ARRAY);  // disable vertex arrays
  glDisableClientState(GL_COLOR_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
end;

//------------------------------------------------------------------------------
// Display info messages
//------------------------------------------------------------------------------
procedure TMainForm.DrawInfo;
var
  textColor: array[0..3] of GLfloat = (1.0, 1.0, 1.0, 1.0);
  s: String;
begin
  ToOrtho;

  s := 'Max Elements Vertices: ' + IntToStr(FMaxVertices);
  DrawString(s, 1, OpenGLControl.Height - FTextHeight, textColor, FFont);

  s := 'Max Elements Indices: ' + IntToStr(FMaxIndices);
  DrawString(s, 1, OpenGLControl.Height - 2*FTextHeight, textColor, FFont);

  ToPerspective;
end;

//------------------------------------------------------------------------------
// Write 2d text using GLUT
// The projection matrix must be set to orthogonal before call this function.
//------------------------------------------------------------------------------
procedure TMainForm.DrawString(const AText: String; X, Y: Integer;
  ATextColor: ToglArray4f; AFont: Pointer);
var
  i: Integer;
  ch: Char;
begin
  glPushAttrib(GL_LIGHTING_BIT or GL_CURRENT_BIT); // lighting and color mask
  glDisable(GL_LIGHTING);      // need to disable lighting for proper text color
  glDisable(GL_TEXTURE_2D);

  glColor4fv(ATextColor);      // set text color
  glRasterPos2i(x, y);         // place text position

  {$ifdef USE_GLUT}
  // loop all characters in the string
  for i := 1 to Length(AText) do
  begin
    ch := AText[i];
    glutBitmapCharacter(AFont, Integer(ch));
  end;
  {$endif}

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_LIGHTING);
  glPopAttrib();
end;

//------------------------------------------------------------------------------
// Draw a string in 3D space
//------------------------------------------------------------------------------
procedure TMainForm.DrawString3D(const AText: String; ATextPos: ToglArray3f;
  AColor: ToglArray4f; AFont: Pointer);
var
  i: Integer;
  ch: Char;
begin
  glPushAttrib(GL_LIGHTING_BIT or GL_CURRENT_BIT); // lighting and color mask
  glDisable(GL_LIGHTING);     // need to disable lighting for proper text color
  glDisable(GL_TEXTURE_2D);

  glColor4fv(@AColor);        // set text color
  glRasterPos3fv(ATextPos);   // place text position

  // Draw the text
  {$ifdef USE_GLUT}
  for i := 1 to Length(AText) do
  begin
    ch := AText[i];
    glutBitmapCharacter(AFont, Integer(ch));
  end;
  {$endif}

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_LIGHTING);
  glPopAttrib();
end;

//------------------------------------------------------------------------------
// Initialize OpenGL
// Disable unused features
//------------------------------------------------------------------------------
procedure TMainForm.InitGL;
begin
  glShadeModel(GL_SMOOTH);                    // shading mathod: GL_SMOOTH or GL_FLAT
  glPixelStorei(GL_UNPACK_ALIGNMENT, 4);      // 4-byte pixel alignment

  // Enable/disable features
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  //glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  //glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_CULL_FACE);

  // Track material ambient and diffuse from surface color, call it before glEnable(GL_COLOR_MATERIAL)
  glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_COLOR_MATERIAL);

  glClearColor(0, 0, 0, 0);                   // background color
  glClearStencil(0);                          // clear stencil buffer
  glClearDepth(1.0);                          // 0 is near, 1 is far
  glDepthFunc(GL_LEQUAL);

  InitLights();
end;

//------------------------------------------------------------------------------
// Initialize lights
//------------------------------------------------------------------------------
procedure TMainForm.InitLights();
const
  lightKa: array[0..3] of GLfloat = (0.2, 0.2, 0.2, 1.0);   // ambient light
  lightKd: array[0..3] of GLfloat = (0.7, 0.7, 0.7, 1.0);   // diffuse light
  lightKs: array[0..3] of GLfloat = (1.0, 1.0, 1.0, 1.0);   // specular light
  lightPos: array[0..3] of GLfloat = (0.0, 0.0, 20.0, 1.0); // positional light
begin
  // set up light colors (ambient, diffuse, specular)
  glLightfv(GL_LIGHT0, GL_AMBIENT, @lightKa);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @lightKd);
  glLightfv(GL_LIGHT0, GL_SPECULAR, @lightKs);

  // position the light
  glLightfv(GL_LIGHT0, GL_POSITION, lightPos);

  glEnable(GL_LIGHT0);      // MUST enable each light source after configuration
end;

//------------------------------------------------------------------------------
// Set camera position and lookat direction
//------------------------------------------------------------------------------
procedure TMainForm.SetCamera(PosX, PosY, PosZ, TargetX, TargetY, TargetZ: GLfloat);
begin
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(PosX, PosY, PosZ, TargetX, TargetY, TargetZ, 0, 1, 0);
            // eye(x,y,z)            focal(x,y,z)       up(x,y,z)
end;

//------------------------------------------------------------------------------
// Set projection matrix as orthogonal
//------------------------------------------------------------------------------
procedure TMainForm.ToOrtho;
begin
  // Set viewport to be the entire window
  glViewport(0, 0, OpenGLControl.Width, OpenGLControl.Height);

  // Set orthographic viewing frustum
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(0, OpenGLControl.Width, 0, OpenGLControl.Height, -1, 1);

  // Switch to modelview matrix in order to set scene
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
end;

//------------------------------------------------------------------------------
// Set the projection matrix as perspective
//------------------------------------------------------------------------------
procedure TMainForm.ToPerspective;
begin
  // Set viewport to be the entire window
  glViewport(0, 0, OpenGlControl.Width, OpenGLControl.Height);

  // Set perspective viewing frustum
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(60.0, OpenGLControl.Width/OpenGLControl.Height, 1.0, 1000.0);
  //             FOV,               AspectRatio,           NearClip, FarClip

  // Switch to modelview matrix in order to set scene
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
end;

procedure TMainForm.OpenGLControlPaint(Sender: TObject);
var
  textPos: ToglArray3f = (-4.0, 3.5, 0.0);
  textColor: ToglArray4f = (1.0, 1.0, 1.0, 1.0);
begin
  // Clear buffer
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

  // Save the initial ModelView matrix before modifying ModelView matrix
  glPushMatrix();

  // Transform camera
  glTranslatef(0, 0, -FCameraDistance);
  glRotatef(FCameraAngleX, 1, 0, 0);   // pitch
  glRotatef(FCameraAngleY, 0, 1, 0);   // heading

  Draw1();        // with immediate mode, glBegin()-glEnd() block
  Draw2();        // with glDrawArrays()
  Draw3();        // with glDrawElements()
  //Draw5();        // with glDrawElements() with interleave vertex array
  {$ifdef DRAW_RANGE_ELEMENTS}
  Draw4();        // with glDrawRangeElements()
  {$endif}

  // Print captions (in 3D)
  DrawString3D('Immediate', textPos, textColor, FFont);
  TextPos[0] := 0.5;
  DrawString3D('glDrawArrays()', textPos, textColor, FFont);
  textPos[0] := -5.0;
  textPos[1] := -4.0;
  DrawString3D('glDrawElements()', textPos, textColor, FFont);
  {$ifdef DRAW_RANGE_ELEMENTS}
  if not FOpenGLTooOld then
  begin
    textPos[0] := 0.5;
    DrawString3D('glDrawRangeElements()', textPos, textColor, FFont);
  end;
  {$endif}

  DrawInfo;     // Print max range of glDrawRangeElements

  glPopMatrix();

  OpenGLControl.SwapBuffers();
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FCameraAngleX := 0.0;
  FCameraAngleY := 0.0;
  FCameraDistance := CAMERA_DISTANCE;

  FDrawMode := 0; // 0:fill, 1: wireframe, 2:points
  FMaxVertices := 0;
  FMaxIndices := 0;

  OpenGLControl.MakeCurrent;

  InitGL;

  {$ifdef USE_GLUT}
  FFont := GLUT_BITMAP_8_BY_13;
  FTextHeight := 13;
  {$ENDIF}

  // Check max of elements vertices and elements indices that your video card supports
  // Use these values to determine the range of glDrawRangeElements()
  // The constants are defined in glext.h
  glGetIntegerv(GL_MAX_ELEMENTS_VERTICES, @FMaxVertices);
  glGetIntegerv(GL_MAX_ELEMENTS_INDICES, @FMaxIndices);
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key in ['d', 'D'] then  // switch rendering modes (fill -> wire -> point)
  begin
    FDrawMode := (FDrawMode + 1) mod 3;
    case FDrawMode of
      0: begin // fill mode
           glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
           glEnable(GL_DEPTH_TEST);
           glEnable(GL_CULL_FACE);
           OpenGLControl.Invalidate;
         end;
      1: begin // wireframe mode
           glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
           glDisable(GL_DEPTH_TEST);
           glDisable(GL_CULL_FACE);
           OpenGLControl.Invalidate;
         end;
      2: begin  // point mode
           glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
           glDisable(GL_DEPTH_TEST);
           glDisable(GL_CULL_FACE);
           OpenGLControl.Invalidate;
         end;
      else
         ;
    end;
  end;
end;

procedure TMainForm.OpenGLControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseX := X;
  FMouseY := Y;

  if (Button = mbLeft) then
    FMouseLeftDown := true
  else
  if (Button = mbRight) then
    FMouseRightDown := true;
end;

procedure TMainForm.OpenGLControlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if FMouseLeftDown then
  begin
    FCameraAngleY := FCameraAngleY + (X - FMouseX);
    FCameraAngleX := FCameraAngleX + (Y - FMouseY);
    FMouseX := x;
    FMouseY := y;
    OpenGLControl.Invalidate;
  end;

  if FMouseRightDown then
  begin
    FCameraDistance := FCameraDistance - (Y - FMouseY) * 0.2;
    FMouseY := y;
    OpenGLControl.Invalidate;
  end;
end;

procedure TMainForm.OpenGLControlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
    FMouseLeftDown := false
  else
  if (Button = mbRight) then
    FMouseRightDown := false;
end;

procedure TMainForm.OpenGLControlResize(Sender: TObject);
begin
  ToPerspective();
end;

end.

