///////////////////////////////////////////////////////////////////////////////
// cameraSimple.h
// ==============
// A simple camera model (192 triangles)
// Use "drawCamera()" to draw this model.
//
// 3D model is converted by the PolyTrans from Okino Computer Graphics, Inc.
// Bounding box of geometry = (-0.5,-0.35,-0.3) to (0.5,0.37,0.3).
//
//  AUTHOR: Song Ho Ahn (song.ahn@gmail.com)
// CREATED: 2008-09-18
// UPDATED: 2018-03-02
///////////////////////////////////////////////////////////////////////////////

unit oglCameraModel;

{$mode ObjFPC}{$H+}

interface

uses
  gl, oglTypes;

procedure DrawCamera;

implementation

// vertices for camera
const
  CAMERA_VERTICES: array[0..614] of GLfloat = (
    0.500000, -0.350000,  0.000000,  0.500000, -0.350000,  0.000000, 0.500000, -0.350000, 0.000000,
   -0.500000, -0.350000,  0.000000, -0.500000, -0.350000,  0.000000,
   -0.500000, -0.350000,  0.000000, -0.500000,  0.350000,  0.000000,
   -0.500000,  0.350000,  0.000000, -0.500000,  0.350000,  0.000000,
    0.500000,  0.350000,  0.000000,  0.500000,  0.350000,  0.000000,
    0.500000,  0.350000,  0.000000, -0.500000,  0.350000,  0.300000,
   -0.500000,  0.350000,  0.300000, -0.500000,  0.350000,  0.300000,
    0.500000,  0.350000,  0.300000,  0.500000,  0.350000,  0.300000,
    0.500000,  0.350000,  0.300000, -0.500000, -0.350000,  0.300000,
   -0.500000, -0.350000,  0.300000, -0.500000, -0.350000,  0.300000,
    0.500000, -0.350000,  0.300000,  0.500000, -0.350000,  0.300000,
    0.500000, -0.350000,  0.300000, -0.285317,  0.092705,  0.000000,
   -0.242705,  0.176336,  0.000000, -0.242705,  0.176336, -0.300000,
   -0.242705,  0.176336, -0.300000, -0.285317,  0.092705, -0.300000,
   -0.285317,  0.092705, -0.300000, -0.176336,  0.242705,  0.000000,
   -0.176336,  0.242705, -0.300000, -0.176336,  0.242705, -0.300000,
   -0.092705,  0.285317,  0.000000, -0.092705,  0.285317, -0.300000,
   -0.092705,  0.285317, -0.300000,  0.000000,  0.300000,  0.000000,
    0.000000,  0.300000, -0.300000,  0.000000,  0.300000, -0.300000,
    0.092705,  0.285317,  0.000000,  0.092705,  0.285317, -0.300000,
    0.092705,  0.285317, -0.300000,  0.176336,  0.242705,  0.000000,
    0.176336,  0.242705, -0.300000,  0.176336,  0.242705, -0.300000,
    0.242705,  0.176336,  0.000000,  0.242705,  0.176336, -0.300000,
    0.242705,  0.176336, -0.300000,  0.285317,  0.092705,  0.000000,
    0.285317,  0.092705, -0.300000,  0.285317,  0.092705, -0.300000,
    0.300000,  0.000000,  0.000000,  0.300000,  0.000000, -0.300000,
    0.300000,  0.000000, -0.300000,  0.285317, -0.092705,  0.000000,
    0.285317, -0.092705, -0.300000,  0.285317, -0.092705, -0.300000,
    0.242705, -0.176336,  0.000000,  0.242705, -0.176336, -0.300000,
    0.242705, -0.176336, -0.300000,  0.176336, -0.242705,  0.000000,
    0.176336, -0.242705, -0.300000,  0.176336, -0.242705, -0.300000,
    0.092705, -0.285317,  0.000000,  0.092705, -0.285317, -0.300000,
    0.092705, -0.285317, -0.300000,  0.000000, -0.300000,  0.000000,
    0.000000, -0.300000, -0.300000,  0.000000, -0.300000, -0.300000,
   -0.092705, -0.285317,  0.000000, -0.092705, -0.285317, -0.300000,
   -0.092705, -0.285317, -0.300000, -0.176336, -0.242705,  0.000000,
   -0.176336, -0.242705, -0.300000, -0.176336, -0.242705, -0.300000,
   -0.242705, -0.176336,  0.000000, -0.242705, -0.176336, -0.300000,
   -0.242705, -0.176336, -0.300000, -0.285317, -0.092705,  0.000000,
   -0.285317, -0.092705, -0.300000, -0.285317, -0.092705, -0.300000,
   -0.300000,  0.000000,  0.000000, -0.300000,  0.000000, -0.300000,
   -0.300000,  0.000000, -0.300000, -0.194164,  0.141069, -0.300000,
   -0.194164,  0.141069, -0.300000, -0.228254,  0.074164, -0.300000,
   -0.228254,  0.074164, -0.300000, -0.141069,  0.194164, -0.300000,
   -0.141069,  0.194164, -0.300000, -0.074164,  0.228254, -0.300000,
   -0.074164,  0.228254, -0.300000,  0.000000,  0.240000, -0.300000,
    0.000000,  0.240000, -0.300000,  0.074164,  0.228254, -0.300000,
    0.0741640, 0.228254, -0.300000,  0.141069,  0.194164, -0.300000,
    0.141069,  0.194164, -0.300000,  0.194164,  0.141069, -0.300000,
    0.194164,  0.141069, -0.300000,  0.228254,  0.074164, -0.300000,
    0.228254,  0.074164, -0.300000,  0.240000,  0.000000, -0.300000,
    0.240000,  0.000000, -0.300000,  0.228254, -0.074164, -0.300000,
    0.228254, -0.074164, -0.300000,  0.194164, -0.141069, -0.300000,
    0.194164, -0.141069, -0.300000,  0.141069, -0.194164, -0.300000,
    0.141069, -0.194164, -0.300000,  0.074164, -0.228254, -0.300000,
    0.074164, -0.228254, -0.300000,  0.000000, -0.240000, -0.300000,
    0.000000, -0.240000, -0.300000, -0.074164, -0.228254, -0.300000,
   -0.074164, -0.228254, -0.300000, -0.141068, -0.194164, -0.300000,
   -0.141068, -0.194164, -0.300000, -0.194164, -0.141068, -0.300000,
   -0.194164, -0.141068, -0.300000, -0.228254, -0.074164, -0.300000,
   -0.228254, -0.074164, -0.300000, -0.240000,  0.000000, -0.300000,
   -0.240000,  0.000000, -0.300000, -0.228254,  0.074164,  0.000000,
   -0.194164,  0.141069,  0.000000, -0.141069,  0.194164,  0.000000,
   -0.074164,  0.228254,  0.000000,  0.000000,  0.240000,  0.000000,
    0.074164,  0.228254,  0.000000,  0.141069,  0.194164,  0.000000,
    0.194164,  0.141069,  0.000000,  0.228254,  0.074164,  0.000000,
    0.240000,  0.000000,  0.000000,  0.228254, -0.074164,  0.000000,
    0.194164, -0.141069,  0.000000,  0.141069, -0.194164,  0.000000,
    0.074164, -0.228254,  0.000000,  0.000000, -0.240000,  0.000000,
   -0.074164, -0.228254,  0.000000, -0.141068, -0.194164,  0.000000,
   -0.194164, -0.141068,  0.000000, -0.228254, -0.074164,  0.000000,
   -0.240000,  0.000000,  0.000000,  0.306365,  0.350000,  0.164697,
    0.313467,  0.350000,  0.178636,  0.313467,  0.370000,  0.178636,
    0.313467,  0.370000,  0.178636,  0.306365,  0.370000,  0.164697,
    0.306365,  0.370000,  0.164697,  0.324529,  0.350000,  0.189697,
    0.324529,  0.370000,  0.189697,  0.324529,  0.370000,  0.189697,
    0.338467,  0.350000,  0.196799,  0.338467,  0.370000,  0.196799,
    0.338467,  0.370000,  0.196799,  0.353918,  0.350000,  0.199246,
    0.353918,  0.370000,  0.199246,  0.353918,  0.370000,  0.199246,
    0.369369,  0.350000,  0.196799,  0.369369,  0.370000,  0.196799,
    0.369369,  0.370000,  0.196799,  0.383307,  0.350000,  0.189697,
    0.383307,  0.370000,  0.189697,  0.383307,  0.370000,  0.189697,
    0.394369,  0.350000,  0.178636,  0.394369,  0.370000,  0.178636,
    0.394369,  0.370000,  0.178636,  0.401471,  0.350000,  0.164697,
    0.401471,  0.370000,  0.164697,  0.401471,  0.370000,  0.164697,
    0.403918,  0.350000,  0.149246,  0.403918,  0.370000,  0.149246,
    0.403918,  0.370000,  0.149246,  0.401471,  0.350000,  0.133795,
    0.401471,  0.370000,  0.133795,  0.401471,  0.370000,  0.133795,
    0.394369,  0.350000,  0.119857,  0.394369,  0.370000,  0.119857,
    0.394369,  0.370000,  0.119857,  0.383307,  0.350000,  0.108795,
    0.383307,  0.370000,  0.108795,  0.383307,  0.370000,  0.108795,
    0.369369,  0.350000,  0.101693,  0.369369,  0.370000,  0.101693,
    0.369369,  0.370000,  0.101693,  0.353918,  0.350000,  0.099246,
    0.353918,  0.370000,  0.099246,  0.353918,  0.370000,  0.099246,
    0.338467,  0.350000,  0.101693,  0.338467,  0.370000,  0.101693,
    0.338467,  0.370000,  0.101693,  0.324529,  0.350000,  0.108795,
    0.324529,  0.370000,  0.108795,  0.324529,  0.370000,  0.108795,
    0.313467,  0.350000,  0.119857,  0.313467,  0.370000,  0.119857,
    0.313467,  0.370000,  0.119857,  0.306365,  0.350000,  0.133795,
    0.306365,  0.370000,  0.133795,  0.306365,  0.370000,  0.133795,
    0.303918,  0.350000,  0.149246,  0.303918,  0.370000,  0.149246,
    0.303918,  0.370000,  0.149246,  0.353918,  0.370000,  0.149246
  );

  CAMERA_NORMALS: array[0..613] of GLfloat = (
    1.00000,   0.000000,  0.000000,  0.000000, -1.000000,  0.000000, 0.000000, 0.000000, -1.00000,
   -1.000000,  0.000000,  0.000000,  0.000000, -1.000000,  0.000000,
    0.000000,  0.000000, -1.000000, -1.000000,  0.000000,  0.000000,
    0.000000,  1.000000,  0.000000,  0.000000,  0.000000, -1.000000,
    1.000000,  0.000000,  0.000000,  0.000000,  1.000000,  0.000000,
    0.000000,  0.000000, -1.000000, -1.000000,  0.000000,  0.000000,
    0.000000,  0.000000,  1.000000,  0.000000,  1.000000,  0.000000,
    1.000000,  0.000000,  0.000000,  0.000000,  0.000000,  1.000000,
    0.000000,  1.000000,  0.000000, -1.000000,  0.000000,  0.000000,
    0.000000, -1.000000,  0.000000,  0.000000,  0.000000,  1.000000,
    1.000000,  0.000000,  0.000000,  0.000000, -1.000000,  0.000000,
    0.000000,  0.000000,  1.000000, -0.951057,  0.309016,  0.000000,
   -0.809017,  0.587785,  0.000000, -0.809017,  0.587785,  0.000000,
    0.000000,  0.000000, -1.000000, -0.951057,  0.309016,  0.000000,
    0.000000,  0.000000, -1.000000, -0.587785,  0.809017,  0.000000,
   -0.587785,  0.809017,  0.000000,  0.000000,  0.000000, -1.000000,
   -0.309017,  0.951057,  0.000000, -0.309017,  0.951057,  0.000000,
    0.000000,  0.000000, -1.000000,  0.000000,  1.000000,  0.000000,
    0.000000,  1.000000,  0.000000,  0.000000,  0.000000, -1.000000,
    0.309017,  0.951056,  0.000000,  0.309017,  0.951056,  0.000000,
    0.000000,  0.000000, -1.000000,  0.587785,  0.809017,  0.000000,
    0.587785,  0.809017,  0.000000,  0.000000,  0.000000, -1.000000,
    0.809017,  0.587785,  0.000000,  0.809017,  0.587785,  0.000000,
    0.000000,  0.000000, -1.000000,  0.951057,  0.309017,  0.000000,
    0.951057,  0.309017,  0.000000,  0.000000,  0.000000, -1.000000,
    1.000000,  0.000000,  0.000000,  1.000000,  0.000000,  0.000000,
    0.000000,  0.000000, -1.000000,  0.951057, -0.309017,  0.000000,
    0.951057, -0.309017,  0.000000,  0.000000,  0.000000, -1.000000,
    0.809017, -0.587785,  0.000000,  0.809017, -0.587785,  0.000000,
    0.000000,  0.000000, -1.000000,  0.587785, -0.809017,  0.000000,
    0.587785, -0.809017,  0.000000,  0.000000,  0.000000, -1.000001,
    0.309017, -0.951057,  0.000000,  0.309017, -0.951057,  0.000000,
    0.000000,  0.000000, -1.000000,  0.000000, -1.000000,  0.000000,
    0.000000, -1.000000,  0.000000,  0.000000,  0.000000, -1.000000,
   -0.309017, -0.951056,  0.000000, -0.309017, -0.951056,  0.000000,
    0.000000,  0.000000, -1.000000, -0.587785, -0.809017,  0.000000,
   -0.587785, -0.809017,  0.000000,  0.000000,  0.000000, -1.000000,
   -0.809017, -0.587785,  0.000000, -0.809017, -0.587785,  0.000000,
    0.000000,  0.000000, -1.000000, -0.951057, -0.309017,  0.000000,
   -0.951057, -0.309017,  0.000000,  0.000000,  0.000000, -1.000000,
   -1.000000, -0.000001,  0.000000, -1.000000, -0.000001,  0.000000,
    0.000000,  0.000000, -1.000000,  0.000000,  0.000000, -1.000000,
    0.809017, -0.587785,  0.000000,  0.000000,  0.000000, -1.000000,
    0.951057, -0.309016,  0.000000,  0.000000,  0.000000, -1.000000,
    0.587785, -0.809017,  0.000000,  0.000000,  0.000000, -1.000000,
    0.309017, -0.951056,  0.000000,  0.000000,  0.000000, -1.000000,
    0.000000, -1.000000,  0.000000,  0.000000,  0.000000, -1.000000,
   -0.309017, -0.951056,  0.000000,  0.000000,  0.000000, -1.000000,
   -0.587785, -0.809017,  0.000000,  0.000000,  0.000000, -1.000000
   -0.809017, -0.587785,  0.000000,  0.000000,  0.000000, -1.000000,
   -0.951057, -0.309017,  0.000000,  0.000000,  0.000000, -1.000000,
   -1.000000,  0.000000,  0.000000,  0.000000,  0.000000, -1.000000,
   -0.951057,  0.309017,  0.000000,  0.000000,  0.000000, -1.000000,
   -0.809017,  0.587785,  0.000000,  0.000000,  0.000000, -1.000000,
   -0.587785,  0.809017,  0.000000,  0.000000,  0.000000, -1.000000,
   -0.309017,  0.951057,  0.000000,  0.000000,  0.000000, -1.000000,
    0.000000,  1.000000,  0.000000,  0.000000,  0.000000, -1.000000,
    0.309017,  0.951056,  0.000000,  0.000000,  0.000000, -1.000000,
    0.587785,  0.809017,  0.000000,  0.000000,  0.000000, -1.000000,
    0.809017,  0.587785,  0.000000,  0.000000,  0.000000, -1.000000,
    0.951057,  0.309017,  0.000000,  0.000000,  0.000000, -1.000000,
    1.000000,  0.000001,  0.000000,  0.951057, -0.309016,  0.000000,
    0.809017, -0.587785,  0.000000,  0.587785, -0.809017,  0.000000,
    0.309017, -0.951056,  0.000000,  0.000000, -1.000000,  0.000000,
   -0.309017, -0.951056,  0.000000, -0.587785, -0.809017,  0.000000,
   -0.809017, -0.587785,  0.000000, -0.951057, -0.309017,  0.000000,
   -1.000000,  0.000000,  0.000000, -0.951057,  0.309017,  0.000000,
   -0.809017,  0.587785,  0.000000, -0.587785,  0.809017,  0.000000,
   -0.309017,  0.951057,  0.000000,  0.000000,  1.000000,  0.000000,
    0.309017,  0.951056,  0.000000,  0.587785,  0.809017,  0.000000,
    0.809017,  0.587785,  0.000000,  0.951057,  0.309017,  0.000000,
    1.000000,  0.000001,  0.000000, -0.951057,  0.000000,  0.309017,
   -0.809017,  0.000000,  0.587786, -0.809017,  0.000000,  0.587786,
    0.000000,  1.000000, -0.000001, -0.951057,  0.000000,  0.309017,
    0.000000,  1.000000, -0.000002, -0.587785,  0.000000,  0.809017,
   -0.587785,  0.000000,  0.809017,  0.000000,  1.000000,  0.000000,
   -0.309016,  0.000000,  0.951057, -0.309016,  0.000000,  0.951057,
    0.000000,  1.000000,  0.000000,  0.000001,  0.000000,  1.000000,
    0.000001,  0.000000,  1.000000,  0.000000,  1.000000,  0.000000,
    0.309018,  0.000000,  0.951056,  0.309018,  0.000000,  0.951056,
    0.000000,  1.000000,  0.000000,  0.587785,  0.000000,  0.809017,
    0.587785,  0.000000,  0.809017,  0.000000,  1.000000,  0.000000,
    0.809017,  0.000000,  0.587786,  0.809017,  0.000000,  0.587786,
    0.000000,  1.000000,  0.000000,  0.951057,  0.000000,  0.309017,
    0.951057,  0.000000,  0.309017,  0.000000,  1.000000,  0.000001,
    1.000000,  0.000000,  0.000000,  1.000000,  0.000000,  0.000000,
    0.000000,  1.000000,  0.000002,  0.951057,  0.000000, -0.309017,
    0.951057,  0.000000, -0.309017,  0.000000,  1.000000,  0.000002,
    0.809017,  0.000000, -0.587786,  0.809017,  0.000000, -0.587786,
    0.000000,  1.000000,  0.000001,  0.587785,  0.000000, -0.809017,
    0.587785,  0.000000, -0.809017,  0.000000,  1.000000,  0.000000,
    0.309017,  0.000000, -0.951056,  0.309017,  0.000000, -0.951056,
    0.000000,  1.000000,  0.000000,  0.000000,  0.000000, -1.000000,
    0.000000,  0.000000, -1.000000,  0.000000,  1.000000,  0.000000,
   -0.309017,  0.000000, -0.951056, -0.309017,  0.000000, -0.951056,
    0.000000,  1.000000,  0.000000, -0.587786,  0.000000, -0.809017,
   -0.587786,  0.000000, -0.809017,  0.000000,  1.000000,  0.000000,
   -0.809017,  0.000000, -0.587785, -0.809017,  0.000000, -0.587785,
    0.000000,  1.000000,  0.000000, -0.951056,  0.000000, -0.309018,
   -0.951056,  0.000000, -0.309018,  0.000000,  1.000000, -0.000001,
   -1.000000,  0.000000, -0.000001, -1.000000,  0.000000, -0.000001,
    0.000000,  1.000000, -0.000002,  0.000000,  1.000000,  0.000000
  );

 CAMERA_INDICES : array[0..244] of Integer = (
      2,   5,  11,   5,   8,  10,   7,  17,   7,  14,  16,  13,  23,  13,  20,  22,
     19,   1,  19,   4,   3,  18,   6,  18,  12,  21,   0,  15,   0,   9, 203, 149,
    204, 147, 204, 152, 204, 155, 204, 158, 204, 161, 204, 164, 204, 167, 204, 170,
    204, 173, 204, 176, 204, 179, 204, 182, 204, 185, 204, 188, 204, 191, 204, 194,
    204, 197, 204, 200, 203, 144, 148, 144, 202, 201, 199, 198, 196, 195, 193, 192,
    190, 189, 187, 186, 184, 183, 181, 180, 178, 177, 175, 174, 172, 171, 169, 168,
    166, 165, 163, 162, 160, 159, 157, 156, 154, 153, 151, 150, 146, 145, 148, 145,
    144, 123,  87, 124,  87, 125,  85, 126,  89, 127,  91, 128,  93, 129,  95, 130,
     97, 131,  99, 132, 101, 133, 103, 134, 105, 135, 107, 136, 109, 137, 111, 138,
    113, 139, 115, 140, 117, 141, 119, 142, 121, 143, 123, 143, 124,  29,  86,  29,
    122,  83, 120,  80, 118,  77, 116,  74, 114,  71, 112,  68, 110,  65, 108,  62,
    106,  59, 104,  56, 102,  53, 100,  50,  98,  47,  96,  44,  94,  41,  92,  38,
     90,  35,  88,  32,  84,  27,  86,  27,  29,  24,  28,  24,  82,  81,  79,  78,
     76,  75,  73,  72,  70,  69,  67,  66,  64,  63,  61,  60,  58,  57,  55,  54,
     52,  51,  49,  48,  46,  45,  43,  42,  40,  39,  37,  36,  34,  33,  31,  30,
     26,  25,  28,  25,  24
  );

procedure DrawCamera;
const
  SHININESS = 32.0;
  AMBIENT_COLOR: array[0..3] of GLfloat = (0.3, 0.3, 0.3, 1.0);
  DIFFUSE_COLOR: array[0..3] of GLfloat = (0.8, 0.8, 0.8, 1.0);
  SPECULAR_COLOR: array[0..3] of GLfloat = (1.0, 1.0, 1.0, 1.0);
begin
  // Set specular and shiniess using glMaterial
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, shininess); // range 0 ~ 128
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @SPECULAR_COLOR);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, @DIFFUSE_COLOR);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, @AMBIENT_COLOR);

  // Set ambient and diffuse color using glColorMaterial (gold-yellow)
  glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
  glColor3fv(@DIFFUSE_COLOR);

  // Start to render polygons
  glEnableClientState(GL_NORMAL_ARRAY);
  glEnableClientState(GL_VERTEX_ARRAY);

  glNormalPointer(GL_FLOAT, 0, @CAMERA_NORMALS);
  glVertexPointer(3, GL_FLOAT, 0, @CAMERA_VERTICES);

  glDrawElements(GL_TRIANGLE_STRIP,  5, GL_UNSIGNED_INT, @CAMERA_INDICES[0]);
  glDrawElements(GL_TRIANGLE_STRIP,  5, GL_UNSIGNED_INT, @CAMERA_INDICES[5]);
  glDrawElements(GL_TRIANGLE_STRIP,  5, GL_UNSIGNED_INT, @CAMERA_INDICES[10]);
  glDrawElements(GL_TRIANGLE_STRIP,  5, GL_UNSIGNED_INT, @CAMERA_INDICES[15]);
  glDrawElements(GL_TRIANGLE_STRIP,  5, GL_UNSIGNED_INT, @CAMERA_INDICES[20]);
  glDrawElements(GL_TRIANGLE_STRIP,  5, GL_UNSIGNED_INT, @CAMERA_INDICES[25]);
  glDrawElements(GL_TRIANGLE_STRIP, 39, GL_UNSIGNED_INT, @CAMERA_INDICES[30]);
  glDrawElements(GL_TRIANGLE_STRIP, 44, GL_UNSIGNED_INT, @CAMERA_INDICES[69]);
  glDrawElements(GL_TRIANGLE_STRIP, 44, GL_UNSIGNED_INT, @CAMERA_INDICES[113]);
  glDrawElements(GL_TRIANGLE_STRIP, 44, GL_UNSIGNED_INT, @CAMERA_INDICES[157]);
  glDrawElements(GL_TRIANGLE_STRIP, 44, GL_UNSIGNED_INT, @CAMERA_INDICES[201]);

  glDisableClientState(GL_VERTEX_ARRAY);	// disable vertex arrays
  glDisableClientState(GL_NORMAL_ARRAY);	// disable normal arrays
end;

end.

