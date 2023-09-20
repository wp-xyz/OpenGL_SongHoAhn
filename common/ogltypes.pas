unit oglTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gl;

type
  ToglArray2f = array[0..1] of GLfloat;
  PoglArray2f = ^ToglArray2f;

  ToglArray3f = array[0..2] of GLfloat;
  PoglArray3f = ^ToglArray3f;

  ToglArray4f = array[0..3] of GLfloat;
  PoglArray4f = ^ToglArray4f;

  ToglArray9f = array[0..8] of GLfloat;
  PoglArray9f = ^ToglArray9f;

  ToglArray16f = array[0..15] of GLfloat;
  PoglArray16f = ^ToglArray16f;

implementation

end.

