unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Buttons,
  gl, glu, OpenGLContext,
  oglTypes, oglMath, oglAbout, oglProjectionController, Types;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    gbProjectionParams: TGroupBox;
    gbProjectionMatrix: TGroupBox;
    Image1: TImage;
    ImageList: TImageList;
    lblNear: TLabel;
    lblFar: TLabel;
    lblNearValue: TLabel;
    lblFarValue: TLabel;
    lblTop: TLabel;
    lblTopValue: TLabel;
    lblLeft: TLabel;
    lblLeftValue: TLabel;
    lblBottom: TLabel;
    lblBottomValue: TLabel;
    lblRight: TLabel;
    lblRightValue: TLabel;
    OpenGLControl: TOpenGLControl;
    ParamsPanel: TPanel;
    rgRenderingMode: TRadioGroup;
    rgProjectionType: TRadioGroup;
    SpeedButton1: TSpeedButton;
    tbNear: TTrackBar;
    tbFar: TTrackBar;
    tbTop: TTrackBar;
    tbLeft: TTrackBar;
    tbBottom: TTrackBar;
    tbRight: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenGLControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure OpenGLControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControlMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OpenGLControlPaint(Sender: TObject);
    procedure OpenGLControlResize(Sender: TObject);
    procedure rgProjectionTypeClick(Sender: TObject);
    procedure rgRenderingModeClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure UpdateController(Sender: TObject);
  private
    FController: TProjectionController;
    FInitDone: Boolean;
    FMatrixImages: array[0..1] of TBitmap;
    FMouseLeftDown: Boolean;
    FMouseRightDown: Boolean;
    FProjMatrixElements: array[0..15] of TLabel;
    procedure LoadResImage(AResName: String; var ABitmap: TBitmap);
    procedure UpdateMatrixDisplay;
    procedure UpdateProjectionControls;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

const
  TRACKBAR_FACTOR = 2;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  LoadResImage('IMAGE_PERSPECTIVE', FMatrixImages[0]);
  LoadResImage('IMAGE_ORTHO', FMatrixImages[1]);
  Image1.Picture.Assign(FMatrixImages[0]);

  for i := 0 to 15 do
  begin
    FProjMatrixElements[i] := TLabel.Create(self);
    FProjMatrixElements[i].Alignment := taCenter;
    FProjMatrixElements[i].Layout := tlCenter;
    FProjMatrixElements[i].Parent := gbProjectionMatrix;
  end;

  FController := TProjectionController.Create;
  FController.SetProjection(-0.5, 0.5, -0.5, 0.5, 1.0, 10.0);

  UpdateProjectionControls;
  UpdateMatrixDisplay;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  FController.SetProjection(-0.5, 0.5, -0.5, 0.5, 1.0, 10.0);
  UpdateProjectionControls;
  UpdateMatrixDisplay;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FMatrixImages[0].Free;
  FMatrixImages[1].Free;
  FController.Free;
end;

procedure TMainForm.OpenGLControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FController.SetMousePosition(X, Y);
  case Button of
    mbLeft: FMouseLeftDown := true;
    mbRight: FMouseRightDown := true;
  end;
end;

procedure TMainForm.OpenGLControlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if FMouseLeftDown then
    FController.RotateCamera(X, Y)
  else
  if FMouseRightDown then
    FController.ZoomCamera(Y)
  else
    exit;

  FController.SetMousePosition(X, Y);
  OpenGLControl.Invalidate;
end;

procedure TMainForm.OpenGLControlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FController.SetMousePosition(X, Y);
  case Button of
    mbLeft: FMouseLeftDown := false;
    mbRight: FMouseRightDown := false;
  end;
end;

procedure TMainForm.OpenGLControlMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  FController.ZoomCameraDelta(WheelDelta / 120.0);
  OpenGLControl.Invalidate;
  Handled := true;
end;

procedure TMainForm.LoadResImage(AResName: String; var ABitmap: TBitmap);
var
  stream: TResourceStream;
begin
  ABitmap := TBitmap.Create;
  stream := TResourceStream.Create(HINSTANCE, AResName, RT_BITMAP);
  try
    ABitmap.LoadFromStream(stream, stream.Size);
  finally
    stream.Free;
  end;
end;

procedure TMainForm.OpenGLControlPaint(Sender: TObject);
begin
  FController.Draw;
  OpenGLControl.SwapBuffers;
end;

procedure TMainForm.OpenGLControlResize(Sender: TObject);
begin
  if not FInitDone then
  begin
    FInitDone := true;
    OpenGLControl.MakeCurrent;
    FController.InitGL;
  end;
  FController.SetWindowSize(OpenGLControl.Width, OpenGLControl.Height);
end;

procedure TMainForm.rgProjectionTypeClick(Sender: TObject);
begin
  Image1.Picture.Assign(FMatrixImages[rgProjectionType.ItemIndex]);
  FController.ProjectionMode := rgProjectionType.ItemIndex;
  OpenGLControl.Invalidate;
end;

procedure TMainForm.rgRenderingModeClick(Sender: TObject);
begin
  FController.DrawMode := rgRenderingMode.ItemIndex;
  OpenGLControl.Invalidate;
end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  with TAboutForm.Create(nil) do
  try
    Title :='OpenGL Projection';
    URL := 'www.songho.ca/opengl/gl_transform.html';
    Copyright := '2008-2017';
    ShowModal;
  finally
    Free;
  end;
end;

procedure TMainForm.UpdateController(Sender: TObject);
begin
  if Sender = tbLeft then
  begin
    FController.ProjectionLeft := tbLeft.Position / TRACKBAR_FACTOR;
    lblLeftValue.Caption := FormatFloat('0.0', FController.ProjectionLeft);
  end else
  if Sender = tbRight then
  begin
    FController.ProjectionRight := tbRight.Position / TRACKBAR_FACTOR;
    lblRightValue.Caption := FormatFloat('0.0', FController.ProjectionRight);
  end else
  if Sender = tbTop then
  begin
    FController.ProjectionTop := tbTop.Position / TRACKBAR_FACTOR;
    lblTopValue.Caption := FormatFloat('0.0', FController.ProjectionTop);
  end else
  if Sender = tbBottom then
  begin
    FController.ProjectionBottom := tbBottom.Position / TRACKBAR_FACTOR;
    lblBottomValue.Caption := FormatFloat('0.0', FController.ProjectionBottom);
  end else
  if Sender = tbFar then
  begin
    FController.ProjectionFar := tbFar.Position / TRACKBAR_FACTOR;
    lblFarValue.Caption := FormatFloat('0.0', FController.ProjectionFar);
  end else
  if Sender = tbNear then
  begin
    FController.ProjectionNear := tbNear.Position / TRACKBAR_FACTOR;
    lblNearValue.Caption := FormatFloat('0.0', FController.ProjectionNear);
  end else
    exit;
  UpdateMatrixDisplay;
  OpenGLControl.Invalidate;
end;

procedure TMainForm.UpdateMatrixDisplay;
var
  i: Integer;
begin
  for i := 0 to 15 do
    FProjMatrixElements[i].Caption := FormatFloat('0.00', FController.MatrixProjection[i]);
end;

procedure TMainForm.UpdateProjectionControls;
begin
  tbLeft.Position := round(FController.ProjectionLeft * TRACKBAR_FACTOR);
  tbRight.Position := round(FController.ProjectionRight * TRACKBAR_FACTOR);
  tbBottom.Position := round(FController.ProjectionBottom * TRACKBAR_FACTOR);
  tbTop.Position := round(FController.ProjectionTop * TRACKBAR_FACTOR);
  tbNear.Position := round(FController.ProjectionNear * TRACKBAR_FACTOR);
  tbFar.Position := round(FController.ProjectionFar * TRACKBAR_FACTOR);
end;


end.

