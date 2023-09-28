unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons,
  gl, glu, OpenGLContext,
  oglMath, oglCameraModel, oglController, oglTeapotModel, oglAbout;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnResetView: TButton;
    btnResetModel: TButton;
    gbCamera: TGroupBox;
    gbModel: TGroupBox;
    gbModelMatrixDisplay1: TGroupBox;
    gbViewMatrixDisplay: TGroupBox;
    gbModelMatrixDisplay: TGroupBox;
    ImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    lblMVM0: TLabel;
    lblMVM1: TLabel;
    lblMVM2: TLabel;
    lblMVM3: TLabel;
    lblMVM4: TLabel;
    lblMVM5: TLabel;
    lblMVM6: TLabel;
    lblMVM7: TLabel;
    lblMVM8: TLabel;
    lblMVM9: TLabel;
    lblMVM10: TLabel;
    lblMVM11: TLabel;
    lblMVM12: TLabel;
    lblMVM13: TLabel;
    lblMVM14: TLabel;
    lblMVM15: TLabel;
    lblVM0: TLabel;
    lblModelRotY: TLabel;
    lblModelRotYValue: TLabel;
    lblCameraPitch: TLabel;
    lblCameraHeading: TLabel;
    lblModelRotX: TLabel;
    lblModelRotXValue: TLabel;
    lblCameraRoll: TLabel;
    lblCameraPitchValue: TLabel;
    lblCameraHeadingValue: TLabel;
    lblModelRotZ: TLabel;
    lblCameraRollValue: TLabel;
    lblModelRotZValue: TLabel;
    lblModelX: TLabel;
    lblModelXValue: TLabel;
    lblCameraY: TLabel;
    lblModelY: TLabel;
    lblModelYValue: TLabel;
    lblCameraZ: TLabel;
    lblCameraXValue: TLabel;
    lblCameraX: TLabel;
    lblCameraYValue: TLabel;
    lblModelZ: TLabel;
    lblCameraZValue: TLabel;
    lblModelZValue: TLabel;
    lblVM1: TLabel;
    lblVM10: TLabel;
    lblVM11: TLabel;
    lblVM12: TLabel;
    lblVM13: TLabel;
    lblVM14: TLabel;
    lblVM15: TLabel;
    lblMM0: TLabel;
    lblMM1: TLabel;
    lblMM2: TLabel;
    lblMM3: TLabel;
    lblVM2: TLabel;
    lblMM4: TLabel;
    lblMM5: TLabel;
    lblMM6: TLabel;
    lblMM7: TLabel;
    lblMM8: TLabel;
    lblMM9: TLabel;
    lblMM10: TLabel;
    lblMM11: TLabel;
    lblMM12: TLabel;
    lblMM13: TLabel;
    lblVM3: TLabel;
    lblMM14: TLabel;
    lblMM15: TLabel;
    lblVM4: TLabel;
    lblVM5: TLabel;
    lblVM6: TLabel;
    lblVM7: TLabel;
    lblVM8: TLabel;
    lblVM9: TLabel;
    OpenGLControl: TOpenGLControl;
    Panel1: TPanel;
    btnAbout: TSpeedButton;
    tbCameraHeading: TTrackBar;
    tbModelRotY: TTrackBar;
    tbModelRotX: TTrackBar;
    tbCameraRoll: TTrackBar;
    tbModelRotZ: TTrackBar;
    tbCameraX: TTrackBar;
    tbCameraPitch: TTrackBar;
    tbModelX: TTrackBar;
    tbCameraY: TTrackBar;
    tbModelY: TTrackBar;
    tbCameraZ: TTrackBar;
    tbModelZ: TTrackBar;
    procedure btnAboutClick(Sender: TObject);
    procedure btnResetViewClick(Sender: TObject);
    procedure btnResetModelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenGLControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControlMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControlMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OpenGLControlPaint(Sender: TObject);
    procedure OpenGLControlResize(Sender: TObject);
    procedure tbModelChange(Sender: TObject);
    procedure tbViewChange(Sender: TObject);
  private
    FController: TModelViewController;
    FMouseLeftDown, FMouseRightDown: Boolean;
    FInitDone: Boolean;
    procedure DrawCameraHandler(Sender: TObject);
    procedure DrawSceneHandler(Sender: TObject);
    procedure UpdateMatrixDisplay;
    procedure UpdateModelController;
    procedure UpdateViewController;
  protected

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.btnAboutClick(Sender: TObject);
begin
  with TAboutForm.Create(nil) do
    try
      Title := 'OpenGL ModelView Matrix';
      URL := 'www.songho.ca/opengl/gl_camera.html';
      Copyright := '2008-2018';
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMainForm.btnResetModelClick(Sender: TObject);
begin
  FController.ResetModelMatrix;
  OpenGLControl.Invalidate;
  UpdateModelController;
  UpdateMatrixDisplay;
end;

procedure TMainForm.btnResetViewClick(Sender: TObject);
begin
  FController.ResetViewMatrix;
  OpenGLControl.Invalidate;
  UpdateViewController;
  UpdateMatrixDisplay;
end;

procedure TMainForm.DrawCameraHandler(Sender: TObject);
begin
  DrawCamera;
end;

procedure TMainForm.DrawSceneHandler(Sender: TObject);
begin
  DrawTeapot;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FController := TModelViewController.Create;
  FController.OnDrawCamera := @DrawCameraHandler;
  FController.OnDrawScene := @DrawSceneHandler;
  FController.ResetViewMatrix;

  UpdateViewController;
  UpdateModelController;
  UpdateMatrixDisplay;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FController.Free;
end;

procedure TMainForm.OpenGLControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FController.SetMousePosition(X, Y);
  FMouseLeftDown := true;
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
  if Button = mbLeft then
    FMouseLeftDown := false;
  if Button = mbRight then
    FMouseRightDown := false;
end;

procedure TMainForm.OpenGLControlMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  FController.ZoomCameraDelta(-WheelDelta/120);
  OpenGLControl.Invalidate;
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
    OpenGLControl.MakeCurrent;
    FController.InitGL;
    FInitDone := true;
  end;
  FController.SetWindowSize(OpenGLControl.Width, OpenGLControl.Height);
end;

procedure TMainForm.tbModelChange(Sender: TObject);
begin
  if Sender = tbModelX then
  begin
    lblModelXValue.Caption := IntToStr(tbModelX.Position);
    FController.ModelX := tbModelX.Position;
  end else
  if Sender = tbModelY then
  begin
    lblModelYValue.Caption := IntToStr(tbModelY.Position);
    FController.ModelY := tbModelY.Position;
  end else
  if Sender = tbModelZ then
  begin
    lblModelZValue.Caption := IntToStr(tbModelZ.Position);
    FController.ModelZ :=tbModelZ.Position;
  end else
  if Sender = tbModelRotX then
  begin
    lblModelRotXValue.Caption := IntToStr(tbModelRotX.Position);
    FController.ModelAngleX := tbModelRotX.Position;
  end else
  if Sender = tbModelRotY then
  begin
    lblModelRotYValue.Caption := IntToStr(tbModelRotY.Position);
    FController.ModelAngleY := tbModelRotY.Position;
  end else
  if Sender = tbModelRotZ then
  begin
    lblModelRotYValue.Caption := IntToStr(tbModelRotZ.Position);
    FController.ModelAngleZ := tbModelRotZ.Position;
  end else
    exit;
  UpdateMatrixDisplay;
  OpenGLControl.Invalidate;
end;


procedure TMainForm.tbViewChange(Sender: TObject);
begin
  if Sender = tbCameraX then
  begin
    lblCameraXValue.Caption := IntToStr(tbCameraX.Position);
    FController.CameraX := tbCameraX.Position;
  end else
  if Sender = tbCameraY then
  begin
    lblCameraYValue.Caption := IntToStr(tbCameraY.Position);
    FController.CameraY := tbCameraY.Position;
  end else
  if Sender = tbCameraZ then
  begin
    lblCameraZValue.Caption := IntToStr(tbCameraZ.Position);
    FController.CameraZ := tbCameraZ.Position;
  end else
  if Sender = tbCameraPitch then
  begin
    lblCameraPitchValue.Caption := IntToStr(tbCameraPitch.Position);
    FController.CameraAngleX := tbCameraPitch.Position;
  end else
  if Sender = tbCameraHeading then
  begin
    lblCameraHeadingValue.Caption := IntToStr(tbCameraHeading.Position);
    FController.CameraAngleY := tbCameraHeading.Position;
  end else
  if Sender = tbCameraRoll then
  begin
    lblCameraRollValue.Caption := IntToStr(tbCameraRoll.Position);
    FController.CameraAngleZ := tbCameraRoll.Position;
  end else
    exit;
  UpdateMatrixDisplay;
  OpenGLControl.Invalidate;
end;

procedure TMainForm.UpdateMatrixDisplay;
var
  m: ToglMatrix4f;
begin
  m := FController.MatrixView;
  lblVM0.Caption := FormatFloat('0.00', m[0]);
  lblVM1.Caption := FormatFloat('0.00', m[1]);
  lblVM2.Caption := FormatFloat('0.00', m[2]);
  lblVM3.Caption := FormatFloat('0.00', m[3]);
  lblVM4.Caption := FormatFloat('0.00', m[4]);
  lblVM5.Caption := FormatFloat('0.00', m[5]);
  lblVM6.Caption := FormatFloat('0.00', m[6]);
  lblVM7.Caption := FormatFloat('0.00', m[7]);
  lblVM8.Caption := FormatFloat('0.00', m[8]);
  lblVM9.Caption := FormatFloat('0.00', m[9]);
  lblVM10.Caption := FormatFloat('0.00', m[10]);
  lblVM11.Caption := FormatFloat('0.00', m[11]);
  lblVM12.Caption := FormatFloat('0.00', m[12]);
  lblVM13.Caption := FormatFloat('0.00', m[13]);
  lblVM14.Caption := FormatFloat('0.00', m[14]);
  lblVM15.Caption := FormatFloat('0.00', m[15]);

  m := FController.MatrixModel;
  lblMM0.Caption := FormatFloat('0.00', m[0]);
  lblMM1.Caption := FormatFloat('0.00', m[1]);
  lblMM2.Caption := FormatFloat('0.00', m[2]);
  lblMM3.Caption := FormatFloat('0.00', m[3]);
  lblMM4.Caption := FormatFloat('0.00', m[4]);
  lblMM5.Caption := FormatFloat('0.00', m[5]);
  lblMM6.Caption := FormatFloat('0.00', m[6]);
  lblMM7.Caption := FormatFloat('0.00', m[7]);
  lblMM8.Caption := FormatFloat('0.00', m[8]);
  lblMM9.Caption := FormatFloat('0.00', m[9]);
  lblMM10.Caption := FormatFloat('0.00', m[10]);
  lblMM11.Caption := FormatFloat('0.00', m[11]);
  lblMM12.Caption := FormatFloat('0.00', m[12]);
  lblMM13.Caption := FormatFloat('0.00', m[13]);
  lblMM14.Caption := FormatFloat('0.00', m[14]);
  lblMM15.Caption := FormatFloat('0.00', m[15]);

  m := FController.MatrixModelView;
  lblMVM0.Caption := FormatFloat('0.00', m[0]);
  lblMVM1.Caption := FormatFloat('0.00', m[1]);
  lblMVM2.Caption := FormatFloat('0.00', m[2]);
  lblMVM3.Caption := FormatFloat('0.00', m[3]);
  lblMVM4.Caption := FormatFloat('0.00', m[4]);
  lblMVM5.Caption := FormatFloat('0.00', m[5]);
  lblMVM6.Caption := FormatFloat('0.00', m[6]);
  lblMVM7.Caption := FormatFloat('0.00', m[7]);
  lblMVM8.Caption := FormatFloat('0.00', m[8]);
  lblMVM9.Caption := FormatFloat('0.00', m[9]);
  lblMVM10.Caption := FormatFloat('0.00', m[10]);
  lblMVM11.Caption := FormatFloat('0.00', m[11]);
  lblMVM12.Caption := FormatFloat('0.00', m[12]);
  lblMVM13.Caption := FormatFloat('0.00', m[13]);
  lblMVM14.Caption := FormatFloat('0.00', m[14]);
  lblMVM15.Caption := FormatFloat('0.00', m[15]);
end;

procedure TMainForm.UpdateModelController;
begin
  tbModelX.Position := round(FController.ModelX);
  tbModelY.Position := round(FController.ModelY);
  tbModelZ.Position := round(FController.ModelZ);
  tbModelRotX.Position := round(FController.ModelAngleX);
  tbModelRotY.Position := round(FController.ModelAngleY);
  tbModelRotZ.Position := round(FController.ModelAngleZ);
end;

procedure TMainForm.UpdateViewController;
begin
  tbCameraX.Position := round(FController.CameraX);
  tbCameraY.Position := round(FController.CameraY);
  tbcameraZ.Position := round(FController.CameraZ);
  tbCameraPitch.Position := round(FController.CameraAngleX);
  tbCameraHeading.Position := round(FController.CameraAngleY);
  tbCameraRoll.Position := round(FController.CameraAngleZ);
end;

end.

