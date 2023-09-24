unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Buttons,
  gl, glu, OpenGLContext,
  oglMath, oglOrbitController, oglAbout, oglCameraModel, oglTeapotModel;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    btnResetCamera: TButton;
    cbShowFOV: TCheckBox;
    cbShowGrid: TCheckBox;
    gbAngles: TGroupBox;
    gbCameraPosition: TGroupBox;
    gbTargetPosition: TGroupBox;
    gbCameraMatrixDisplay: TGroupBox;
    gbCameraQuaternionDisplay: TGroupBox;
    ImageList: TImageList;
    lblTargetPosX: TLabel;
    lblTargetPosXValue: TLabel;
    lblTargetPosY: TLabel;
    lblTargetPosYValue: TLabel;
    lblTargetPosZ: TLabel;
    lblTargetPosZValue: TLabel;
    lblPitch: TLabel;
    lblCameraPosX: TLabel;
    lblCameraPosXValue: TLabel;
    lblCameraPosZ: TLabel;
    lblCameraPosZValue: TLabel;
    lblCM0: TLabel;
    lblCM1: TLabel;
    lblCM10: TLabel;
    lblCM11: TLabel;
    lblCM12: TLabel;
    lblCM13: TLabel;
    lblCM14: TLabel;
    lblCM15: TLabel;
    lblCQs: TLabel;
    lblCQx: TLabel;
    lblCQy: TLabel;
    lblCQz: TLabel;
    lblCM2: TLabel;
    lblCM3: TLabel;
    lblCM4: TLabel;
    lblCM5: TLabel;
    lblCM6: TLabel;
    lblCM7: TLabel;
    lblCM8: TLabel;
    lblCM9: TLabel;
    lblYaw: TLabel;
    lblPitchValue: TLabel;
    lblRoll: TLabel;
    lblCameraPosY: TLabel;
    lblYawValue: TLabel;
    lblRollValue: TLabel;
    lblCameraPosYValue: TLabel;
    OpenGLControl1: TOpenGLControl;
    OpenGLControl2: TOpenGLControl;
    ParamPanel: TPanel;
    btnAbout: TSpeedButton;
    tbTargetPosX: TTrackBar;
    tbTargetPosY: TTrackBar;
    tbTargetPosZ: TTrackBar;
    tbPitch: TTrackBar;
    tbCameraPosX: TTrackBar;
    tbCameraPosZ: TTrackBar;
    tbYaw: TTrackBar;
    tbRoll: TTrackBar;
    tbCameraPosY: TTrackBar;
    procedure btnAboutClick(Sender: TObject);
    procedure btnResetCameraClick(Sender: TObject);
    procedure cbShowFOVChange(Sender: TObject);
    procedure cbShowGridChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure OpenGLControl1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControl1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OpenGLControl1Paint(Sender: TObject);
    procedure OpenGLControl1Resize(Sender: TObject);
    procedure OpenGLControl2Paint(Sender: TObject);
    procedure OpenGLControl2Resize(Sender: TObject);
    procedure UpdateController(Sender: TObject);
  private
    FController: ToglOrbitController;
    FInitDone1, FInitDone2: Boolean;
    procedure DrawCameraHandler(Sender: TObject);
    procedure DrawSceneHandler(Sender: TObject);
    procedure UpdateCameraAngleControls;
    procedure UpdateCameraPositionControls;
    procedure UpdateCameraTargetControls;
    procedure UpdateMatrixDisplay;

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
      Title := 'OpenGL Orbit Camera';
      URL := 'www.songho.ca/opengl/gl_camera.html';
      Copyright := '2016-2021';
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMainForm.btnResetCameraClick(Sender: TObject);
begin
  FController.ResetCamera;

  UpdateCameraAngleControls;
  UpdateCameraPositionControls;
  UpdateMatrixDisplay;

  OpenGLControl1.Invalidate;
  OpenGLControl2.Invalidate;
end;

procedure TMainForm.DrawCameraHandler(Sender: TObject);
begin
  glPushMatrix;
  glRotatef(180.0, 1,0,0);  // Make camera face towards object.
  DrawCamera;
  glPopMatrix;
end;

procedure TMainForm.DrawSceneHandler(Sender: TObject);
begin
  DrawTeaPot;
end;

procedure TMainForm.cbShowFOVChange(Sender: TObject);
begin
  FController.FoVEnabled := cbShowFOV.Checked;
  OpenGLControl1.Invalidate;
end;

procedure TMainForm.cbShowGridChange(Sender: TObject);
begin
  FController.GridEnabled := cbShowGrid.Checked;
  OpenGLControl1.Invalidate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin

  FController := ToglOrbitController.Create;
  FController.OnDrawCamera := @DrawCameraHandler;
  FController.OnDrawScene := @DrawSceneHandler;

  UpdateCameraAngleControls;
  UpdateCameraPositionControls;
  UpdateCameraTargetControls;
  UpdateMatrixDisplay;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FController.Free;
end;

procedure TMainForm.OpenGLControl1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FController.SetMousePosition(X, Y);
  if Button = mbLeft then
    FController.SetMouseLeft(true)
  else if Button = mbRight then
    FController.SetMouseRight(true);
end;

procedure TMainForm.OpenGLControl1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Shift = [ssLeft]) then
  begin
    FController.RotateCamera(x, y);
    OpenGLControl1.Invalidate;
    OpenGLControl2.Invalidate;
  end else
  if (Shift = [ssRight]) then
  begin
    FController.ZoomCamera(Y);
    OpenGLControl1.Invalidate;
    OpenGLControl2.Invalidate;
  end;
end;

procedure TMainForm.OpenGLControl1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FController.SetMousePosition(X, Y);
  if Button = mbLeft then
    FController.SetMouseLeft(false)
  else if Button = mbRight then
    FController.SetMouseRight(false);
end;

procedure TMainForm.OpenGLControl1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  FController.ZoomCameraDelta(WheelDelta/120);
  OpenGLControl1.Invalidate;
  OpenGLControl2.Invalidate;
end;

procedure TMainForm.OpenGLControl1Paint(Sender: TObject);
begin
  OpenGLControl1.MakeCurrent;
  FController.Draw(1);
  OpenGLControl1.SwapBuffers;
end;

procedure TMainForm.OpenGLControl1Resize(Sender: TObject);
begin
  if not FInitDone1 then
  begin
    OpenGLControl1.MakeCurrent;
    FController.InitGL;
    FInitDone1 := true;
  end;
  FController.SetWindowSize(OpenGLControl1.Width, OpenGLControl1.Height);
  OpenGLControl1.Invalidate;
end;

procedure TMainForm.OpenGLControl2Paint(Sender: TObject);
begin
  OpenGLControl2.MakeCurrent;
  FController.Draw(2);
  OpenGLControl2.SwapBuffers;
end;

procedure TMainForm.OpenGLControl2Resize(Sender: TObject);
begin
  if not FInitDone2 then
  begin
    OpenGLControl2.MakeCurrent;
    FController.InitGL;
    FInitDone2 := true;
  end;
  FController.SetWindowSize(OpenGLControl2.Width, OpenGLControl2.Height);
  OpenGLControl2.Invalidate;
end;

procedure TMainForm.UpdateCameraAngleControls;
begin
  tbPitch.Position := round(FController.CameraAngleX);
  tbYaw.Position := round(FController.CameraAngleY);
  tbRoll.Position := round(FController.CameraAngleZ);
end;

procedure TMainForm.UpdateCameraPositionControls;
begin
  tbCameraPosX.Position := round(FController.CameraPositionX);
  tbCameraPosY.Position := round(FController.CameraPositionY);
  tbCameraPosZ.Position := round(FController.CameraPositionZ);
end;

procedure TMainForm.UpdateCameraTargetControls;
begin
  tbTargetPosX.Position := round(FController.CameraTargetX);
  tbTargetPosY.Position := round(FController.CameraTargetY);
  tbTargetPosZ.Position := round(FController.CameraTargetZ);
end;

procedure TMainForm.UpdateController(Sender: TObject);
begin
  if Sender = tbPitch then
  begin
    FController.CameraAngleX := tbPitch.Position;
    lblPitchValue.Caption := IntToStr(tbPitch.Position);
  end else
  if Sender = tbYaw then
  begin
    FController.CameraAngleY := tbYaw.Position;
    lblYawValue.Caption := IntToStr(tbYaw.Position);
  end else
  if Sender = tbRoll then
  begin
    FController.CameraAngleZ := tbRoll.Position;
    lblRollValue.Caption := IntToStr(tbRoll.Position);
  end else
  if Sender = tbCameraPosX then
  begin
    FController.CameraPositionX := tbCameraPosX.Position;
    lblCameraPosXValue.Caption := IntToStr(tbCameraPosX.Position);
  end else
  if Sender = tbCameraPosY then
  begin
    FController.CameraPositionY := tbCameraPosY.Position;
    lblCameraPosYValue.Caption := IntToStr(tbCameraPosY.Position);
  end else
  if Sender = tbCameraPosZ then
  begin
    FController.CameraPositionZ := tbCameraPosZ.Position;
    lblCameraPosZValue.Caption := IntToStr(tbCameraPosZ.Position);
  end else
  if Sender = tbTargetPosX then
  begin
    FController.CameraTargetX := tbTargetPosX.Position;
    lblTargetPosXValue.Caption := IntToStr(tbTargetPosX.Position);
  end else
  if Sender = tbTargetPosY then
  begin
    FController.CameraTargetY := tbTargetPosY.Position;
    lblTargetPosYValue.Caption := IntToStr(tbTargetPosY.Position);
  end else
  if Sender = tbTargetPosZ then
  begin
    FController.CameraTargetZ := tbTargetPosZ.Position;
    lblTargetPosZValue.Caption := IntToStr(tbTargetPosZ.Position);
  end else
    exit;
  UpdateMatrixDisplay;
  OpenGLControl1.Invalidate;
  OpenGLControl2.Invalidate;
end;

procedure TMainForm.UpdateMatrixDisplay;
begin
  lblCM0.Caption := FormatFloat('0.00', FController.CameraMatrix[0]);
  lblCM1.Caption := FormatFloat('0.00', FController.CameraMatrix[1]);
  lblCM2.Caption := FormatFloat('0.00', FController.CameraMatrix[2]);
  lblCM3.Caption := FormatFloat('0.00', FController.CameraMatrix[3]);
  lblCM4.Caption := FormatFloat('0.00', FController.CameraMatrix[4]);
  lblCM5.Caption := FormatFloat('0.00', FController.CameraMatrix[5]);
  lblCM6.Caption := FormatFloat('0.00', FController.CameraMatrix[6]);
  lblCM7.Caption := FormatFloat('0.00', FController.CameraMatrix[7]);
  lblCM8.Caption := FormatFloat('0.00', FController.CameraMatrix[8]);
  lblCM9.Caption := FormatFloat('0.00', FController.CameraMatrix[9]);
  lblCM10.Caption := FormatFloat('0.00', FController.CameraMatrix[10]);
  lblCM11.Caption := FormatFloat('0.00', FController.CameraMatrix[11]);
  lblCM12.Caption := FormatFloat('0.00', FController.CameraMatrix[12]);
  lblCM13.Caption := FormatFloat('0.00', FController.CameraMatrix[13]);
  lblCM14.Caption := FormatFloat('0.00', FController.CameraMatrix[14]);
  lblCM15.Caption := FormatFloat('0.00', FController.CameraMatrix[15]);

  lblCQs.Caption := FormatFloat('0.00', FController.CameraQuaternion.S);
  lblCQx.Caption := FormatFloat('0.00', FController.CameraQuaternion.X);
  lblCQy.Caption := FormatFloat('0.00', FController.CameraQuaternion.Y);
  lblCQz.Caption := FormatFloat('0.00', FController.CameraQuaternion.Z);
end;

end.

