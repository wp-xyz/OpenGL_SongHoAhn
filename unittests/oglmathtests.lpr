program oglmathtests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, matrix2tests, matrix3tests, matrix4tests;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

