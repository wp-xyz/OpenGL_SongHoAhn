unit oglAbout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  LCLIntf, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Button1: TButton;
    Image1: TImage;
    lblURL: TLabel;
    lblCopyright: TLabel;
    lblTitle: TLabel;
    procedure lblURLClick(Sender: TObject);
    procedure lblURLMouseEnter(Sender: TObject);
    procedure lblURLMouseLeave(Sender: TObject);
  private
    FCopyright: String;
    function GetTitle: String;
    function GetURL: String;
    procedure SetCopyright(const AValue: String);
    procedure SetTitle(const AValue: String);
    procedure SetURL(const AValue: String);
  public
    property Title: String read GetTitle write SetTitle;
    property URL: String read GetURL write SetURL;
    property Copyright: String read FCopyright write SetCopyright;

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.lblURLMouseEnter(Sender: TObject);
begin
  lblURL.Cursor := crHandPoint;
  lblURL.Font.Style := [fsUnderline];
end;

procedure TAboutForm.lblURLClick(Sender: TObject);
begin
  OpenURL(lblURL.Caption);
end;

procedure TAboutForm.lblURLMouseLeave(Sender: TObject);
begin
  lblURL.Cursor := crDefault;
  lblURL.Font.Style := [];
end;

function TAboutForm.GetTitle: String;
begin
  Result := lblTitle.Caption;
end;

function TAboutForm.GetURL: String;
begin
  Result := lblURL.Caption;
end;

procedure TAboutForm.SetCopyright(const AValue: String);
begin
  lblCopyright.Caption := Format(
    'Copyright (c) %s Song Ho Ahn' + LineEnding +
    'Lazarus port by Werner Pamler', [FCopyright]);
end;

procedure TAboutForm.SetTitle(const AValue: String);
begin
  lblTitle.Caption := AValue;
end;

procedure TAboutForm.SetURL(const AValue: String);
begin
  lblURL.Caption := AValue;
end;

end.

