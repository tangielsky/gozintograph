unit colfind;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Clipbrd;

type

  { TColorFinderForm }

  TColorFinderForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ColorDialog1: TColorDialog;
    Edit1: TEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
  private

  public

  end;

var
  ColorFinderForm: TColorFinderForm;

implementation

{$R *.lfm}

{ TColorFinderForm }

procedure TColorFinderForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TColorFinderForm.Button2Click(Sender: TObject);
begin
  Clipboard.AsText:=Edit1.Text;
  Close;
end;

procedure TColorFinderForm.Edit1Click(Sender: TObject);
begin
  if ColorDialog1.Execute then Edit1.Text:=IntToStr(ColorDialog1.Color);
end;

end.

