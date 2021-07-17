unit modprop;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  gozgraph;

type

  { TModifyPropertiesForm }

  TModifyPropertiesForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    EditField: TEdit;
    EditValue: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
  public
    FValueProperty: TGozValueProperty;
    function ShowModal: Integer; override;

  published
    property ValueProperty : TGozValueProperty read FValueProperty write FValueProperty;
  end;

var
  ModifyPropertiesForm: TModifyPropertiesForm;

implementation

{$R *.lfm}

{ TModifyPropertiesForm }

function TModifyPropertiesForm.ShowModal: Integer;
begin
  if FValueProperty=nil then exit;

  EditField.Text:=FValueProperty.Field.Name;
  EditValue.Text:=FValueProperty.Value;

  Result:=inherited ShowModal;
end;


procedure TModifyPropertiesForm.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TModifyPropertiesForm.Button1Click(Sender: TObject);
begin
  FValueProperty.Field.Name:=EditField.Text;
  FValueProperty.Value:=EditValue.Text;
  Close;
end;

end.

