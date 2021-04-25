unit gozfileinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn, Grids, ComCtrls;

type

  { TFileInformationForm }

  TFileInformationForm = class(TForm)
    Button1: TButton;
    ListView1: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FileInformationForm: TFileInformationForm;

implementation

{$R *.lfm}

uses main, gozgraph;


procedure TFileInformationForm.Button1Click(Sender: TObject);
begin

  Close;
end;

procedure TFileInformationForm.FormCreate(Sender: TObject);
  procedure Add(caption, value : string);
  var
    ListItem : TListItem;
  begin
    ListItem:=ListView1.Items.Add;
    ListItem.Caption:=caption;
    ListItem.SubItems.Add(value);
  end;

begin
  ListView1.BeginUpdate;
  ListView1.Items.Clear;
 { if item<>nil then
    begin
      Add('Dateiname','');
      Add('Anzahl Elemente','0');
      Add('Anzahl Produkte','0 / 0%');
      Add('Anzahl Baugruppen','0 / 0%');
      Add('Anzahl Einzelteile','0 / 0%');
      Add('Anzahl Baugruppen mit Mehrfachverwendung','0 / 0%');
      Add('Anzahl Einzelteile mit Mehrfachverwendung','0 / 0%');


    end;  }
  ListView1.EndUpdate;
end;

end.

