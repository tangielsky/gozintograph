unit gozinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, gozgraph;

type

  { TGozInfoForm }

  TGozInfoForm = class(TForm)
    Button1: TButton;
    ListView1: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
  private

  public
    function ShowModal(GozGraph : TGozIntoGraph): Integer;

  end;

var
  GozInfoForm: TGozInfoForm;

implementation

{$R *.lfm}

{ TGozInfoForm }

procedure TGozInfoForm.Button1Click(Sender: TObject);
begin
  Close;
end;

function TGozInfoForm.ShowModal(GozGraph: TGozIntoGraph): Integer;
var
  i,j,k,p : longint;

  procedure Add(caption, value : string; index : integer);
  var
    ListItem : TListItem;
  begin
    ListItem:=ListView1.Items.Add;
    ListItem.Caption:=caption;
    ListItem.ImageIndex:=index;
    ListItem.SubItems.Add(value);
  end;

begin
  ListView1.BeginUpdate;
  ListView1.Items.Clear;
  if GozGraph<>nil then
    begin
      Add('Dispo.-Stufe',IntToStr(GozGraph.MaxLevel),4);

      i:=GozGraph.Items.Count;
      Add('Anzahl Elemente',IntToStr(i)+' Stk',5);

      j:=GozGraph.CountItemType(gitProduct,false);
      if i=0 then p:=0
      else p:=Round(j/i*100);
      Add('Anzahl Produkte',IntToStr(j)+' Stk / '+IntToStr(p)+'%',5);

      j:=GozGraph.CountItemType(gitModule,false);
      if i=0 then p:=0
      else p:=Round(j/i*100);
      Add('Anzahl Baugruppen',IntToStr(j)+' Stk / '+IntToStr(p)+'%',5);

      k:=GozGraph.CountItemType(gitModule,true);
      if j=0 then p:=0
      else p:=Round(k/j*100);
      Add('  davon mit Mehrfachverwendung',IntToStr(k)+' Stk / '+IntToStr(p)+'%',5);

      j:=GozGraph.CountItemType(gitItem,false);
      if i=0 then p:=0
      else p:=Round(j/i*100);
      Add('Anzahl Einzelteile',IntToStr(j)+' Stk / '+IntToStr(p)+'%',5);

      k:=GozGraph.CountItemType(gitItem,true);
      if j=0 then p:=0
      else p:=Round(k/j*100);
      Add('  davon mit Mehrfachverwendung',IntToStr(k)+' Stk / '+IntToStr(p)+'%',5);
    end;
  ListView1.EndUpdate;

  Result:=inherited ShowModal;
end;

end.

