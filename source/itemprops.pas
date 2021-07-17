unit itemprops;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls,
  Grids, ExtCtrls, Spin, ComCtrls, ComboEx, Buttons, Inifiles, Math,
  main, gozgraph;

type

  { TItemPropertiesForm }

  TItemPropertiesForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ComboBoxExSize: TComboBoxEx;
    ComboBoxExDescription: TComboBoxEx;
    ComboBoxExColor2: TComboBoxEx;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    SpeedButtonDescription: TSpeedButton;
    SpeedButtonColor2: TSpeedButton;
    SpeedButtonSize: TSpeedButton;
    SpinEditSizeMax: TSpinEdit;
    SpinEditSizeMin: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SpeedButtonColor2Click(Sender: TObject);
    procedure SpeedButtonDescriptionClick(Sender: TObject);
    procedure SpeedButtonSizeClick(Sender: TObject);
  private
    FGozGraph : TGozIntoGraph;
  public
    function ShowModal(GozGraph: TGozIntoGraph): Integer;

  published
    property GozGraph : TGozIntoGraph read FGozGraph write FGozGraph;

  end;

var
  ItemPropertiesForm: TItemPropertiesForm;

implementation

{$R *.lfm}

uses gozfunc;


{ TItemPropertiesForm }

procedure TItemPropertiesForm.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TItemPropertiesForm.SpeedButtonColor2Click(Sender: TObject);
begin
  ComboBoxExColor2.ItemIndex:=1;
end;

procedure TItemPropertiesForm.SpeedButtonDescriptionClick(Sender: TObject);
begin
  ComboBoxExDescription.ItemIndex:=1;
end;

procedure TItemPropertiesForm.SpeedButtonSizeClick(Sender: TObject);
begin
  ComboBoxExSize.ItemIndex:=1;
  SpinEditSizeMin.Value:=FGozGraph.DefaultItemHeight;
  SpinEditSizeMax.Value:=FGozGraph.DefaultItemHeight;
end;

function TItemPropertiesForm.ShowModal(GozGraph: TGozIntoGraph): Integer;
var
  i : integer;

begin
  FGozGraph:=GozGraph;

  //Description
  ComboBoxExDescription.ItemsEx.Clear;
  ComboBoxExDescription.ItemsEx.AddItem('bestehende beibehalten');
  ComboBoxExDescription.ItemsEx.AddItem('Name',4,4,4,4);
  ComboBoxExDescription.ItemsEx.AddItem('Dispositionsstufe',4,4,4,4);
  ComboBoxExDescription.ItemsEx.AddItem('Typ',4,4,4,4);
  FGozGraph.Properties.UpdateComboBoxEx(ComboBoxExDescription);
  ComboBoxExDescription.ItemIndex:=0;


  //Second color
  ComboBoxExColor2.ItemsEx.Clear;
  ComboBoxExColor2.ItemsEx.AddItem('bestehende beibehalten');
  ComboBoxExColor2.ItemsEx.AddItem('ohne',3,3,3,3);

  FGozGraph.Properties.UpdateComboBoxEx(ComboBoxExColor2);
  ComboBoxExColor2.ItemIndex:=0;


  //Size
  if SpinEditSizeMin.Value=0 then SpinEditSizeMin.Value:=FGozGraph.DefaultItemWidth;
  if SpinEditSizeMax.Value=0 then SpinEditSizeMax.Value:=FGozGraph.DefaultItemWidth;

  ComboBoxExSize.ItemsEx.Clear;
  ComboBoxExSize.ItemsEx.AddItem('bestehende beibehalten');
  ComboBoxExSize.ItemsEx.AddItem('ohne',3,3,3,3);
  ComboBoxExSize.ItemsEx.AddItem('Verbindungselemente',4,4,4,4);
  ComboBoxExSize.ItemsEx.AddItem('Verbindungsmengen',4,4,4,4);

  FGozGraph.Properties.UpdateComboBoxEx(ComboBoxExSize);
  ComboBoxExSize.ItemIndex:=0;


  Result:=inherited ShowModal;
end;

procedure TItemPropertiesForm.Button1Click(Sender: TObject);
var
  i : integer;
  GozGraphItem : TGozGraphItem;
  sizemin,sizemax,v : extended;

  function Scale(value : float) : integer;
  begin
    if sizemax-sizemin=0 then result:=0
    else result:=Round((value-sizemin)/(sizemax-sizemin)*(SpinEditSizeMax.Value-SpinEditSizeMin.Value)+SpinEditSizeMin.Value);
  end;

  function SizeValue(GozGraphItem : TGozGraphItem) : extended;
  begin
    result:=0;
    if ComboBoxExSize.ItemIndex=2 then
      result:=FGozGraph.GetIngoingInformation(GozGraphItem.Caption).Items+
        FGozGraph.GetOutgoingInformation(GozGraphItem.Caption).Items
    else if ComboBoxExSize.ItemIndex=3 then
      result:=FGozGraph.GetIngoingInformation(GozGraphItem.Caption).Quantity+
        FGozGraph.GetOutgoingInformation(GozGraphItem.Caption).Quantity
    else if ComboBoxExSize.ItemIndex>3 then
      result:=GozGraphItem.Properties.GetValueAsFloat(
        TGozFieldProperty(ComboBoxExSize.ItemsEx[ComboBoxExSize.ItemIndex].Data).Id);

  end;

begin
  //find min/max
  sizemin:=MaxDouble;
  sizemax:=0;
  if ComboBoxExSize.ItemIndex>1 then
      for i:=0 to FGozGraph.Items.Count-1 do
        begin
          GozGraphItem:=TGozGraphItem(FGozGraph.Items[i]);
          v:=SizeValue(GozGraphItem);
          if v>sizemax then sizemax:=v;
          if v<sizemin then sizemin:=v;
        end;

  //format GozGraph
  for i:=0 to FGozGraph.Items.Count-1 do
    begin
      GozGraphItem:=TGozGraphItem(FGozGraph.Items[i]);

      //Description
      if ComboBoxExDescription.ItemIndex=1 then
        GozGraphItem.Description:=GozGraphItem.Caption
      else if ComboBoxExDescription.ItemIndex=2 then
        GozGraphItem.Description:=IntToStr(GozGraphItem.Level)
      else if ComboBoxExDescription.ItemIndex=3 then
        GozGraphItem.Description:=GozGraphItem.ItemTypeCaption
      else if ComboBoxExDescription.ItemIndex>3 then
        GozGraphItem.Description:=GozGraphItem.Properties.GetValue(
          TGozFieldProperty(ComboBoxExDescription.ItemsEx[ComboBoxExDescription.ItemIndex].Data).Id);

      //Second Color
      if ComboBoxExColor2.ItemIndex=1 then
        GozGraphItem.Color2:=clNone
      else if ComboBoxExColor2.ItemIndex>1 then
        GozGraphItem.Color2:=GozGraphItem.Properties.GetValueAsColor(
          TGozFieldProperty(ComboBoxExColor2.ItemsEx[ComboBoxExColor2.ItemIndex].Data).Id);

      //Size
      if ComboBoxExSize.ItemIndex=1 then
          GozGraphItem.SetSize(FGozGraph.DefaultItemWidth,FGozGraph.DefaultItemHeight)
      else if ComboBoxExSize.ItemIndex>1 then
          begin
            v:=SizeValue(GozGraphItem);
            GozGraphItem.SetSize(Scale(v),Scale(v));
        end;
    end;

  FGozGraph.Repaint;
  Close;
end;

end.

