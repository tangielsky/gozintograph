unit importprops;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls,
  Grids, ExtCtrls, Spin, ComCtrls, Inifiles,
  main, gozgraph;

const
  NEW_ITEM = 'Element hinzufügen...';

type

  { TImportItemPropertiesForm }

  TImportItemPropertiesForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ButtonImportProperty: TButton;
    ButtonDeleteProperty: TButton;
    CheckBoxHasHeaders: TCheckBox;
    ComboBoxDelimiter: TComboBox;
    ComboBoxItem: TComboBox;
    EditProperty: TEdit;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    ListBoxProperty: TListBox;
    ListView1: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ButtonImportPropertyClick(Sender: TObject);
    procedure ButtonDeletePropertyClick(Sender: TObject);
    procedure CheckBoxHasHeadersChange(Sender: TObject);
    procedure ComboBoxDelimiterChange(Sender: TObject);
    procedure FileNameEdit1AcceptFileName(Sender: TObject; var Value: String);
    procedure FileNameEdit1Change(Sender: TObject);
    procedure ListBoxPropertyClick(Sender: TObject);
  private
    FGozGraph : TGozIntoGraph;
  public
    function ShowModal: Integer; override;

  published
    property GozGraph : TGozIntoGraph read FGozGraph write FGozGraph;
  end;

var
  ImportItemPropertiesForm: TImportItemPropertiesForm;

implementation

{$R *.lfm}

uses gozfunc;


{ TImportItemPropertiesForm }

function TImportItemPropertiesForm.ShowModal: Integer;
var
  Ini : TInifile;
  s : string;
  item : TListitem;
begin
  if FGozGraph=nil then exit;

  try
    Ini:=TInifile.Create(ConfigFilename);
    s:='Import CSV Props';
    ComboBoxDelimiter.Text:=Ini.ReadString(s,'Delimiter',';');
    ComboBoxItem.Text:=Ini.ReadString(s,'Item','');
    CheckBoxHasHeaders.Checked:=Ini.ReadBool(s,'HasHeaders',false);

    Ini.Free;
  except
  end;

  Listview1.Items.Clear;
  FGozGraph.Properties.UpdateListview(Listview1);

  item:=Listview1.Items.Add;
  item.ImageIndex:=-1;
  item.caption:=NEW_ITEM;

  Result:=inherited ShowModal;
end;

procedure TImportItemPropertiesForm.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TImportItemPropertiesForm.ButtonImportPropertyClick(Sender: TObject);
var
  item : TListitem;
begin
  if Listview1.Selected=nil then
    begin
      MessageDlg('Vorher eine Eigenschaft in der Liste auswählen!',mtInformation,[mbOk],0);
      exit;
    end;

  item:=Listview1.Selected;

  if item.Caption=NEW_ITEM then
    begin
      if ListBoxProperty.ItemIndex=-1 then
        begin
          MessageDlg('Vorher eine Spalte auswählen.',mtInformation,[mbOk],0);
          exit;
        end;
      if EditProperty.Text='' then
        begin
          MessageDlg('Vorher eine Spaltennamen vergeben.',mtInformation,[mbOk],0);
          exit;
        end;
      item:=Listview1.Items.Insert(Listview1.Items.Count-1);
      item.ImageIndex:=2; //new
      item.Caption:=EditProperty.Text;
      item.SubItems.Add(ListBoxProperty.Items[ListBoxProperty.ItemIndex]);
      if ListBoxProperty.ItemIndex<ListBoxProperty.Items.Count-1 then
        begin
          ListBoxProperty.ItemIndex:=ListBoxProperty.ItemIndex+1;
          ListBoxPropertyClick(Sender);
        end;
    end
  else
    begin
      item.ImageIndex:=1; //update
      if item.SubItems.Count=0 then item.SubItems.Add('');
      item.SubItems[0]:=ListBoxProperty.Items[ListBoxProperty.ItemIndex];
    end;

end;

procedure TImportItemPropertiesForm.ButtonDeletePropertyClick(Sender: TObject);
var
  item : TListitem;
begin
  if Listview1.Selected=nil then
    begin
      MessageDlg('Vorher einen Eintrag in der Liste auswählen!',mtInformation,[mbOk],0);
      exit;
    end;

  item:=Listview1.Selected;

  if item.ImageIndex=1 then
    begin
      item.ImageIndex:=0;
      item.SubItems[0]:='';
    end
  else if item.ImageIndex=2 then item.Delete;

end;

procedure TImportItemPropertiesForm.CheckBoxHasHeadersChange(Sender: TObject);
begin
  FileNameEdit1Change(Sender);
end;

procedure TImportItemPropertiesForm.ComboBoxDelimiterChange(Sender: TObject);
begin
  FileNameEdit1Change(Sender);
end;

procedure TImportItemPropertiesForm.FileNameEdit1AcceptFileName(
  Sender: TObject; var Value: String);
begin
  FileNameEdit1Change(Sender);
end;

procedure TImportItemPropertiesForm.FileNameEdit1Change(Sender: TObject);
var
  sl : TStringList;
  sr : TStringArray;
  i,j,k : integer;
begin
  if FileExists(FileNameEdit1.FileName)=false then exit;

  sl:=TStringList.Create;
  sl.LoadFromFile(FileNameEdit1.FileName);

  if sl.Count>5 then j:=5
  else sl.Count;

  StringGrid1.RowCount:=j+1;
  StringGrid1.ColCount:=1;

  if CheckBoxHasHeaders.Checked then StringGrid1.FixedRows:=2
  else StringGrid1.FixedRows:=1;
  for i:=0 to j-1 do
    begin
      sr:=sl[i].Split(ComboBoxDelimiter.Text);
      if length(sr)>StringGrid1.ColCount then StringGrid1.ColCount:=length(sr);
      for k:=0 to length(sr)-1 do
        StringGrid1.Cells[k,i+1]:=sr[k];
    end;

  ComboBoxItem.Items.Clear;
  for k:=0 to StringGrid1.ColCount-1 do
    begin
      StringGrid1.Cells[k,0]:=IntToStr(k+1);
      ComboBoxItem.Items.Add(IntToStr(k+1));
    end;

  ListBoxProperty.Items.Assign(ComboBoxItem.Items);

  sl.Free;
end;

procedure TImportItemPropertiesForm.ListBoxPropertyClick(Sender: TObject);
var
  index : integer;
begin
  if ListBoxProperty.ItemIndex=-1 then exit;

  index:=StrInt(ListBoxProperty.Items[ListBoxProperty.ItemIndex]);
  if CheckBoxHasHeaders.Checked and (index>0)then
    EditProperty.Text:=StringGrid1.Cells[index-1,1];
end;

procedure TImportItemPropertiesForm.Button1Click(Sender: TObject);
var
  PosItem : integer;
  s : string;
begin
  if ComboBoxItem.Text='' then
    begin
      MessageDlg('Achtung','Bitte vorher Element eintragen.',mtInformation,[mbOK],0);
      exit;
    end;

  if ComboBoxDelimiter.Text='' then
    begin
      MessageDlg('Achtung','Bitte vorher Trennzeichen eintragen.',mtInformation,[mbOK],0);
      exit;
    end;

  if not FileExists(FileNameEdit1.FileName) then
    begin
      MessageDlg('Achtung','Bitte vorher eine gültige CSV-Datei auswählen.',mtInformation,[mbOK],0);
      exit;
    end;

  if ComboBoxItem.Text='' then PosItem:=-1
  else PosItem:=StrInt(ComboBoxItem.Text);

  FGozGraph.ImportPropsFromCsvFile(
    FileNameEdit1.FileName,
    ComboBoxDelimiter.Text,
    CheckboxHasHeaders.Checked,
    PosItem,
    Listview1);

  Close;
end;

end.

