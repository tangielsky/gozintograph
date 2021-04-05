unit importcsv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn, Grids;

type

  { TImportCsvForm }

  TImportCsvForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBoxHasHeaders: TCheckBox;
    ComboBoxSource: TComboBox;
    ComboBoxDestination: TComboBox;
    ComboBoxQuantity: TComboBox;
    ComboBoxDelimiter: TComboBox;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBoxHasHeadersChange(Sender: TObject);
    procedure ComboBoxDelimiterChange(Sender: TObject);
    procedure FileNameEdit1Change(Sender: TObject);
  private
    function StrInt(s : string) : integer;

  public

  end;

var
  ImportCsvForm: TImportCsvForm;

implementation

{$R *.lfm}

uses main, gozgraph;


{ TImportCsvForm }

procedure TImportCsvForm.Button1Click(Sender: TObject);
var
  j : integer;
begin
  if (ComboboxSource.Text='') or (ComboboxDestination.Text='') then
    begin
      MessageDlg('Achtung','Bitte vorher Zielknoten und Startknoten eintragen.',mtWarning,[mbOK],0);
      exit;
    end;

  if ComboboxQuantity.Text='' then j:=-1
  else j:=StrInt(ComboboxQuantity.Text);

  MainForm.LabelFilename.Caption:=FileNameEdit1.FileName;

  MainForm.GozGraph.ImportFromCsvFile(FileNameEdit1.FileName, ComboBoxDelimiter.Text,
    CheckboxHasHeaders.Checked,
    StrInt(ComboboxSource.Text),StrInt(ComboboxDestination.Text),j);

  Close;
end;

procedure TImportCsvForm.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TImportCsvForm.CheckBoxHasHeadersChange(Sender: TObject);
begin
  FileNameEdit1Change(Sender);
end;

procedure TImportCsvForm.ComboBoxDelimiterChange(Sender: TObject);
begin
  FileNameEdit1Change(Sender);
end;



procedure TImportCsvForm.FileNameEdit1Change(Sender: TObject);
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

  ComboboxDestination.Items.Clear;
  for k:=0 to StringGrid1.ColCount-1 do
    begin
      StringGrid1.Cells[k,0]:=IntToStr(k+1);
      ComboboxDestination.Items.Add(IntToStr(k+1));
    end;

  ComboboxSource.Items.Assign(ComboboxDestination.Items);
  ComboboxQuantity.Items.Assign(ComboboxDestination.Items);

  sl.Free;

end;

function TImportCsvForm.StrInt(s: string): integer;
begin
  try
    result:=StrToInt(s);
  except
    result:=0;
  end;
end;

end.

