unit importcsv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn, Grids, Inifiles, gozgraph;

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
    Label6: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBoxHasHeadersChange(Sender: TObject);
    procedure ComboBoxDelimiterChange(Sender: TObject);
    procedure FileNameEdit1Change(Sender: TObject);
  private
   FGozGraph : TGozIntoGraph;
 public
   function ShowModal: Integer; override;

 published
   property GozGraph : TGozIntoGraph read FGozGraph write FGozGraph;

  end;

var
  ImportCsvForm: TImportCsvForm;

implementation

{$R *.lfm}

uses gozfunc;


{ TImportCsvForm }

procedure TImportCsvForm.Button1Click(Sender: TObject);
var
  j : integer;
  Ini : TInifile;
  s : string;
begin
  if (ComboboxSource.Text='') or (ComboboxDestination.Text='') then
    begin
      MessageDlg('Achtung','Bitte vorher Zielknoten und Startknoten eintragen.',mtInformation,[mbOK],0);
      exit;
    end;

  if ComboBoxDelimiter.Text='' then
    begin
      MessageDlg('Achtung','Bitte vorher Trennzeichen eintragen.',mtInformation,[mbOK],0);
      exit;
    end;

  if ComboboxQuantity.Text='' then j:=-1
  else j:=StrInt(ComboboxQuantity.Text);

  FGozGraph.ImportFromCsvFile(
    FileNameEdit1.FileName,
    ComboBoxDelimiter.Text,
    CheckboxHasHeaders.Checked,
    StrInt(ComboboxSource.Text),
    StrInt(ComboboxDestination.Text),
    j);


  try
    Ini:=TInifile.Create(ConfigFilename);
    s:='Import CSV';
    Ini.WriteString(s,'Delimiter',ComboBoxDelimiter.Text);
    Ini.WriteString(s,'Source',ComboBoxSource.Text);
    Ini.WriteString(s,'Destination',ComboBoxDestination.Text);
    Ini.WriteString(s,'Quantity',ComboBoxQuantity.Text);
    Ini.WriteBool(s,'HasHeaders',CheckBoxHasHeaders.Checked);
    Ini.Free;
  except
  end;


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

function TImportCsvForm.ShowModal: Integer;
var
  Ini : TInifile;
  s : string;
begin
  try
    Ini:=TInifile.Create(ConfigFilename);
    s:='Import CSV';
    ComboBoxDelimiter.Text:=Ini.ReadString(s,'Delimiter',';');
    ComboBoxSource.Text:=Ini.ReadString(s,'Source','');
    ComboBoxDestination.Text:=Ini.ReadString(s,'Destination','');
    ComboBoxQuantity.Text:=Ini.ReadString(s,'Quantity','');
    CheckBoxHasHeaders.Checked:=Ini.ReadBool(s,'HasHeaders',false);
    Ini.Free;
  except
  end;

  Result:=inherited ShowModal;
end;



end.

