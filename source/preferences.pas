unit preferences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Spin, EditBtn, Inifiles, gozgraph;

type
  { TPreferencesForm }

  TPreferencesForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBoxItemDetails: TCheckBox;
    CheckBoxLinks: TCheckBox;
    ColorButtonBackground: TColorButton;
    ColorButtonItem: TColorButton;
    ColorButtonItemSelected: TColorButton;
    ColorButtonModule: TColorButton;
    ColorButtonLinkSelected: TColorButton;
    ColorButtonProduct: TColorButton;
    ColorButtonLink: TColorButton;
    ColorButtonItemText: TColorButton;
    ColorButtonLinkText: TColorButton;
    ComboBoxItemShape: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SpinEditItemDiameter: TSpinEdit;
    SpinEditDistanceX: TSpinEdit;
    SpinEditDistanceY: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FGozGraph : TGozIntoGraph;
  public
    procedure Apply;
    procedure LoadFromFile;
    procedure SaveToFile;

  published
    property GozGraph : TGozIntoGraph read FGozGraph write FGozGraph;

  end;

var
  PreferencesForm: TPreferencesForm;

implementation

{$R *.lfm}

uses main, gozfunc;

{ TPreferencesForm }

procedure TPreferencesForm.Button1Click(Sender: TObject);
begin
  SaveToFile;
  Apply;
  Close;
end;

procedure TPreferencesForm.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TPreferencesForm.LoadFromFile;
var
  Ini : TInifile;
  s : string;
begin
  try
    Ini:=TInifile.create(ConfigFilename);
    s:='Item';
    ColorButtonProduct.ButtonColor:=Ini.ReadInteger(s,'Product color',clGreen);
    ColorButtonModule.ButtonColor:=Ini.ReadInteger(s,'Module color',clNavy);
    ColorButtonItem.ButtonColor:=Ini.ReadInteger(s,'Item color',clBlue);
    ColorButtonItemSelected.ButtonColor:=Ini.ReadInteger(s,'Item selected color',clYellow);
    ColorButtonItemText.ButtonColor:=Ini.ReadInteger(s,'Item text color',clBlack);
    SpinEditItemDiameter.Value:=Ini.ReadInteger(s,'Item diameter',50);
    SpinEditDistanceX.Value:=Ini.ReadInteger(s,'Item distance x',75);
    SpinEditDistanceY.Value:=Ini.ReadInteger(s,'Item distance y',150);
    CheckBoxItemDetails.Checked:=Ini.ReadBool(s,'Item details',true);
    ComboBoxItemShape.ItemIndex:=Ini.ReadInteger(s,'Item shape',0);

    s:='Link';
    ColorButtonLink.ButtonColor:=Ini.ReadInteger(s,'Link color',clSilver);
    ColorButtonLinkSelected.ButtonColor:=Ini.ReadInteger(s,'Link selected color',clBlack);
    ColorButtonLinkText.ButtonColor:=Ini.ReadInteger(s,'Link text color',clSilver);
    CheckboxLinks.Checked:=Ini.ReadBool(s,'Link',true);

    s:='Background';
    ColorButtonBackground.ButtonColor:=Ini.ReadInteger(s,'Color',clWhite);

    Ini.Free;
  except
    MessageDlg('Achtung','Einstellungen konnten nicht geöffnet werden',mtWarning,[mbOK],0);
  end;
end;

procedure TPreferencesForm.SaveToFile;
var
  Ini : TInifile;
  s : string;
begin
  try
    Ini:=TInifile.create(ConfigFilename);
    s:='Item';
    Ini.WriteInteger(s,'Product color',ColorButtonProduct.ButtonColor);
    Ini.WriteInteger(s,'Module color',ColorButtonModule.ButtonColor);
    Ini.WriteInteger(s,'Item color',ColorButtonItem.ButtonColor);
    Ini.WriteInteger(s,'Item selected color',ColorButtonItemSelected.ButtonColor);
    Ini.WriteInteger(s,'Item text color',ColorButtonItemText.ButtonColor);
    Ini.WriteInteger(s,'Item diameter',SpinEditItemDiameter.Value);
    Ini.WriteInteger(s,'Item distance x',SpinEditDistanceX.Value);
    Ini.WriteInteger(s,'Item distance y',SpinEditDistanceY.Value);
    Ini.WriteBool(s,'Item details',CheckBoxItemDetails.Checked);
    Ini.WriteInteger(s,'Item shape',ComboBoxItemShape.ItemIndex);


    s:='Link';
    Ini.WriteInteger(s,'Link color',ColorButtonLink.ButtonColor);
    Ini.WriteInteger(s,'Link selected color',ColorButtonLinkSelected.ButtonColor);
    Ini.WriteInteger(s,'Link text color',ColorButtonLinkText.ButtonColor);
    Ini.WriteBool(s,'Link',CheckboxLinks.Checked);

    s:='Background';
    Ini.WriteInteger(s,'Color',ColorButtonBackground.ButtonColor);

    Ini.Free;
  except
    MessageDlg('Achtung','Einstellungen konnten nicht gesichert werden',mtWarning,[mbOK],0);
  end;
end;


procedure TPreferencesForm.Apply;
begin
  gozProductColor:=ColorButtonProduct.ButtonColor;
  gozModuleColor:=ColorButtonModule.ButtonColor;
  gozItemColor:=ColorButtonItem.ButtonColor;

  MainForm.PanelItemDetails.Visible:=CheckBoxItemDetails.Checked;

  with FGozGraph do
    begin
      Color:=ColorButtonBackground.ButtonColor;
      HighlightColor:=ColorButtonItemSelected.ButtonColor;
      TextColor:=ColorButtonItemText.ButtonColor;
      LinkTextColor:=ColorButtonLinkText.ButtonColor;
      LinkHighlightColor:=ColorButtonLinkSelected.ButtonColor;
      LinkColor:=ColorButtonLink.ButtonColor;
      ShowLinks:=CheckboxLinks.Checked;

      DefaultItemWidth:=SpinEditItemDiameter.Value;
      DefaultItemHeight:=SpinEditItemDiameter.Value;
      DistanceX:=SpinEditDistanceX.Value;
      DistanceY:=SpinEditDistanceY.Value;

      if ComboBoxItemShape.ItemIndex=1 then ItemShape:=gisMixed
      else ItemShape:=gisCircle;
    end;
end;

end.

