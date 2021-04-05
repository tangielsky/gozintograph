unit preferences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Spin, EditBtn, Inifiles;

type
  { TPreferencesForm }

  TPreferencesForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBoxItemDetails: TCheckBox;
    ColorButtonBackground: TColorButton;
    ColorButtonItem: TColorButton;
    ColorButtonItemSelected: TColorButton;
    ColorButtonModule: TColorButton;
    ColorButtonLinkSelected: TColorButton;
    ColorButtonProduct: TColorButton;
    ColorButtonLink: TColorButton;
    ColorButtonItemText: TColorButton;
    ColorButtonLinkText: TColorButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
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
    FFilename : string;
    procedure SetFilename(AValue: string);
  public
    procedure Apply;
    procedure LoadFromFile(AFilename : string);
    procedure SaveToFile(AFilename : string);
  published
    property Filename : string read FFilename write SetFilename;

  end;

var
  PreferencesForm: TPreferencesForm;

implementation

{$R *.lfm}

uses main, gozgraph;

{ TPreferencesForm }

procedure TPreferencesForm.Button1Click(Sender: TObject);
begin
  SaveToFile(FFilename);
  Apply;
  Close;
end;

procedure TPreferencesForm.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TPreferencesForm.LoadFromFile(AFilename: string);
var
  Ini : TInifile;
  s : string;
begin
  //messagedlg('Laden',mtInformation,[mbok],0);
  try
    Ini:=TInifile.create(AFilename);
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

    s:='Link';
    ColorButtonLink.ButtonColor:=Ini.ReadInteger(s,'Link color',clSilver);
    ColorButtonLinkSelected.ButtonColor:=Ini.ReadInteger(s,'Link selected color',clBlack);
    ColorButtonLinkText.ButtonColor:=Ini.ReadInteger(s,'Link text color',clSilver);

    s:='Background';
    ColorButtonBackground.ButtonColor:=Ini.ReadInteger(s,'Color',clWhite);

    Ini.Free;
  except
    MessageDlg('Achtung','Einstellungen konnten nicht geöffnet werden',mtWarning,[mbOK],0);
  end;
end;

procedure TPreferencesForm.SaveToFile(AFilename: string);
var
  Ini : TInifile;
  s : string;
begin
  try
    //messagedlg('Speichern:',mtInformation,[mbok],0);
    Ini:=TInifile.create(AFilename);
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

    s:='Link';
    Ini.WriteInteger(s,'Link color',ColorButtonLink.ButtonColor);
    Ini.WriteInteger(s,'Link selected color',ColorButtonLinkSelected.ButtonColor);
    Ini.WriteInteger(s,'Link text color',ColorButtonLinkText.ButtonColor);

    s:='Background';
    Ini.WriteInteger(s,'Color',ColorButtonBackground.ButtonColor);

    Ini.Free;
  except
    MessageDlg('Achtung','Einstellungen konnten nicht gesichert werden',mtWarning,[mbOK],0);
  end;
end;

procedure TPreferencesForm.SetFilename(AValue: string);
begin
  if FFilename=AValue then Exit;
  FFilename:=AValue;
  LoadFromFile(FFilename);
end;

procedure TPreferencesForm.Apply;
begin
  gozProductColor:=ColorButtonProduct.ButtonColor;
  gozModuleColor:=ColorButtonModule.ButtonColor;
  gozItemColor:=ColorButtonItem.ButtonColor;

  MainForm.PanelItemDetails.Visible:=CheckBoxItemDetails.Checked;

  with MainForm.GozGraph do
    begin
      Color:=ColorButtonBackground.ButtonColor;
      HighlightColor:=ColorButtonItemSelected.ButtonColor;
      TextColor:=ColorButtonItemText.ButtonColor;
      LinkTextColor:=ColorButtonLinkText.ButtonColor;
      LinkHighlightColor:=ColorButtonLinkSelected.ButtonColor;
      LinkColor:=ColorButtonLink.ButtonColor;

      DefaultItemWidth:=SpinEditItemDiameter.Value;
      DefaultItemHeight:=SpinEditItemDiameter.Value;
      DistanceX:=SpinEditDistanceX.Value;
      DistanceY:=SpinEditDistanceY.Value;
    end;
end;

end.

