unit main;

{$mode objfpc}{$H+}

{
Gozintograph Viewer

https://techpluscode.de/produktstruktur-analyse-im-gozintograph/
https://github.com/tangielsky/gozintograph

(C) 2021 Thomas Angielsky



Version 0.1: first realeased version
             compiled with Lazarus 2.0.12

}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
  ExtCtrls, Buttons, StdCtrls, ComCtrls, Types, LCLIntf, Inifiles, gozgraph;


const
  URL_TECHPLUSCODE = 'https://techpluscode.de';


type
  { TMainForm }

  TMainForm = class(TForm)
    ActionImportCsvIcon: TAction;
    ActionZoomAll: TAction;
    ActionAutolayout: TAction;
    ActionZoomOut: TAction;
    ActionZoomIn: TAction;
    ActionAbout: TAction;
    ActionHomepage: TAction;
    ActionSetupPreferences: TAction;
    ActionImportCsv: TAction;
    ActionListIconLarge: TActionList;
    ActionListIconSmall: TActionList;
    CheckBoxSelectionDown: TCheckBox;
    CheckBoxSelectionUp: TCheckBox;
    CheckBoxShowQuantities: TCheckBox;
    CheckBoxShowCaptions: TCheckBox;
    ImageListIconLarge: TImageList;
    ImageListIconSmall: TImageList;
    LabelFilename: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    LabelZoom: TLabel;

    GozGraph : TGozIntoGraph;
    Label9: TLabel;
    LabelX: TLabel;
    ListBox1: TListBox;
    ListViewSelected: TListView;
    Panel1: TPanel;
    Panel19: TPanel;
    Panel2: TPanel;
    Panel29: TPanel;
    Panel3: TPanel;
    Panel30: TPanel;
    Panel31: TPanel;
    Panel32: TPanel;
    Panel33: TPanel;
    Panel36: TPanel;
    Panel39: TPanel;
    Panel4: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    PanelItemDetails: TPanel;
    ScrollBox1: TScrollBox;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    TrackBarZoom: TTrackBar;
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionAutolayoutExecute(Sender: TObject);
    procedure ActionHomepageExecute(Sender: TObject);
    procedure ActionImportCsvExecute(Sender: TObject);
    procedure ActionSetupPreferencesExecute(Sender: TObject);
    procedure ActionZoomAllExecute(Sender: TObject);
    procedure ActionZoomInExecute(Sender: TObject);
    procedure ActionZoomOutExecute(Sender: TObject);
    procedure CheckBoxSelectionDownChange(Sender: TObject);
    procedure CheckBoxSelectionUpChange(Sender: TObject);
    procedure CheckBoxShowCaptionsChange(Sender: TObject);
    procedure CheckBoxShowQuantitiesChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ScrollBox1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure ScrollBox1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure TrackBarZoomChange(Sender: TObject);
  private
    FirstStart : boolean;
    procedure ApplyZoom(ZoomDelta: integer);
    procedure GozGraphItemSelected( item: TGozGraphItem);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses preferences, importcsv, about;


{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FirstStart:=true;

  GozGraph:=TGozIntoGraph.Create(MainForm);
  GozGraph.Align:=alNone;
  GozGraph.Top:=0;
  GozGraph.Left:=0;
  GozGraph.OnItemSelected:=@GozGraphItemSelected;
  Scrollbox1.InsertControl(GozGraph);
end;

procedure TMainForm.FormActivate(Sender: TObject);
var
  Ini : TInifile;
  s,e : string;
begin
  if FirstStart=false then exit;
  FirstStart:=false;

  PreferencesForm.Filename:=copy(
    ExtractFilename(ParamStr(0)),1,
    length(ExtractFilename(ParamStr(0)))-length(ExtractFileExt(ParamStr(0))))
    +'.ini';
  PreferencesForm.Apply;

  try
    Ini:=TInifile.create(PreferencesForm.Filename);
    s:='Window';
    Left:=Ini.ReadInteger(s,'Left',50);
    Top:=Ini.ReadInteger(s,'Top',50);
    Width:=Ini.ReadInteger(s,'Width',800);
    Height:=Ini.ReadInteger(s,'Height',600);

    s:='View';
    CheckBoxSelectionUp.Checked:=Ini.ReadBool(s,'SelectionUp',false);
    CheckBoxSelectionDown.Checked:=Ini.ReadBool(s,'SelectionDown',false);
    CheckBoxShowQuantities.Checked:=Ini.ReadBool(s,'ShowQuantities',false);
    CheckBoxShowCaptions.Checked:=Ini.ReadBool(s,'ShowCaptions',true);

    Ini.Free;
  except
  end;

  CheckBoxSelectionUpChange(Sender);
  CheckBoxSelectionDownChange(Sender);
  CheckBoxShowQuantitiesChange(Sender);
  CheckBoxShowCaptionsChange(Sender);

end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Ini : TInifile;
  s : string;
begin
  try
    Ini:=TInifile.create(PreferencesForm.Filename);
    s:='Window';
    Ini.WriteInteger(s,'Left',Left);
    Ini.WriteInteger(s,'Top',Top);
    Ini.WriteInteger(s,'Width',Width);
    Ini.WriteInteger(s,'Height',Height);

    s:='View';
    Ini.WriteBool(s,'SelectionUp',CheckBoxSelectionUp.Checked);
    Ini.WriteBool(s,'SelectionDown',CheckBoxSelectionDown.Checked);
    Ini.WriteBool(s,'ShowQuantities',CheckBoxShowQuantities.Checked);
    Ini.WriteBool(s,'ShowCaptions',CheckBoxShowCaptions.Checked);

    Ini.Free;
  except
  end;
end;



procedure TMainForm.ApplyZoom(ZoomDelta : integer);
begin
  TrackBarZoom.Position:=TrackBarZoom.Position+ZoomDelta;
  GozGraph.ScaleFactor:=TrackBarZoom.Position/100;
  LabelZoom.Caption:=IntToStr(TrackBarZoom.Position)+'%';
end;

procedure TMainForm.ActionAboutExecute(Sender: TObject);
begin
  try
    AboutForm:=TAboutForm.Create(self);
    AboutForm.ShowModal;
  finally
    AboutForm.Free;
  end;
end;

procedure TMainForm.ActionAutolayoutExecute(Sender: TObject);
begin
  GozGraph.Autolayout;
end;

procedure TMainForm.ActionHomepageExecute(Sender: TObject);
begin
  OpenUrl(URL_TECHPLUSCODE);
end;

procedure TMainForm.ActionImportCsvExecute(Sender: TObject);
begin
  ImportCsvForm.ShowModal;
end;

procedure TMainForm.ActionSetupPreferencesExecute(Sender: TObject);
begin
  PreferencesForm.ShowModal;
  GozGraph.Repaint;
end;

procedure TMainForm.ActionZoomAllExecute(Sender: TObject);
begin
  GozGraph.SetScaleFactorFromWidth(Scrollbox1.Width);
  TrackBarZoom.Position:=Round(GozGraph.ScaleFactor*100);
  LabelZoom.Caption:=IntToStr(TrackBarZoom.Position)+'%';
end;

procedure TMainForm.ActionZoomInExecute(Sender: TObject);
begin
  ApplyZoom(1);
end;

procedure TMainForm.ActionZoomOutExecute(Sender: TObject);
begin
  ApplyZoom(-1);
end;

procedure TMainForm.CheckBoxSelectionDownChange(Sender: TObject);
begin
  GozGraph.SelectionAllDown:=CheckBoxSelectionDown.Checked;
end;

procedure TMainForm.CheckBoxSelectionUpChange(Sender: TObject);
begin
  GozGraph.SelectionAllUp:=CheckBoxSelectionUp.Checked;
end;

procedure TMainForm.CheckBoxShowCaptionsChange(Sender: TObject);
begin
  GozGraph.ShowCaptions:=CheckBoxShowCaptions.Checked;
end;

procedure TMainForm.CheckBoxShowQuantitiesChange(Sender: TObject);
begin
  GozGraph.ShowQuantities:=CheckBoxShowQuantities.Checked;
end;


procedure TMainForm.GozGraphItemSelected(item : TGozGraphItem);
  procedure Add(caption, value : string);
  var
    ListItem : TListItem;
  begin
    ListItem:=ListViewSelected.Items.Add;
    ListItem.Caption:=caption;
    ListItem.SubItems.Add(value);
  end;

begin
  ListViewSelected.BeginUpdate;
  ListViewSelected.Items.Clear;
  if item<>nil then
    begin
      Add('Name',item.caption);
      Add('Typ',item.ItemTypeCaption);
      Add('Dispo.-Stufe',IntToStr(item.Level));
      Add('Eingehend',IntToStr(item.IngoingInformation.Items)+' Element(e)');
      Add('',FloatToStr(item.IngoingInformation.Quantity)+' Stk');
      Add('Ausgehend',IntToStr(item.OutgoingInformation.Items)+' Element(e)');
      Add('',FloatToStr(item.OutgoingInformation.Quantity)+' Stk');
    end;
  ListViewSelected.EndUpdate;
end;


procedure TMainForm.ScrollBox1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then ApplyZoom(-1);
end;

procedure TMainForm.ScrollBox1MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then ApplyZoom(1);
end;

procedure TMainForm.TrackBarZoomChange(Sender: TObject);
begin
  ApplyZoom(0);
end;

end.

