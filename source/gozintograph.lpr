program gozintograph;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, lazcontrols, gozgraph, main, about,
  preferences, importcsv, gozinfo, itemprops, gozfunc, colfind, importprops,
  modprop;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Title:='GozIntoGraph';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPreferencesForm, PreferencesForm);
  Application.CreateForm(TImportCsvForm, ImportCsvForm);
  Application.CreateForm(TColorFinderForm, ColorFinderForm);
  Application.CreateForm(TImportItemPropertiesForm, ImportItemPropertiesForm);
  Application.CreateForm(TModifyPropertiesForm, ModifyPropertiesForm);
  Application.Run;
end.

