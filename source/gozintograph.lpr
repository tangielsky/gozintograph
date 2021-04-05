program gozintograph;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, lazcontrols, gozgraph,
  main, about, preferences, importcsv
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Title:='GozIntoGraph';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPreferencesForm, PreferencesForm);
  Application.CreateForm(TImportCsvForm, ImportCsvForm);
  Application.Run;
end.

