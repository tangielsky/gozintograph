unit about;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLIntf,
  ExtCtrls,

  // FPC 3.0 fileinfo reads exe resources as long as you register the appropriate units
  fileinfo,
  winpeimagereader, {need this for reading exe info}
  elfreader, {needed for reading ELF executables}
  machoreader {needed for reading MACH-O executables}
  ;



type

  { TAboutForm }

  TAboutForm = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelVersion: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label3Click(Sender: TObject);
  private

  public

  end;

var
  AboutForm: TAboutForm;
  FileVerInfo: TFileVersionInfo;

implementation

uses main;

{$R *.lfm}

{ TAboutForm }



procedure TAboutForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    LabelVersion.Caption:='Version '+FileVerInfo.VersionStrings.Values['FileVersion'];
    Label2.Caption:='(C)opyright '+FileVerInfo.VersionStrings.Values['LegalCopyright'];
  finally
    FileVerInfo.Free;
  end;

  Label1.Caption:=Application.Title;
  Image1.Picture.Assign(Application.Icon);
  Label3.Caption:=URL_TECHPLUSCODE;
end;

procedure TAboutForm.Label3Click(Sender: TObject);
begin
  OpenUrl(URL_TECHPLUSCODE);
end;

end.

