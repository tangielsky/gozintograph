unit gozfunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function StrInt(s: string): integer;
function StrFloat(s: string): extended;

var
  ConfigFilename : string;


implementation

function StrInt(s: string): integer;
begin
  try
    result:=StrToInt(s);
  except
    result:=0;
  end;
end;

function StrFloat(s: string): extended;
begin
  try
    DecimalSeparator:=',';
    result:=StrToFloat(s);
  except
    result:=0.0;
  end;
end;

end.

