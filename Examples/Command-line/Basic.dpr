program Basic;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  CL,
  OpenCLIPP in '..\..\Source\OpenCLIPP.pas';

var
  ContextPtr: TOcipContext;
  Error: TOcipError;
begin
  try
    Error := ocipInitialize(ContextPtr, PAnsiChar('Intel'), CL_DEVICE_TYPE_CPU);
    if Error <> 0 then
      Writeln(ocipGetErrorName(Error));

    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

