program plyview;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, lazopenglcontext
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='PLYview';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TGLForm1, GLForm1);
  Application.Run;
end.

