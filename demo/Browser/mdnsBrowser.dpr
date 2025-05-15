program mdnsBrowser;

uses
  Vcl.Forms,
  mdnsBrowser_Main in 'mdnsBrowser_Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
