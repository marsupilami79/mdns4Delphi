program mdnsBrowser;

uses
  Vcl.Forms,
  mdnsBrowser_Main in 'mdnsBrowser_Main.pas' {FormBrowser};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormBrowser, FormBrowser);
  Application.Run;
end.
