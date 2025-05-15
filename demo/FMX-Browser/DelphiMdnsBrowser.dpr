program DelphiMdnsBrowser;

uses
  System.StartUpCopy,
  FMX.Forms,
  mdnsBrowser in 'mdnsBrowser.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
