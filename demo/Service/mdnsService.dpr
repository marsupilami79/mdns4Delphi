program mdnsService;

uses
  Vcl.Forms,
  mdnsService_Main in 'mdnsService_Main.pas' {FormService};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormService, FormService);
  Application.Run;
end.
