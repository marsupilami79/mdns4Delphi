unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  windns, Vcl.StdCtrls, IdBaseComponent, IdComponent, IdCustomTCPServer,
  IdTCPServer, IdContext;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    IdTCPServer1: TIdTCPServer;
    procedure Button1Click(Sender: TObject);
    procedure IdTCPServer1Execute(AContext: TIdContext);
  private
    { Private-Deklarationen }
    ServiceCancel: TDNS_SERVICE_CANCEL;
    RegisterCompleteStatus: DWORD;
    procedure RegisterCompleteCallback;
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure DnsServiceRegisterComplete(Status: DWORD; pQueryContext: Pointer; pInstance: PDNS_SERVICE_INSTANCE); winapi;
begin
  Form1.RegisterCompleteStatus := Status;
  TThread.ForceQueue(nil, Form1.RegisterCompleteCallback, 100);
end;

procedure TForm1.IdTCPServer1Execute(AContext: TIdContext);
begin
//
end;

procedure TForm1.RegisterCompleteCallback;
begin
  Form1.Memo1.Lines.Add('CompleteCallback:' + IntToStr(RegisterCompleteStatus));
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Request: TDNS_SERVICE_REGISTER_REQUEST;
  ServiceInstance: TDNS_SERVICE_INSTANCE;

  ServiceName: String;
  HostName: String;

  Res: DWORD;

  Err: Cardinal;
  PService: PDNS_SERVICE_INSTANCE;
begin
  IdTCPServer1.Active := true;

  InitWindns;

  //ServiceName := 'topsales._http._tcp.local';
  ServiceName := '_https._tcp.local';
  HostName := 'paulchen.local';

  ServiceInstance.pszInstanceName := PWideChar(ServiceName);
  ServiceInstance.pszHostName := PWideChar(HostName);
  ServiceInstance.ip4Address := nil; //@ip4address;
  ServiceInstance.ip6Address := nil; //@ip6address;
  ServiceInstance.wPort := 12345;
  ServiceInstance.wPriority := 0;
  ServiceInstance.wWeight := 0;
  ServiceInstance.dwPropertyCount := 0;
  ServiceInstance.Keys := nil;
  ServiceInstance.Values := nil;
  ServiceInstance.dwInterfaceIndex := 0;

  Request.Version := DNS_QUERY_REQUEST_VERSION1;
  Request.InterfaceIndex := 0;
  Request.pServiceInstance := @ServiceInstance;
  Request.pRegisterCompletionCallback := @DnsServiceRegisterComplete;
  Request.pQueryContext := nil;
  Request.hCredentials := 0;
  Request.unicastEnabled := false;

  Res := DnsServiceRegister(@Request, nil);
  if Res <> DNS_REQUEST_PENDING then
    Err := GetLastError;
  try
    if Res <> DNS_REQUEST_PENDING then begin
      //RaiseLastOSError
      Memo1.Lines.Add(IntToStr(Res) + ' -> ' + IntToStr(Err));
    end else
      Memo1.Lines.Append('DnsServiceRegister: Pending...');
  except
    Memo1.Lines.Add(IntToStr(Res));
  end;
end;

end.
