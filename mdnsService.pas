unit mdnsService;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ELSE}
  {$IFDEF MSWINDOWS}
    {$DEFINE WINDOWS}
  {$ENDIF}
{$ENDIF}

uses Classes, Windns;

type
  TDnsStatus = (dsUnregistered, dsPending, dsRegistered);

  TMdnsService = class(TComponent)
    protected
      FStatus: TDnsStatus;
      FServiceName: String;
      FRealServiceName: UnicodeString;
      FPortNumber: Word;
      FHostName: UnicodeString;

      FRequest: TDNS_SERVICE_REGISTER_REQUEST;
      FServiceInstance: TDNS_SERVICE_INSTANCE;
      FCancel: TDNS_SERVICE_CANCEL;
      procedure SetActive(NewValue: Boolean);
      function GetActive: Boolean;
      procedure FinishRegistration;
    public
      procedure RegisterService;
      procedure UnregisterService;
    published
      property Active: Boolean read GetActive write SetActive;
      property ServiceName: String read FServiceName write FServiceName;
      property PortNumber: Word read FPortNumber write FPortNumber;
  end;

implementation

uses Windows, SysUtils;

// The Windows portion is taken from here:
// https://www.delphipraxis.net/107832-post3.html
// The Unix portion is taken from here:
// https://forum.lazarus.freepascal.org/index.php/topic,30885.msg196955.html#msg196955
function GetComputerName: String;
{$IFDEF WINDOWS}
var
  Size: DWORD;
{$IFEND}
begin
  {$IFDEF WINDOWS}
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength(Result, Size);
  if Windows.GetComputerName(PChar(Result), Size) then
    SetLength(Result, Size)
  else
    Result := '';
  {$ELSE}
  Result := GetHostName;
  {$IFEND}
end;

procedure DnsServiceRegisterComplete(Status: DWORD; pQueryContext: Pointer; pInstance: PDNS_SERVICE_INSTANCE); winapi;
begin
  {$IFDEF FPC}
  TThread.ForceQueue(nil, TMdnsService(pQueryContext).FinishRegistration);
  {$ELSE}
  TThread.ForceQueue(nil, TMdnsService(pQueryContext).FinishRegistration, 100);
  {$ENDIF}
end;

procedure TMdnsService.RegisterService;
var
  Res: DWORD;
begin
  if FStatus = dsUnregistered then begin
    InitWindns;

    FHostName := UnicodeString(LowerCase(GetComputerName) + '.local');
    FRealServiceName := UnicodeString(LowerCase(GetComputerName) + '.' + FServiceName);

    FServiceInstance.pszInstanceName := PWideChar(FRealServiceName);
    FServiceInstance.pszHostName := PWideChar(FHostName);
    FServiceInstance.ip4Address := nil; //@ip4address;
    FServiceInstance.ip6Address := nil; //@ip6address;
    FServiceInstance.wPort := FPortNumber;
    FServiceInstance.wPriority := 0;
    FServiceInstance.wWeight := 0;
    FServiceInstance.dwPropertyCount := 0;
    FServiceInstance.Keys := nil;
    FServiceInstance.Values := nil;
    FServiceInstance.dwInterfaceIndex := 0;

    FRequest.Version := DNS_QUERY_REQUEST_VERSION1;
    FRequest.InterfaceIndex := 0;
    FRequest.pServiceInstance := @FServiceInstance;
    FRequest.pRegisterCompletionCallback := @DnsServiceRegisterComplete;
    FRequest.pQueryContext := Self;
    FRequest.hCredentials := 0;
    FRequest.unicastEnabled := false;

    Res := DnsServiceRegister(@FRequest, @FCancel);
    if Res <> DNS_REQUEST_PENDING then
      RaiseLastOsError
    else begin
      FStatus := dsPending;
    end;
  end;
end;

procedure TMdnsService.UnregisterService;
var
  Res: DWORD;
begin
  case FStatus of
    dsPending: begin
      DnsServiceRegisterCancel(@FCancel);
      FStatus := dsUnregistered;
    end;
    dsRegistered: begin
      Res := DnsServiceDeRegister(@FRequest, nil);
      if Res <> DNS_REQUEST_PENDING then
        RaiseLastOSError;
        FStatus := dsUnregistered;
    end;
  end;
end;

procedure TMdnsService.SetActive(NewValue: Boolean);
begin
  if (FStatus <> dsUnregistered) and not NewValue then
    UnregisterService;
  if (FStatus = dsUnregistered) and NewValue then
    RegisterService;
end;

function TMdnsService.GetActive: Boolean;
begin
  Result := FStatus <> dsUnregistered;
end;

procedure TMdnsService.FinishRegistration;
begin
  FStatus := dsRegistered;
end;

end.
