{$IFNDEF MDSN_UNIT_INCLUDE}
unit mdnsResolverWindows;
{$ENDIF}

{$IFDEF FPC}
{$mode Delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, windns, mdnsCore, SyncObjs, Generics.Collections;

type
  TMdnsResolver = class(TComponent)
    protected
      FServiceType: UnicodeString;
      FOnResolved: TmdnsResolveEvent;
      FRequest: TDNS_SERVICE_BROWSE_REQUEST;
      FCancel: TDNS_SERVICE_CANCEL;
      FMainThreadId: Cardinal;
      FResultQueue: TList<PDNS_RECORDW>;
      FErrorQueue: TList<DWORD>;
      FErrorLock: TCriticalSection;
      FResultLock: TCriticalSection;
      procedure ProcessResults;
      procedure ProcessResult(pDnsRecord: PDNS_RECORDW);
      procedure ProcessError(Status: DWORD);
      function GetServiceType: String;
      procedure SetServiceType(NewType: String);
      procedure AddResult(Result: PDNS_RECORDW);
      procedure AddError(Error: DWORD);
    public
      procedure StartResolve;
      procedure StopResolve;
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
    published
      property ServiceType: String read GetServiceType write SetServiceType;
      property OnResolved: TmdnsResolveEvent read FOnResolved write FOnResolved;
  end;

implementation

uses Windows;

type
  PMdnsResolver = ^TMdnsResolver;

var
  CallBack: TDNS_CALLBACK;

function formatIpV6Address(Address: TIP6_ADDRESS): String;
var
  x: Integer;
begin
  Result := '';
  for x := 0 to 15 do begin
    if (x > 0) and ((x and 1) = 0) then Result := Result + ':';
    Result := Result + IntToHex(Address.IP6Byte[x]);
  end;
end;

function formatIpV4Address(Address: TIP4_ADDRESS): String;
var
  x: Integer;
begin
  Result := '';
  for x := 0 to 3 do begin
    if (x > 0) then Result := Result + '.';
    Result := Result + IntToStr(Address.IP4Byte[x]);
  end;
end;

function dnsbrowsecallback(Status: DWord; pQueryContext: Pointer; pDnsRecord: PDNS_RECORDW): TDNS_STATUS; winapi;
begin
  try
    if status = 0 then begin
      if Assigned(pDnsRecord) then
        TMdnsResolver(pQueryContext).AddResult(pDnsRecord);
    end else begin
      TMdnsResolver(pQueryContext).AddError(Status);
    end;
    Result := 0;
  except
    Result := -1;
  end;
end;

constructor TMdnsResolver.Create(AOwner: TComponent);
begin
  inherited;
  FResultQueue := TList<PDNS_RECORDW>.Create;
  FResultLock := TCriticalSection.Create;
  FErrorQueue := TList<DWORD>.Create;
  FErrorLock := TCriticalSection.Create;
end;

destructor TMdnsResolver.Destroy;
begin
  if Assigned(FResultQueue) then
    FreeAndNil(FResultQueue);
  if Assigned(FResultLock) then
    FreeAndNil(FResultLock);
  if Assigned(FErrorQueue) then
    FreeAndNil(FErrorQueue);
  if Assigned(FErrorLock) then
    FreeAndNil(FErrorLock);
  inherited;
end;


procedure TMdnsResolver.StartResolve;
var
  Status: TDNS_STATUS;
begin
  FMainThreadId := GetCurrentThreadId;
  InitWindns;
  CallBack.pBrowseCallback := @dnsbrowsecallback;

  FRequest.Version := 1;
  FRequest.InterfaceIndex := 0;
  FRequest.QueryName := @FServiceType[1];
  FRequest.pQueryContext := self;
  FRequest.Callback := CallBack;

  Status := DnsServiceBrowse(@FRequest, @FCancel);
  if Status <> DNS_REQUEST_PENDING then
    raise mdnsException.Create('DNS error ' + IntToStr(Status));
end;

function TMdnsResolver.GetServiceType: String;
begin
  Result := String(FServiceType);
end;

procedure TMdnsResolver.SetServiceType(NewType: String);
begin
  FServiceType := UnicodeString(NewType);
end;

procedure TMdnsResolver.ProcessResult(pDnsRecord: PDNS_RECORDW);
var
  Result: TmdnsResult;
  x: Integer;
  Str: PWideChar;
  Host: String;
  CurrentRecord: PDNS_RECORDW;
begin
  CurrentRecord := pDnsRecord;
  while Assigned(CurrentRecord) do begin
    if CurrentRecord^.wType = DNS_TYPE_PTR then begin
      Result.PTR.Name := CurrentRecord^.pName;
      Result.PTR.NameHost := CurrentRecord^.data.PTR.pNameHost;
    end else if CurrentRecord^.wType = DNS_TYPE_SRV then begin
      Result.SRV.Name := CurrentRecord^.pName;
      Result.SRV.NameTarget := CurrentRecord^.data.SRV.pNameTarget;
      Result.SRV.Port := CurrentRecord^.data.SRV.wPort;
      Result.SRV.Priority := CurrentRecord^.data.SRV.wPriority;
      Result.SRV.Weight := CurrentRecord^.data.SRV.wWeight;
    end else if CurrentRecord^.wType = DNS_TYPE_A then begin
      Result.A.Name := CurrentRecord^.pName;
      Result.A.IpAddress := formatIpV4Address(CurrentRecord^.data.A.IpAddress);
    end else if CurrentRecord^.wType = DNS_TYPE_TEXT then begin
      Result.TXT.Name := pDnsRecord^.pName;
      {$IFDEF WIN32}
      SetLength(Result.TXT.Strings, CurrentRecord^.data.TXT.dwStringCount);
      for x := 0 to Integer(CurrentRecord^.data.TXT.dwStringCount) -1  do begin
        Str := CurrentRecord^.data.TXT.pStringArray[x];
        Result.TXT.Strings[x] := Str;
      end;
      {$ENDIF}
    end else if CurrentRecord^.wType = DNS_TYPE_AAAA then begin
      Result.AAAA.Name := CurrentRecord^.pName;
      Result.AAAA.IpAddress := formatIpV6Address(CurrentRecord^.data.AAAA.Ip6Address);
    end;

    CurrentRecord := CurrentRecord^.Next;
  end;

  Result.Errorcode := 0;
  if Result.A.Name <> '' then
    Host := Result.A.IpAddress
  else if Result.AAAA.Name <> '' then
    Host := Result.AAAA.IpAddress
  else if Result.SRV.Name <> '' then
    Host := Result.SRV.NameTarget;

  if (Host <> '') and (Result.SRV.Name <> '') then begin
    Result.Host := Host;
    Result.Port := Result.SRV.Port;
  end;

  if Assigned(FOnResolved) and (Assigned(pDnsRecord)) then
    FOnResolved(Self, Result);

  DnsRecordListFree(pDnsRecord, DnsFreeRecordList);
end;

procedure TMdnsResolver.ProcessError(Status: DWORD);
var
  Result: TmdnsResult;
begin
  Result.PTR.Name := GetServiceType;
  Result.Errorcode := Status;
  Result.Resolved := False;
  if Assigned(FOnResolved) then
    FOnResolved(Self, Result);
end;

procedure TMdnsResolver.StopResolve;
begin
  raise mdnsException.Create('StopResolve is not implemented yet.');
end;

procedure TMdnsResolver.AddResult(Result: PDNS_RECORDW);
begin
  FResultLock.Enter;
  try
    FResultQueue.Add(Result);
  finally
    FResultLock.Leave;
  end;
  TThread.ForceQueue(nil, ProcessResults, 250);
end;

procedure TMdnsResolver.AddError(Error: DWORD);
begin
  FErrorLock.Enter;
  try
    FErrorQueue.Add(Error);
  finally
    FErrorLock.Leave;
  end;
  TThread.ForceQueue(nil, ProcessResults);
end;

procedure TMdnsResolver.ProcessResults;
var
  continueProcessing: Boolean;
  mdnsResult: PDNS_RECORDW;
  mdnsError: DWORD;
  hadResult: Boolean;
begin
  mdnsResult := nil;
  mdnsError := 0;
  continueProcessing := True;
  while continueProcessing do begin
    FResultLock.Enter;
    try
      hadResult := FResultQueue.Count >= 1;
      if hadResult then begin
        mdnsResult := FResultQueue.Items[0];
        FResultQueue.Delete(0);
      end;
      continueProcessing := FResultQueue.Count >= 1;

      if hadResult then
        ProcessResult(mdnsResult);
    finally
      FResultLock.Leave;
    end;
  end;

  continueProcessing := True;
  while continueProcessing do begin
    FErrorLock.Enter;
    try
      hadResult := FErrorQueue.Count >= 1;
      if hadResult then begin
        mdnsError := FErrorQueue.Items[0];
        FErrorQueue.Delete(0);
      end;
      continueProcessing := FErrorQueue.Count >= 1;

      if hadResult then
        ProcessError(mdnsError);
    finally
      FErrorLock.Leave;
    end;
  end;
end;

end.

