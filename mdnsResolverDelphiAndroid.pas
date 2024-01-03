{$IFNDEF MDSN_UNIT_INCLUDE}
unit mdnsResolverDelphiAndroid;
{$ENDIF}

interface

uses mdnsCore, Classes, Generics.Collections, System.SyncObjs, FMX.Types,
     AndroidApi.JNI.Net;

type
  TMdnsResolver = class(TComponent)
    protected
      FServiceType: String;
      FSearchService: String;
      FOnResolved: TmdnsResolveEvent;
      FNsdManager: JNsdManager;
      ResolveQueue: TList<JNsdServiceInfo>;
      ResolveLock: TCriticalSection;
      ResultQueue: TList<TmdnsResult>;
      ResultLock: TCriticalSection;
      ResultTimer: TTimer;
      FDiscoveryListener: JNsdManager_DiscoveryListener;
      procedure OnDiscover(ServiceInfo: JNsdServiceInfo);
      procedure OnResolve(ServiceInfo: JNsdServiceInfo);
      procedure OnResolveError(ServiceInfo: JNsdServiceInfo; ErrorCode: Integer);
      procedure OnResultTimer(Sender: TObject);
      procedure ResolveNext;
      procedure AddResult(Result: TmdnsResult);
    public
      procedure StartResolve;
      procedure StopResolve;
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
    published
      property ServiceType: String read FServiceType write FServiceType;
      property OnResolved: TmdnsResolveEvent read FOnResolved write FOnResolved;
  end;

implementation

uses Androidapi.JNI.JavaTypes, Androidapi.JNIBridge, Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText, SysUtils, Androidapi.JNI.Java.Net;

const
  baseQueryName = '_services._dns-sd._udp';

type
  TDiscoveryListener = class(TJavaLocal, JNsdManager_DiscoveryListener)
    protected
      FResolver: TMdnsResolver;
    public
      procedure onDiscoveryStarted(serviceType: JString); cdecl;
      procedure onDiscoveryStopped(serviceType: JString); cdecl;
      procedure onServiceFound(serviceInfo: JNsdServiceInfo); cdecl;
      procedure onServiceLost(serviceInfo: JNsdServiceInfo); cdecl;
      procedure onStartDiscoveryFailed(serviceType: JString; errorCode: Integer); cdecl;
      procedure onStopDiscoveryFailed(serviceType: JString; errorCode: Integer); cdecl;
      constructor Create(Resolver: TMdnsResolver);
  end;

  TResolveListener = class(TJavaLocal, JNsdManager_ResolveListener)
    protected
      FResolver: TMdnsResolver;
    public
      procedure onResolveFailed(serviceInfo: JNsdServiceInfo; errorCode: Integer); cdecl;
      procedure onServiceResolved(serviceInfo: JNsdServiceInfo); cdecl;
      constructor Create(Resolver: TMdnsResolver);
  end;

constructor TMdnsResolver.Create(AOwner: TComponent);
begin
  inherited;
  ResolveQueue := TList<JNsdServiceInfo>.Create;
  ResolveLock := TCriticalSection.Create;
  ResultQueue := TList<TmdnsResult>.Create;
  ResultLock := TCriticalSection.Create;
  ResultTimer := TTimer.Create(self);
  ResultTimer.Interval := 1000;
  ResultTimer.OnTimer := OnResultTimer;
end;

destructor TMdnsResolver.Destroy;
begin
  if Assigned(ResultTimer) then
    ResultTimer.Enabled := False;
  if Assigned(ResultLock) then
    FreeAndNil(ResultLock);
  if Assigned(ResultQueue) then
    FreeAndNil(ResultQueue);
  if Assigned(ResolveLock) then
    FreeAndNil(ResolveLock);
  if Assigned(ResolveQueue) then
    FreeAndNil(ResolveQueue);
  inherited;
end;

procedure TMdnsResolver.StartResolve;
var
  Obj: JObject;
begin
  FSearchService := FServiceType;
  if FSearchService.EndsWith('.local.') then
    Delete(FSearchService, Length(FSearchService) - 6, 7)
  else if FSearchService[Length(FSearchService)] = '.' then
    Delete(FSearchService, Length(FSearchService), 1)
  else if FSearchService.EndsWith('.local') then
    Delete(FSearchService, Length(FSearchService) - 5, 6);


  if not Assigned(FNsdManager) then begin
    Obj := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NSD_SERVICE);
    if Assigned(Obj) then begin
      FNsdManager := TJNsdManager.Wrap((Obj as ILocalObject).GetObjectID);
      if not Assigned(FNsdManager) then
        raise mdnsException.Create('Could not get an NsdManager interface.');
    end else
      raise mdnsException.Create('Obj is empty.');
  end;

  FDiscoveryListener := TDiscoveryListener.Create(self) as JNsdManager_DiscoveryListener;
  //FDiscoveryListener := Unit1.TDiscoveryListener.Create as JNsdManager_DiscoveryListener;
  FNsdManager.discoverServices(StringToJString(FSearchService), TJNsdManager.JavaClass.PROTOCOL_DNS_SD, FDiscoveryListener);
  ResultTimer.Enabled := True;
end;

procedure TMdnsResolver.StopResolve;
begin
  if Assigned(FNsdManager) and Assigned(FDiscoveryListener) then begin
    FNsdManager.stopServiceDiscovery(FDiscoveryListener);
    FDiscoveryListener := nil;
  end;
  ResultTimer.Enabled := False;
end;


procedure TMdnsResolver.OnDiscover(ServiceInfo: JNsdServiceInfo);
begin
  ResolveLock.Enter;
  try
    ResolveQueue.Add(ServiceInfo);
    if ResolveQueue.Count = 1 then
      FNsdManager.resolveService(ResolveQueue.Items[0], TResolveListener.Create(self) as JNsdManager_ResolveListener);
  finally
    ResolveLock.Leave;
  end;
end;

procedure TMdnsResolver.ResolveNext;
var
  Listener: JNsdManager_ResolveListener;
begin
  ResolveLock.Enter;
  try
    ResolveQueue.Delete(0);
    if ResolveQueue.Count >= 1 then begin
      Listener := TResolveListener.Create(self) as JNsdManager_ResolveListener;
      FNsdManager.resolveService(ResolveQueue.Items[0], Listener);
    end;
  finally
    ResolveLock.Leave;
  end;
end;

procedure TMdnsResolver.AddResult(Result: TmdnsResult);
begin
  ResultLock.Enter;
  try
    ResultQueue.Add(Result);
  finally
    ResultLock.Leave;
  end;
end;

procedure TMdnsResolver.OnResolve(ServiceInfo: JNsdServiceInfo);
var
  ResultRec: TmdnsResult;
  Host: JInetAddress;
begin
  if FSearchService = baseQueryName then begin
    ResultRec.PTR.Name := baseQueryName;
    ResultRec.PTR.NameHost := JStringToString(ServiceInfo.getServiceName) + '.' + JStringToString(ServiceInfo.getServiceType);
  end else begin
    ResultRec.PTR.Name := JStringToString(ServiceInfo.getServiceType);
    ResultRec.PTR.NameHost := JStringToString(ServiceInfo.getServiceName);
  end;
  ResultRec.Errorcode := 0;

  Host := ServiceInfo.getHost;
  if Assigned(Host) then begin
    ResultRec.Host := JStringToString(Host.getHostAddress);
    ResultRec.Port := Word(ServiceInfo.getPort);

    // It doesn't make sense to fill these as Android doesn't give us enough information.
    // Accessing ServiceInfo.Attributes would allow reconstructing the TXT record.
    // Unfortunately I don't know how to access the attributes from Delphi
    {
    ResultRec.SRV.Name := ResultRec.PTR.NameHost;
    ResultRec.SRV.NameTarget := JStringToString(Host.getHostName);
    ResultRec.SRV.Port := ResultRec.Port;

    if Pos('.', ResultRec.Host) > 0 then begin
      ResultRec.A.Name := ResultRec.SRV.NameTarget;
      ResultRec.A.IpAddress := ResultRec.Host;
    end else begin
      ResultRec.AAAA.Name := ResultRec.SRV.NameTarget;
      ResultRec.AAAA.IpAddress := ResultRec.Host;
    end;
    }
  end;

  AddResult(ResultRec);

  ResolveNext;
end;

procedure TMdnsResolver.OnResolveError(ServiceInfo: JNsdServiceInfo; ErrorCode: Integer);
var
  ResultRec: TmdnsResult;
begin
  ResolveNext;

  if FSearchService = baseQueryName then begin
    ResultRec.PTR.Name := baseQueryName;
    ResultRec.PTR.NameHost := JStringToString(ServiceInfo.getServiceName) + '.' + JStringToString(ServiceInfo.getServiceType);
    ResultRec.Errorcode := 0;
    ResultRec.isError := False;
  end else begin
    ResultRec.PTR.Name := JStringToString(ServiceInfo.getServiceType);
    ResultRec.PTR.NameHost := JStringToString(ServiceInfo.getServiceName);
    ResultRec.Errorcode := ErrorCode;
    ResultRec.isError := True;
  end;

  AddResult(ResultRec);
end;

procedure TMdnsResolver.OnResultTimer(Sender: TObject);
var
  keepGoing: Boolean;
  ResultRec: TmdnsResult;
begin
  repeat
    ResultLock.Enter;
    try
      if ResultQueue.Count = 0 then begin
        ResultRec.Errorcode := -1;
      end else begin
        ResultRec := ResultQueue.Items[0];
        ResultQueue.Delete(0);
        keepGoing := ResultQueue.Count > 0;
      end;
    finally
      ResultLock.Leave;
    end;

    if ResultRec.Errorcode = -1 then
      keepGoing := false
    else begin
      keepGoing := True;
      if Assigned(FOnResolved) then
        FOnResolved(self, ResultRec);
    end;
  until not keepGoing;
end;

{------------------------------------------------------------------------------}

procedure TDiscoveryListener.onDiscoveryStarted(serviceType: JString);
begin
  // do nothing
end;

procedure TDiscoveryListener.onDiscoveryStopped(serviceType: JString);
begin
  // do nothing
end;

procedure TDiscoveryListener.onServiceFound(serviceInfo: JNsdServiceInfo);
begin
  FResolver.OnDiscover(serviceInfo);
end;

procedure TDiscoveryListener.onServiceLost(serviceInfo: JNsdServiceInfo);
begin
  // do nothing
end;

procedure TDiscoveryListener.onStartDiscoveryFailed(serviceType: JString; errorCode: Integer);
begin
  // do nothing
end;

procedure TDiscoveryListener.onStopDiscoveryFailed(serviceType: JString; errorCode: Integer);
begin
  // do nothing
end;

constructor TDiscoveryListener.Create(Resolver: TMdnsResolver);
begin
  inherited Create;
  FResolver := Resolver;
end;

{------------------------------------------------------------------------------}

procedure TResolveListener.onResolveFailed(serviceInfo: JNsdServiceInfo; errorCode: Integer);
begin
  FResolver.OnResolveError(serviceInfo, errorCode);
end;

procedure TResolveListener.onServiceResolved(serviceInfo: JNsdServiceInfo);
begin
  FResolver.OnResolve(serviceInfo);
end;

constructor TResolveListener.Create(Resolver: TMdnsResolver);
begin
  inherited Create;
  FResolver := Resolver;
end;



end.
