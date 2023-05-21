unit windns;

{$ifdef FPC}
{$mode Delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils{$IFDEF FPC}, ctypes{$ENDIF};

type
  {$IFNDEF FPC}
    DWORD = Cardinal;
    QWORD = UInt64;
    clong = longint;
    culong = cardinal;
    TLibHandle = Cardinal;
  {$ENDIF}

  {windns.h}
  TIP4_ADDRESS = record
    case boolean of
      true: (IP4Dword: DWORD);
      false: (IP4Byte: Array[0..3] of Byte);
  end;
  PIP4_ADDRESS = ^TIP4_ADDRESS;

  TIP6_ADDRESS = packed record
    case integer of
      1: (IP6Qword: Array [0..1] of QWORD);
      2: (IP6Dword: Array [0..3] of DWORD);
      3: (IP6Word: Array [0..7] of WORD);
      4: (IP6Byte: Array [0..15] of BYTE);
  end;
  PIP6_ADDRESS = ^TIP6_ADDRESS;

  TDNS_STATUS = clong;

  TDNS_A_DATA = packed record
    IpAddress: TIP4_ADDRESS;
  end;
  PDNS_A_DATA = ^TDNS_A_DATA;

  TDNS_PTR_DATAW = packed record
    pNameHost: PWideChar;
  end;
  PDNS_PTR_DATAW = ^TDNS_PTR_DATAW;

  TDNS_SOA_DATAW = packed record
    pNamePrimaryServer: PWideChar;
    pNameAdministrator: PWideChar;
    dwSerialNo: DWORD;
    dwRefresh: DWORD;
    dwRetry: DWORD;
    dwExpire: DWORD;
    dwDefaultTtl: DWORD;
  end;
  PDNS_SOA_DATAW = ^TDNS_SOA_DATAW;

  TDNS_MINFO_DATAW = packed record
    pNameMailbox: PWideChar;
    pNameErrorsMailbox: PWideChar;
  end;

  TDNS_MX_DATAW = packed record
    pNameExchange: PWideChar;
    wPreference: WORD;
    Pad: WORD;        // keep ptrs DWORD aligned
  end;
  PDNS_MX_DATAW = ^TDNS_MX_DATAW;

  /// <summary>
  ///  This definition is probably wrong. Don't use it without checking back
  ///  with the original windns.h from Microsoft.
  /// </summary>
  TDNS_TXT_DATAW = packed record
    dwStringCount: DWORD;
    //pStringArray: Array of PWideChar;
    pStringArray: Array[0..255] of PWideChar;
  end;
  PDNS_TXT_DATAW = ^TDNS_TXT_DATAW;

  TDNS_AAAA_DATA = packed record
    Ip6Address: TIP6_ADDRESS;
  end;
  PDNS_AAAA_DATA = ^TDNS_AAAA_DATA;

  TDNS_SRV_DATAW = packed record
    pNameTarget: PWideChar;
    wPriority: WORD;
    wWeight: WORD;
    wPort: WORD;
    Pad: WORD;            // keep ptrs DWORD aligned
  end;
  PDNS_SRV_DATAW = ^TDNS_SRV_DATAW;

  TDNS_NAPTR_DATAW = packed record
    wOrder: WORD;
    wPreference: WORD;
    pFlags: PWideChar;
    pService: PWideChar;
    pRegularExpression: PWideChar;
    pReplacement: PWideChar;
  end;
  PDNS_NAPTR_DATAW = ^TDNS_NAPTR_DATAW;

  TDNS_RECORD_DATAW = record
    case integer of
      1: (A: TDNS_A_DATA);
      2: (SOA: TDNS_SOA_DATAW);
      3: (PTR: TDNS_PTR_DATAW);
      4: (NS: TDNS_PTR_DATAW);
      5: (CNAME: TDNS_PTR_DATAW);
      6: (DNAME: TDNS_PTR_DATAW);
      7: (MB: TDNS_PTR_DATAW);
      8: (MD: TDNS_PTR_DATAW);
      9: (MF: TDNS_PTR_DATAW);
      10: (MG: TDNS_PTR_DATAW);
      11: (MR: TDNS_PTR_DATAW);
      12: (MINFO: TDNS_MINFO_DATAW);
      13: (RP: TDNS_MINFO_DATAW);
      14: (MX: TDNS_MX_DATAW);
      15: (AFSDB: TDNS_MX_DATAW);
      16: (RT: TDNS_MX_DATAW);
      17: (HINFO: TDNS_TXT_DATAW);
      18: (ISDN: TDNS_TXT_DATAW);
      19: (TXT: TDNS_TXT_DATAW);
      20: (X25: TDNS_TXT_DATAW);
  //      21: (Null: TDNS_NULL_DATA);
  //      22: (WKS: TDNS_WKS_DATA);
      23: (AAAA: TDNS_AAAA_DATA);
  //      24: (KEY: TDNS_KEY_DATA);
  //      25: (SIG: TDNS_SIG_DATAW);
  //      26: (ATMA: TDNS_ATMA_DATA);
  //      27: (NXT: TDNS_NXT_DATAW);
      28: (SRV: TDNS_SRV_DATAW);
      29: (NAPTR: TDNS_NAPTR_DATAW);
  //      30: (OPT: TDNS_OPT_DATA);
  //      31: (DS: TDNS_DS_DATA);
  //      32: (RRSIG: TDNS_RRSIG_DATAW);
  //      33: (NSEC: DNS_NSEC_DATAW);
  //      34: (DNSKEY: DNS_DNSKEY_DATA);
  //      35: (TKEY: DNS_TKEY_DATAW);
  //      36: (TSIG: TDNS_TSIG_DATAW);
  //      37: (WINS: TDNS_WINS_DATA);
  //      38: (WINSR: TDNS_WINSR_DATAW);
  //      39: (NBSTAT: TDNS_WINSR_DATAW);
  //      40: (DHCID: TDNS_DHCID_DATA);
  //      41: (NSEC3: TDNS_NSEC3_DATA);
  //      42: (NSEC3PARAM: TDNS_NSEC3PARAM_DATA);
  //      43: (TLSA: TDNS_TLSA_DATA);
  //      44: (UNKNOWN: TDNS_UNKNOWN_DATA);
  //      45: (pDataPtr: PBYTE);
  end;
  PTDNS_RECORD_DATAW = ^TDNS_RECORD_DATAW;

  PDNS_RECORDW = ^TDNS_RECORDW;
  TDNS_RECORDW = record
    Next:  PDNS_RECORDW;
    pName: PWideChar;
    wType: Word;
    wDataLength: Word;
    Flags: DWord;
    dwTtl: Dword;
    dwReserved: DWord;
    data: TDNS_RECORD_DATAW;
  end;

  TDNS_SERVICE_CANCEL = record
    reserved: Pointer;
  end;
  PDNS_SERVICE_CANCEL = ^TDNS_SERVICE_CANCEL;

  TDNS_SERVICE_BROWSE_CALLBACK = function(Status: DWord; pQueryContext: Pointer; pDnsRecord: PDNS_RECORDW): TDNS_STATUS; winapi;

  TDNS_CALLBACK = packed record
    case boolean of
      true: (pBrowseCallback: Pointer);
  //      false: (PPBrowseCallbackV2: Pointer);
  end;

  TDNS_SERVICE_BROWSE_REQUEST = packed record
    Version: culong;
    InterfaceIndex: culong;
    QueryName: PWideChar;
    Callback: TDNS_CALLBACK;
    pQueryContext: Pointer;
  end;
  PDNS_SERVICE_BROWSE_REQUEST = ^TDNS_SERVICE_BROWSE_REQUEST;

  TDnsServiceBrowse = function(pRequest: PDNS_SERVICE_BROWSE_REQUEST; pCancel: PDNS_SERVICE_CANCEL): TDNS_STATUS; winapi;
  TDnsRecordListFree = procedure(var p: PDNS_RECORDW; t: NativeUInt); winapi;
  TDnsServiceBrowseCancel = function(pCancelHandle: PDNS_SERVICE_CANCEL): TDNS_STATUS; winapi;

const
  {DNS error codes are defined in winerror.h}
  DNS_REQUEST_PENDING = 9506;

  DNS_QUERY_REQUEST_VERSION1 = 1;

  //
  //  DNS Record Types
  //
  //  _TYPE_ defines are in host byte order.
  //  _RTYPE_ defines are in net byte order.
  //
  //  Generally always deal with types in host byte order as we index
  //  resource record functions by type.
  //

  DNS_TYPE_ZERO       = $0000;

  //  RFC 1034/1035
  DNS_TYPE_A          = $0001;      //  1
  DNS_TYPE_NS         = $0002;      //  2
  DNS_TYPE_MD         = $0003;      //  3
  DNS_TYPE_MF         = $0004;      //  4
  DNS_TYPE_CNAME      = $0005;      //  5
  DNS_TYPE_SOA        = $0006;      //  6
  DNS_TYPE_MB         = $0007;      //  7
  DNS_TYPE_MG         = $0008;      //  8
  DNS_TYPE_MR         = $0009;      //  9
  DNS_TYPE_NULL       = $000a;      //  10
  DNS_TYPE_WKS        = $000b;      //  11
  DNS_TYPE_PTR        = $000c;      //  12
  DNS_TYPE_HINFO      = $000d;      //  13
  DNS_TYPE_MINFO      = $000e;      //  14
  DNS_TYPE_MX         = $000f;      //  15
  DNS_TYPE_TEXT       = $0010;      //  16

  //  RFC 1183
  DNS_TYPE_RP         = $0011;      //  17
  DNS_TYPE_AFSDB      = $0012;      //  18
  DNS_TYPE_X25        = $0013;      //  19
  DNS_TYPE_ISDN       = $0014;      //  20
  DNS_TYPE_RT         = $0015;      //  21

  //  RFC 1348
  DNS_TYPE_NSAP       = $0016;      //  22
  DNS_TYPE_NSAPPTR    = $0017;      //  23

  //  RFC 2065    (DNS security)
  DNS_TYPE_SIG        = $0018;      //  24
  DNS_TYPE_KEY        = $0019;      //  25

  //  RFC 1664    (X.400 mail)
  DNS_TYPE_PX         = $001a;      //  26

  //  RFC 1712    (Geographic position)
  DNS_TYPE_GPOS       = $001b;      //  27

  //  RFC 1886    (IPv6 Address)
  DNS_TYPE_AAAA       = $001c;      //  28

  //  RFC 1876    (Geographic location)
  DNS_TYPE_LOC        = $001d;      //  29

  //  RFC 2065    (Secure negative response)
  DNS_TYPE_NXT        = $001e;      //  30

  //  Patton      (Endpoint Identifier)
  DNS_TYPE_EID        = $001f;      //  31

  //  Patton      (Nimrod Locator)
  DNS_TYPE_NIMLOC     = $0020;      //  32

  //  RFC 2052    (Service location)
  DNS_TYPE_SRV        = $0021;      //  33

  //  ATM Standard something-or-another (ATM Address)
  DNS_TYPE_ATMA       = $0022;      //  34

  //  RFC 2168    (Naming Authority Pointer)
  DNS_TYPE_NAPTR      = $0023;      //  35

  //  RFC 2230    (Key Exchanger)
  DNS_TYPE_KX         = $0024;      //  36

  //  RFC 2538    (CERT)
  DNS_TYPE_CERT       = $0025;      //  37

  //  A6 Draft    (A6)
  DNS_TYPE_A6         = $0026;      //  38

  //  DNAME Draft (DNAME)
  DNS_TYPE_DNAME      = $0027;      //  39

  //  Eastlake    (Kitchen Sink)
  DNS_TYPE_SINK       = $0028;      //  40

  //  RFC 2671    (EDNS OPT)
  DNS_TYPE_OPT        = $0029;      //  41

  //  RFC 4034    (DNSSEC DS)
  DNS_TYPE_DS         = $002b;      //  43

  //  RFC 4034    (DNSSEC RRSIG)
  DNS_TYPE_RRSIG      = $002e;      //  46

  //  RFC 4034    (DNSSEC NSEC)
  DNS_TYPE_NSEC       = $002f;      //  47

  //  RFC 4034    (DNSSEC DNSKEY)
  DNS_TYPE_DNSKEY     = $0030;      //  48

  //  RFC 4701    (DHCID)
  DNS_TYPE_DHCID      = $0031;      //  49

  //  RFC 5155    (DNSSEC NSEC3)
  DNS_TYPE_NSEC3      = $0032;      //  50

  //  RFC 5155    (DNSSEC NSEC3PARAM)
  DNS_TYPE_NSEC3PARAM = $0033;      //  51

  //RFC 6698	(TLSA)
  DNS_TYPE_TLSA	    = $0034;      //  52

  //
  //  IANA Reserved
  //

  DNS_TYPE_UINFO      = $0064;      //  100
  DNS_TYPE_UID        = $0065;      //  101
  DNS_TYPE_GID        = $0066;      //  102
  DNS_TYPE_UNSPEC     = $0067;      //  103

  //
  //  Query only types (1035, 1995)
  //      - Crawford      (ADDRS)
  //      - TKEY draft    (TKEY)
  //      - TSIG draft    (TSIG)
  //      - RFC 1995      (IXFR)
  //      - RFC 1035      (AXFR up)
  //

  DNS_TYPE_ADDRS      = $00f8;      //  248
  DNS_TYPE_TKEY       = $00f9;      //  249
  DNS_TYPE_TSIG       = $00fa;      //  250
  DNS_TYPE_IXFR       = $00fb;      //  251
  DNS_TYPE_AXFR       = $00fc;      //  252
  DNS_TYPE_MAILB      = $00fd;      //  253
  DNS_TYPE_MAILA      = $00fe;      //  254
  DNS_TYPE_ALL        = $00ff;      //  255
  DNS_TYPE_ANY        = $00ff;      //  255

  DnsFreeFlat = 0;
  DnsFreeRecordList = 1;
  DnsFreeParsedMessageFields = 2;

procedure InitWindns;

var
  DnsServiceBrowse: TDnsServiceBrowse;
  DnsRecordListFree: TDnsRecordListFree;
  DnsServiceBrowseCancel: TDnsServiceBrowseCancel;

implementation

uses mdnsCore{$IFNDEF FPC}{$IF DEFINED(WIN32) OR DEFINED(WIN64)}, Windows{$IFEND}{$ENDIF};

var
  Lib: NativeUInt;

function GetSymbol(Name: String): Pointer;
begin
  Result := GetProcAddress(Lib, 'DnsServiceBrowse');
  if not Assigned(Result) then
    raise mdnsException.Create('Could not load symbol ' + Name + ' from dnsapi.dll.');
end;

procedure InitWindns;
begin
  if Lib = 0 then begin
    Lib := LoadLibrary('dnsapi.dll');
    if Lib = 0 then
      RaiseLastOSError;

    DnsServiceBrowse := TDnsServiceBrowse(GetSymbol('DnsServiceBrowse'));
    DnsRecordListFree := TDnsRecordListFree(GetSymbol('DnsRecordListFree'));
    DnsServiceBrowseCancel := TDnsServiceBrowseCancel(GetSymbol('DnsServiceBrowseCancel'));
  end;
end;

end.
