unit mdnsCore;

interface

uses Types, SysUtils;

type
  TmdnsPtrRecord = record
    Name: String;
    NameHost: String;
  end;

  TmdnsSrvRecord = record
    Name: String;
    NameTarget: String;
    Port: Word;
    Weight: Word;
    Priority: Word;
  end;

  TmdnsTxtRecord = record
    Name: String;
    Strings: TStringDynArray;
  end;

  TmdnsARecord = record
    Name: String;
    IpAddress: String;
  end;

  TMdnsAaaaRecord = record
    Name: String;
    IpAddress: String;
  end;

  TmdnsResult = record
    PTR: TmdnsPtrRecord;
    SRV: TmdnsSrvRecord;
    A: TmdnsARecord;
    AAAA: TMdnsAaaaRecord;
    TXT: TmdnsTxtRecord;
    Host: String;
    Port: Word;
    Resolved: Boolean;
    Errorcode: Integer;
  end;

  TmdnsResolveEvent = procedure (Sender: TObject; const Result: TmdnsResult) of object;

  mdnsException = class(Exception);

implementation

end.
