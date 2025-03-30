unit mdnsResolver;

{$DEFINE MDSN_UNIT_INCLUDE}

{$IF DEFINED(WIN32) OR DEFINED(WIN64)}
{$I mdnsResolverWindows.pas}
{$IFEND}
{$IFDEF ANDROID}
{$I mdnsResolverDelphiAndroid.pas}
{$ENDIF}
