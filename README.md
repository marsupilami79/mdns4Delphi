# mdns4Delphi
This project implements helper objects for for doing DNS-SD in Delphi and Lazarus / Free Pascal applications.

## Service Discovery

Currently this project can do service discovery on Windows and Android.
Android support only works on Delphi.

## Registering a Service

For now services can only be registered on Windows.
A Linux implementation might follow.

## Units in this project

  * mdnsCore contains some shared data types and structures.
  * mdnsResolver is a proxy unit that imports mdnsResolverDelphiAndroid or mdnsResolverWindows depending on the current compiler architecture.
  * mdnsResolverDelphiAndroid implements a resolver for Delphi on Android. In most cases one will want to use the mdnsResolver unit instead.
  * mdnsResolverWindows implements a resolver for Windows that can be used with Delphi and Lazarus / Free Pascal. In most cases one will want to use the mdnsResolver unit instead.
  * mdnsService implements a service registration object for Windows that can be used with Delphi and Lazarus / Free Pascal
