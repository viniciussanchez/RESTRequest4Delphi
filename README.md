# REST Request for Delphi
![Delphi Supported Versions](https://img.shields.io/badge/Delphi%20Supported%20Versions-XE3..10.3%20Rio-blue.svg)
![Platforms](https://img.shields.io/badge/Supported%20platforms-Win32%20and%20Win64-red.svg)
![Compatibility](https://img.shields.io/badge/Compatibility-VCL,%20FireMonkey%20and%20uniGUI-brightgreen.svg)
 
## Prerequisites
 * `[Optional]` For ease I recommend using the Boss for installation
   * [**Boss**](https://github.com/HashLoad/boss) - Dependency Manager for Delphi
 
## Installation using Boss (dependency manager for Delphi applications)
```
boss install github.com/viniciussanchez/RESTRequest4Delphi
```

## Manual Installation
Add the following folders to your project, in *Project > Options > Resource Compiler > Directories and Conditionals > Include file search path*
```
../RESTRequest4Delphi/src/core
../RESTRequest4Delphi/src/interfaces
```

## Getting Started
You need to use RESTRequest4D.Request.Intf and RESTRequest4D.Request
```pascal
uses RESTRequest4D.Request.Intf, RESTRequest4D.Request;
```

#### Method

Use `Request.GetMethod` method to get the method set. `rmGET` is default parameter.

```pascal
begin
  Request.SetMethod(rmGET); // Use REST.Types
end;
``` 

#### URL

You can set the URL in several ways. Use the one that suits you.

```pascal
begin
  Request.SetBaseURL('http://localhost:8080/datasnap/rest/servermethods/method');
  Request.SetBaseURL('http://localhost:8080/datasnap/rest').SetResource('servermethods/method');
  Request.SetBaseURL('http://localhost:8080/datasnap/rest').SetResource('servermethods').SetResourceSuffix('method');
end;
``` 

To get the values set use:

```pascal
begin
  Request.GetBaseURL;
  Request.GetResource;
  Request.GetResourceSuffix;
  Request.GetFullRequestURL(True);
end;
```

In the `GetFullRequestURL` method the parameter indicates whether to add the parameters. Default is `True`.



## Samples

![RESTRequest4D](img/Screenshot_1.png)
