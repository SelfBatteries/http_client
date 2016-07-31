 '$Revision:$'
 '
Copyright 1992-2016 AUTHORS.
See the legal/LICENSE file for license information and legal/AUTHORS for authors.
'
["preFileIn" self] value


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> () From: ( | {
         'Category: applications\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         http_client <- bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals http_client.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Headers\x7fComment: Common subset of headers.\x7fModuleInfo: Module: http_client InitialContents: InitializeToExpression: (nil)'
        
         commonHeaders.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Headers\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         defaultHeaders = ( |
            | ^ffHeaders).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Headers\x7fComment: Firefox headers on Windows XP\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         ffHeaders = ( |
             dict.
            | 
            dict: commonHeaders copy.
            dict at: 'User-Agent' Put: 'Mozilla/5.0 (Windows; U; Windows NT 5.1; cs; rv:1.9.2.3) Gecko/20100401 Firefox/3.6.13'.
            ^dict).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Headers\x7fComment: Headers from Internet Explorer 7.0 on Windows XP\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         ieHeaders = ( |
             dict.
            | 
            dict: commonHeaders copy.
            dict at: 'User-Agent' Put: 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0)'.
            ^dict).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Headers\x7fComment: Firefox headers on Linux\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         lffHeaders = ( |
             dict.
            | 
            dict: commonHeaders copy.
            dict at: 'User-Agent' Put: 'Mozilla/5.0 (X11; U; Linux i686; cs; rv:1.9.2.3) Gecko/20100401 Firefox/3.6.13'.
            ^dict).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: InitializeToExpression: (traits clonable)'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Parsers\x7fCategory: Prototypes\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         parsed_url <- bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( |
             {} = 'Comment: I know that this is not the best possible
implementation and that it also ignores
parts of the RFC, but it is good enough
for simple HTTP client.\x7fModuleInfo: Creator: globals http_client parsed_url.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         = other = ( |
            | 
            (protocol = other protocol) &&
            (domain = other domain) &&
            (port = other port) &&
            (path = other path)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         copy = ( |
            | parent.copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'Category: Data\x7fModuleInfo: Module: http_client InitialContents: InitializeToExpression: (nil)'
        
         domain.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         fromString: url = ( |
             parsed.
             tmp.
             url_copy.
            | 
            parsed: self copy.
            url_copy: url copy.

            "parse protocol"
            (url includesSubstring: '://') ifTrue: [
              tmp: url_copy splitOn: '://'.
              parsed protocol: tmp first.
              url_copy: tmp last.
            ].

            "parse domain"
            url_copy first = '[' "IPv6"
              ifTrue: [
                (url_copy includesSubstring: ']') ifFalse: [error: 'Invalid IPv6 address!'].
                tmp: url_copy splitOn: ']'.
                parsed domain: (tmp first) , ']'.
              ] False: [
                tmp: url_copy splitOn: '/'.
                (tmp size = 1) ifTrue: [tmp: (url_copy splitOn: '?')]. "handle get parameters"
                parsed domain: tmp first.
              ].

            tmp removeFirst.
            url_copy: ((tmp size = 1) ifTrue: [tmp first] False: [tmp asString]).
            parsed path: url_copy.

            "parse port"
            parsed: parsePort: parsed.

            ^parsed).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'Category: Parsers\x7fModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         parsePort: parsed = ( |
             tmp.
            | 

            parsed domain first = '[' "IPv6"
              ifTrue: [
                (parsed domain includesSubstring: ']:') ifTrue: [
                  tmp: parsed domain splitOn: ']:'.
                  parsed domain: tmp first , ']'.
                  parsed port: tmp last asInteger.
                ]
              ] False: [
                (parsed domain includesSubstring: ':') ifTrue: [
                  tmp: parsed domain splitOn: ':'.
                  parsed domain: tmp first.
                  parsed port: tmp last asInteger.
                ].
              ].

            ^parsed).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'Category: Data\x7fModuleInfo: Module: http_client InitialContents: InitializeToExpression: (nil)'
        
         path.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'Category: Data\x7fModuleInfo: Module: http_client InitialContents: InitializeToExpression: (nil)'
        
         port.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'Category: Data\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         portOrDefault = ( |
            | 
            port != nil ifTrue: [^port].

            protocol = 'http' ifTrue: [^80].
            protocol = 'https' ifTrue: [^443].
            protocol = 'ftp' ifTrue: [^21].
            protocol = 'ssh' ifTrue: [^22].

            error: 'Protocol not set, can\'t decide default port!').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'Category: Data\x7fModuleInfo: Module: http_client InitialContents: InitializeToExpression: (nil)'
        
         protocol.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'Category: Unittests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         test = ( |
             tmp.
            | 
            tmp: self copy.
            tmp protocol: 'http'.
            tmp domain: 'kitakitsune.org'.
            tmp port: 22.
            tmp path: ''.

            assert: (fromString: 'http://kitakitsune.org:22/') Equals: tmp.

            tmp port: nil.
            assert: (fromString: 'http://kitakitsune.org/') Equals: tmp.
            assert: (fromString: 'http://kitakitsune.org') Equals: tmp.

            tmp protocol: nil.
            assert: (fromString: 'kitakitsune.org') Equals: tmp.

            tmp domain: '[kitakitsune.org]'.
            assert: (fromString: '[kitakitsune.org]') Equals: tmp.

            tmp: fromString: 'http://kitakitsune.org?asd'.
            assert: tmp domain Equals: 'kitakitsune.org'.

            ^true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'parsed_url' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: InitializeToExpression: (tests suite)'
        
         tests* = bootstrap stub -> 'globals' -> 'tests' -> 'suite' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         request: url Type: type Data: data Body: body Headers: headers = ( |
             socket.
             url_obj.
            | 
            url_obj: parsed_url fromString: url.
            socket: os_file openTCPHost: (url_obj domain) asByteVector Port: (url_obj portOrDefault)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         http_client = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'http_client' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'http_client' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules http_client.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'http_client' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications/http_client'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'http_client' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'http_client' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'http_client' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'http_client' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision:$'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'http_client' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'mixins' -> 'clonable' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | clone).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'mixins' -> 'identity' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         = x = ( |
            | == x).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'mixins' -> 'identity' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | identityHash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'mixins' -> 'unordered' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         descendantResponsibilities = bootstrap setObjectAnnotationOf: bootstrap stub -> 'mixins' -> 'unordered' -> 'descendantResponsibilities' -> () From: ( |
             {} = 'Comment: The following methods must be implemented by a descendant.\x7fModuleInfo: Creator: mixins unordered descendantResponsibilities.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'clonable' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'lobby' -> ().
        } | ) 



 '-- Side effects'

 globals modules http_client postFileIn
