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
         'Category: Headers\x7fComment: Common subset of headers.\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         commonHeaders <- bootstrap setObjectAnnotationOf: ( [|d|
	d: dictionary copyRemoveAll.
	d at: ('Accept') Put: ('text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain').
	d at: ('Accept-Charset') Put: ('utf-8').
	d at: ('Accept-Language') Put: ('cs,en-us;q=0.7,en;q=0.3').
	d at: ('Connection') Put: ('keep-alive').
	d at: ('Keep-Alive') Put: ('300').
	d at: ('User-Agent') Put: ('Self HTTP client https://github.com/Bystroushaak/http_client').
] value) From: ( |
             {} = 'ModuleInfo: Creator: globals http_client commonHeaders.

CopyDowns:
globals set. copy 
SlotsToOmit: parent prototype.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Constants\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         crlf = '\x0d
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Headers\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         defaultHeaders = ( |
            | 
            ^commonHeaders copy).
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
         'Category: Requests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         getRequest: url Headers: headers Parameters: params = ( |
             response.
             socket.
             url_obj.
            | 
            url_obj: parsed_url fromString: url.
            socket: self openConnection: url_obj.

            socket write: 'GET ', url_obj path, ' ', httpVersion, crlf.
            socket write: 'Host: ', url_obj domain, crlf.
            sendHeaders: socket.
            socket write: crlf.

            response: parseHeaders: socket.

            ^response).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         headRequest: url Headers: headers = ( |
             response.
             socket.
             url_obj.
            | 
            url_obj: parsed_url fromString: url.
            socket: self openConnection: url_obj.

            socket write: 'HEAD ', url_obj path, ' ', httpVersion, crlf.
            socket write: 'Host: ', url_obj domain, crlf.
            sendHeaders: socket.
            socket write: crlf.

            response: parseHeaders: socket.
            socket close.

            ^response).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Constants\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         httpVersion = 'HTTP/1.1'.
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
         'Category: Requests\x7fCategory: Internals\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         openConnection: url_obj = ( |
            | 
            ^ os_file openTCPHost: (url_obj domain) Port: (url_obj portOrDefault)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: InitializeToExpression: (traits clonable)'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fCategory: Internals\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         parseHeader: line Do: blk = ( |
            | 
            line
                findSubstring: ':'
                IfPresent: [| :index |
                  blk value: (line copyFrom: 0 UpTo: index)
                      With: ((line copyFrom: (index + 1) UpTo: (line size)) shrinkwrapped).
                  ^true.
                ]
                IfAbsent: [^false]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fCategory: Internals\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         parseHeaders: socket = ( |
             buffer.
             response_obj.
             should_continue.
            | 

            response_obj: self response clone.

            buffer: socket readLine splitOn: ' '.
            response_obj httpVersion: (buffer first).
            response_obj statusCode: ((buffer at: 1) asInteger).

            [
              buffer: socket readLine.
              should_continue: self parseHeader: buffer
                                    Do: [| :k. :v | response_obj headers at: k Put: v ].
            ] untilFalse: [ should_continue ].

            ^response_obj).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fCategory: Internals\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         parseResponse: socket Response: response = ( |
             content_length.
             transfer_encoding.
            | 
            status_code: response statusCode.

            "Special codes with no data - defined in RFC 2616, section 4.4
            (http://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4.4)"
            ((status_code >= 100) && (status_code < 200)) ||
            (status_code = 204) ||
            (status_code = 304)
              ifTrue: [^response].


            "Chunked requests"
            transfer_encoding: response headers at: 'Transfer-Encoding' IfAbsent: nil.
            (transfer_encoding uncapitalizeAll) = 'chunked' ifTrue: [


              ^response.
            ].

            "Requests specified by ContentLength"
            content_length: response headers at: 'Content-Length' IfAbsent: nil.
            content_length != nil ifTrue: [
              content_length asInteger.

              ^response.
            ].

            [

            ] untilFalse: [socket isLive].

            ^response).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Prototypes\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
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

            parsed path = '' ifTrue: [parsed path: '/'].

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
            tmp path: '/'.

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
         'Category: Prototypes\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         response = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'http_client' -> 'response' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals http_client response.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: InitializeToExpression: (nil)'
        
         body.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: InitializeToExpression: (dictionary copy.)'
        
         headers <- dictionary copy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: InitializeToExpression: (nil)\x7fVisibility: public'
        
         httpVersion.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: InitializeToExpression: (nil)'
        
         statusCode.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Requests\x7fCategory: Internals\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         sendHeaders: socket = ( |
            | 
            commonHeaders copy do: [ |:v. :k. |
              socket write: (k, ': ', v, crlf).
            ]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Unittests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         test = ( |
            | 
            testUrlDecodeEncode.

            ^ true).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'Category: Unittests\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         testUrlDecodeEncode = ( |
             decoded = 'test:$#@=?%^Q^$'.
             encoded = 'test%3a%24%23%40%3d%3f%25%5eQ%5e%24'.
            | 

            assert: decoded urlEncode Equals: encoded.
            assert: encoded urlDecode Equals: decoded).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'http_client' -> () From: ( | {
         'ModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         tests* = bootstrap stub -> 'globals' -> 'tests' -> 'suite' -> ().
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

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'string' -> () From: ( | {
         'Category: transforming\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         urlDecode = ( |
             out.
             tokens.
            | 
            tokens: self splitOn: '%'.

            tokens size == 0 ifTrue: [^''].
            tokens size == 1 ifTrue: [^tokens first].

            out: tokens removeFirst.
            tokens do: [| :token |
              token size < 2
                ifTrue: [out: (out, token)]
                False: [| hex_pair. |
                  hex_pair: (token at: 0), (token at: 1).
                  out: out,
                       (hex_pair hexAsInteger asCharacter),
                       (token copyFrom: 2 UpTo: (token size)).
                ].
            ].

            ^out).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'string' -> () From: ( | {
         'Category: transforming\x7fModuleInfo: Module: http_client InitialContents: FollowSlot'
        
         urlEncode = ( |
             alpha.
             c.
             digit.
             out.
             reserved.
             unreserved.
             whitelist.
            | 
            out: mutableString clone.

            "Datasets specified by RFC 3986"
            reserved: '!()*' asSet. "It should be ':/?#[]@!$&\'()*+,;=' ?"
            alpha: 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ' asSet.
            digit: '0123456789' asSet.
            unreserved: ('-._~' asSet), alpha, digit.
            whitelist: reserved, unreserved.

            bytesDo: [| :i. high. low. |
              c: i asCharacter.
              (whitelist includes: c)
                ifTrue: [out: out, c]
                False: [out: out, '%', (i hexPrintString)].
            ].

            ^out).
        } | ) 



 '-- Side effects'

 globals modules http_client postFileIn
