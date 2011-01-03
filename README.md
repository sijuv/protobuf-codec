# Protobuf Codec
Protobuf-codec provides codec for protobuf and other data formats like xml/json. It is fast and relies on proven frameworks like [jackson](http://jackson.codehaus.org/)
 for json and [woodstox]( http://woodstox.codehaus.org/) for xml for parsing. 

[Project Wiki](https://github.com/sijuv/protobuf-codec/wiki)

## UPDATES:

03/Jan/11 : Disted version 1.1
     - Base64 encode unknownfields instead of hex
     - Added support for byte protobuf type, ByteString is written out as a base64 encoded
       string.
       
08/Dec : Disted version 1.0
     - Added support for xml
     - Added support for unknown fields
     - Split into a maven multi module project- core,xml and json
     - Removed com.google from package declaration.
     
06/Dec : 
	- Mavenized and disted verion 0.0.1





 
## BUILDING
 - Needs Maven
 - Needs [protoc](http://code.google.com/apis/protocolbuffers/docs/proto.html#generating) to run test cases.


## DEPENDENCIES :
- Jackson for json parsing. 
- woodstox for xml parsing 
- commons-codec for base64 encode/decode


## BUGS & ISSUES
Raise bugs/issues on [git] (https://github.com/sijuv/protobuf-codec/issues)




