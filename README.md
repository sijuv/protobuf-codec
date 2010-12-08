# Protobuf Codec
Protobuf-codec provides codec for protobuf and other data formats like xml/json. It is fast and relies on proven frameworks like [jackson](http://jackson.codehaus.org/)
 for json and [woodstox]( http://woodstox.codehaus.org/) for xml for parsing. 

[Project Wiki](https://github.com/sijuv/protobuf-codec/wiki)

## UPDATES:
06/Dec : 
	- Mavenized and disted verion 0.0.1

08/Dec : Disted version 1.0
     - Added support for xml
     - Added support for unknown fields
     - Split into a maven multi module project- core,xml and json
     - Removed com.google from package declaration.



 
## BUILDING
 - Needs Maven
 - Needs [protoc](http://code.google.com/apis/protocolbuffers/docs/proto.html#generating) to run test cases.


## DEPENDENCIES :
- Jackson for json parsing. 
- woodstox for xml parsing 
- commons-codec for hex encode/decode


## BUGS & ISSUES
Raise bugs/issues on [git] (https://github.com/sijuv/protobuf-codec/issues)




