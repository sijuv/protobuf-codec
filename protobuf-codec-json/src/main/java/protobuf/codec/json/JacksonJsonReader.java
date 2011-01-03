package protobuf.codec.json;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Map;

import org.apache.commons.codec.binary.Base64;
import org.codehaus.jackson.JsonParser;
import org.codehaus.jackson.JsonToken;

import protobuf.codec.AbstractCodec;
import protobuf.codec.Codec.Feature;
import protobuf.codec.ParseException;

import com.google.protobuf.ByteString;
import com.google.protobuf.Descriptors.Descriptor;
import com.google.protobuf.Descriptors.FieldDescriptor;
import com.google.protobuf.Descriptors.FieldDescriptor.JavaType;
import com.google.protobuf.ExtensionRegistry;
import com.google.protobuf.ExtensionRegistry.ExtensionInfo;
import com.google.protobuf.Message;
import com.google.protobuf.Message.Builder;

/**
 * Jackson json reader
 * 
 * @author sijuv
 * 
 */
public class JacksonJsonReader {

	public static Message parse(Builder builder, JsonParser parser,ExtensionRegistry extnRegistry,Map<Feature,Object> featureMap)
			throws IOException {
		
		parser.configure(org.codehaus.jackson.JsonParser.Feature.AUTO_CLOSE_SOURCE, (Boolean)featureMap.get(Feature.CLOSE_STREAM));
		parser.nextToken();
		parseObject(builder, parser,extnRegistry,featureMap);
		return builder.build();
	}

	private static Builder parseObject(Builder builder, JsonParser parser,ExtensionRegistry extnRegistry,Map<Feature,Object> featureMap)
			throws IOException {
		
		if (!JsonToken.START_OBJECT.equals(parser.getCurrentToken())){
			throw new ParseException("Parser should point to a START_OBJECT event"); 
		}
		
		Descriptor descriptor = builder.getDescriptorForType();
		while (!parser.nextToken().equals(JsonToken.END_OBJECT)) {
			JsonToken currToken = parser.getCurrentToken();
			assert (currToken.equals(JsonToken.FIELD_NAME));
			String fieldName = parser.getCurrentName();
			FieldDescriptor field=null;
			if(JsonCodec.isExtensionFieldName(fieldName,featureMap)){
				fieldName=JsonCodec.parseExtensionFieldName(fieldName,featureMap);
				ExtensionInfo extnInfo =extnRegistry.findExtensionByName(fieldName);
				
				if(extnInfo==null){
					parser.nextToken(); // Move, we are skipping this field
					if(JsonToken.START_ARRAY.equals(parser.getCurrentToken())
							||JsonToken.START_OBJECT.equals(parser.getCurrentToken())){
						parser.skipChildren();	
					}
					continue;
				}
				field=extnInfo.descriptor;
			}else if(AbstractCodec.isFieldNameUnknownField(fieldName,featureMap)){
				parser.nextToken();
				if(AbstractCodec.supportUnknownFields(featureMap)){
					String unknownFieldsText=parser.getText();
					AbstractCodec.mergeUnknownFieldsFromString(builder, extnRegistry, unknownFieldsText);
				}
				continue;
			}
			else{
				field = descriptor.findFieldByName(fieldName);
			}
			if(field==null){
				throw new ParseException("Field cannot be null, processing fieldName "+fieldName);
			}
			parser.nextToken();
			setFields(builder, field, parser,extnRegistry, featureMap);
		}
		return builder;
	}


	private static Builder setFields(Builder builder, FieldDescriptor field,JsonParser parser
			,ExtensionRegistry extnRegistry,Map<Feature,Object> featureMap) throws IOException {
		Object value = getValue(builder, field, parser,extnRegistry,featureMap);
		if (value == null) {
			// What to do in case of null values ? protobuf does not allow null.
		} else {
			builder.setField(field, value);
		}
		return builder;
	}

	private static void handleArray(Builder builder,FieldDescriptor arrayField, 
			JsonParser parser,ExtensionRegistry extnRegistry,Map<Feature,Object> featureMap) throws IOException {
		
		while (!JsonToken.END_ARRAY.equals(parser.nextToken())) {
			JsonToken token = parser.getCurrentToken();
			if (JsonToken.START_ARRAY.equals(token)) {
				//
			} else {
				Object value = getValue(builder, arrayField, parser,extnRegistry,featureMap);
				builder.addRepeatedField(arrayField, value);
			}
		}
	}

	private static Object getValue(Builder builder, FieldDescriptor field,
			JsonParser parser,ExtensionRegistry extnRegistry,Map<Feature,Object> featureMap) throws IOException {
		JsonToken token = parser.getCurrentToken();
		Object value = null;
		switch (token) {
		case VALUE_STRING:
			if (JavaType.ENUM.equals(field.getJavaType())) {
				value = field.getEnumType().findValueByName(parser.getText());
			} else if (JavaType.STRING.equals(field.getJavaType())) {
				value = parser.getText();
			} else if(JavaType.BYTE_STRING.equals(field.getJavaType())){
				value=ByteString.copyFrom(Base64.decodeBase64(parser.getText()));
			}else {
				throw new UnsupportedEncodingException(
						String.format(
								"Unsupported java type [%s] for field [%s] for json type VALUE_STRING",
								field.getJavaType(), field.getName()));
			}
			break;
		case VALUE_TRUE:
			value = Boolean.TRUE;
			break;
		case VALUE_FALSE:
			value = Boolean.FALSE;
			break;
		case VALUE_NUMBER_INT:
			if (field.getJavaType().equals(JavaType.INT)) {
				value = parser.getIntValue();
			} else if (JavaType.LONG.equals(field.getJavaType())) {
				value = parser.getLongValue();
			} else {
				throw new UnsupportedEncodingException(
						String.format(
								"Unsupported java type [%s] for field [%s] for json type VALUE_NUMBER_INT",
								field.getJavaType(), field.getName()));
			}
			break;
		case VALUE_NUMBER_FLOAT:
			if (JavaType.DOUBLE.equals(field.getJavaType())) {
				value = parser.getDoubleValue();
			} else if (JavaType.FLOAT.equals(field.getJavaType())) {
				value = parser.getFloatValue();
			} else {
				throw new UnsupportedEncodingException(
						String.format(
								"Unsupported java type [%s] for field [%s] for json type VALUE_NUMBER_FLOAT",
								field.getJavaType(), field.getName()));
			}
			break;
		case START_OBJECT:
			Builder newBuilder=null;
			if(field.isExtension()){
				newBuilder = extnRegistry.findExtensionByName(field.getFullName()).defaultInstance.toBuilder();
			}else{
				newBuilder = builder.newBuilderForField(field);
			}
			value = parseObject(newBuilder, parser, extnRegistry,featureMap).build();
			break;
		case START_ARRAY:
			handleArray(builder, field, parser,extnRegistry,featureMap);
			break;

		case VALUE_NULL:
			// protobuf does not support null, returning null here however.
			break;
		default:
			throw new UnsupportedEncodingException(String.format(
					"Unsupported token type [%s]", token));
		}
		return value;
	}
}