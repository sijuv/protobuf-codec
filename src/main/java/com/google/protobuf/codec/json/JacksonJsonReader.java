package com.google.protobuf.codec.json;
import java.io.IOException;
import java.io.UnsupportedEncodingException;

import org.codehaus.jackson.JsonParser;
import org.codehaus.jackson.JsonToken;

import com.google.protobuf.ExtensionRegistry;
import com.google.protobuf.ExtensionRegistry.ExtensionInfo;
import com.google.protobuf.GeneratedMessage.ExtendableBuilder;
import com.google.protobuf.Message;
import com.google.protobuf.Descriptors.Descriptor;
import com.google.protobuf.Descriptors.FieldDescriptor;
import com.google.protobuf.Descriptors.FieldDescriptor.JavaType;
import com.google.protobuf.Message.Builder;

/**
 * Jackson json reader
 * 
 * @author sijuv
 * 
 */
public class JacksonJsonReader {

	public static Message parse(Builder builder, JsonParser parser,ExtensionRegistry extnRegistry)
			throws IOException {
		parser.nextToken();
		parseObject(builder, parser,extnRegistry);
		return builder.build();
	}

	private static Builder parseObject(Builder builder, JsonParser parser,ExtensionRegistry extnRegistry)
			throws IOException {
		assert (JsonToken.START_OBJECT.equals(parser.getCurrentToken())); 
		Descriptor descriptor = builder.getDescriptorForType();
		while (!parser.nextToken().equals(JsonToken.END_OBJECT)) {
			JsonToken currToken = parser.getCurrentToken();
			assert (currToken.equals(JsonToken.FIELD_NAME));
			String fieldName = parser.getCurrentName();
			FieldDescriptor field=null;
			if(JsonCodec.isExtensionFieldName(fieldName)){
				fieldName=JsonCodec.parseExtensionFieldName(fieldName);
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
			}else{
				field = descriptor.findFieldByName(fieldName);
			}
			assert(field!=null);
			parser.nextToken();
			setFields(builder, field, parser,extnRegistry);
		}
		return builder;
	}


	private static Builder setFields(Builder builder, FieldDescriptor field,JsonParser parser,ExtensionRegistry extnRegistry) throws IOException {
		Object value = getValue(builder, field, parser,extnRegistry);
		if (value == null) {
			// What to do in case of null values ? protobuf does not allow null.
		} else {
			builder.setField(field, value);
		}
		return builder;
	}

	private static void handleArray(Builder builder,
			FieldDescriptor arrayField, JsonParser parser,ExtensionRegistry extnRegistry) throws IOException {
		while (!JsonToken.END_ARRAY.equals(parser.nextToken())) {
			JsonToken token = parser.getCurrentToken();
			if (JsonToken.START_ARRAY.equals(token)) {
				//
			} else {
				Object value = getValue(builder, arrayField, parser,extnRegistry);
				builder.addRepeatedField(arrayField, value);
			}
		}
	}

	private static Object getValue(Builder builder, FieldDescriptor field,
			JsonParser parser,ExtensionRegistry extnRegistry) throws IOException {
		JsonToken token = parser.getCurrentToken();
		Object value = null;
		switch (token) {
		case VALUE_STRING:
			if (JavaType.ENUM.equals(field.getJavaType())) {
				value = field.getEnumType().findValueByName(parser.getText());
			} else if (JavaType.STRING.equals(field.getJavaType())) {
				value = parser.getText();
			} else {
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
			Builder newBuilder = builder.newBuilderForField(field);
			value = parseObject(newBuilder, parser, extnRegistry).build();
			break;
		case START_ARRAY:
			handleArray(builder, field, parser,extnRegistry);
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