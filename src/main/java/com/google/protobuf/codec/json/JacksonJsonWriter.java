package com.google.protobuf.codec.json;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.codehaus.jackson.JsonGenerator;

import com.google.protobuf.Message;
import com.google.protobuf.Descriptors.EnumValueDescriptor;
import com.google.protobuf.Descriptors.FieldDescriptor;

/**
 * Jackson json writer
 * 
 * @author sijuv
 * 
 */
public class JacksonJsonWriter {

	public static void generateJSONFields(Message message,
			JsonGenerator generator) throws IOException {

		generator.writeStartObject();
		Iterator<Map.Entry<FieldDescriptor, Object>> iterator = message.getAllFields().entrySet().iterator(); // Get all set fields
		while (iterator.hasNext()) {
			Map.Entry<FieldDescriptor, Object> record = iterator.next();
			FieldDescriptor field = record.getKey();
			String fieldName = field.isExtension() ?JsonCodec.getExtensionFieldName(field.getName()): field.getName(); // If extn field? box
			Object value = record.getValue();
			if (field.isRepeated()) {
				generator.writeArrayFieldStart(fieldName);
				Iterator<?> iter = ((List<?>) value).iterator();
				while (iter.hasNext()) {
					writeFieldValue(field, iter.next(), generator);
				}
				generator.writeEndArray();
			} else {
				generator.writeFieldName(fieldName);
				writeFieldValue(field, value, generator);
			}
		}
		generator.writeEndObject();
	}

	// Extract the field value depending on its java type
	private static void writeFieldValue(FieldDescriptor fieldDesc,Object value, JsonGenerator generator) throws IOException {
		
		switch (fieldDesc.getJavaType()) {
		case INT:
			generator.writeNumber((Integer) value);
			break;
		case LONG:
			generator.writeNumber((Long) value);
			break;
		case FLOAT:
			generator.writeNumber((Float) value);
			break;
		case DOUBLE:
			generator.writeNumber((Double) value);
			break;
		case BOOLEAN:
			generator.writeBoolean((Boolean) value);
			break;
		case STRING:
			generator.writeString((String) value);
			break;
		case ENUM:
			generator.writeString(((EnumValueDescriptor) value).getName());
			break;
		case BYTE_STRING:
			break;// What to do here? can be used for unknown fields?//TODO
					// UnknownFields?
		case MESSAGE:
			generateJSONFields((Message) value, generator);
			break;
		default:
			throw new UnsupportedEncodingException(
					String.format(
							"Unspupported protobuf java field type [%s] for field [%s] ",
							fieldDesc.getJavaType(), fieldDesc.getName()));

		}
	}
}