package protobuf.codec.json;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.codec.binary.Base64;
import org.codehaus.jackson.JsonGenerator;

import protobuf.codec.AbstractCodec;
import protobuf.codec.Codec.Feature;

import com.google.protobuf.ByteString;
import com.google.protobuf.Descriptors.EnumValueDescriptor;
import com.google.protobuf.Descriptors.FieldDescriptor;
import com.google.protobuf.Message;
import com.google.protobuf.UnknownFieldSet;

/**
 * Jackson json writer
 * 
 * @author sijuv
 * 
 */
public class JacksonJsonWriter {

	public static void generateJSONFields(Message message,JsonGenerator generator,Map<Feature,Object> featureMap) throws IOException {
		
		generator.configure(org.codehaus.jackson.JsonGenerator.Feature.AUTO_CLOSE_TARGET, (Boolean)featureMap.get(Feature.CLOSE_STREAM));
		
		if(AbstractCodec.prettyPrint(featureMap)){
			generator.useDefaultPrettyPrinter(); 
		}
		if(!AbstractCodec.closeStream(featureMap)){
			generator.configure(org.codehaus.jackson.JsonGenerator.Feature.AUTO_CLOSE_TARGET, false);
		}
		
		generator.writeStartObject();
		Iterator<Map.Entry<FieldDescriptor, Object>> iterator = message.getAllFields().entrySet().iterator(); // Get all set fields
		while (iterator.hasNext()) {
			Map.Entry<FieldDescriptor, Object> record = iterator.next();
			FieldDescriptor field = record.getKey();
			String fieldName = field.isExtension() ?JsonCodec.getExtensionFieldName(field.getName(),featureMap): field.getName(); // If extn field? box
			Object value = record.getValue();
			if (field.isRepeated()) {
				generator.writeArrayFieldStart(fieldName);
				Iterator<?> iter = ((List<?>) value).iterator();
				while (iter.hasNext()) {
					writeFieldValue(field, iter.next(), generator,featureMap);
				}
				generator.writeEndArray();
			} else {
				generator.writeFieldName(fieldName);
				writeFieldValue(field, value, generator,featureMap);
			}
		}
		if(AbstractCodec.supportUnknownFields(featureMap)){
			writeUnknownFieldSet(message.getUnknownFields(),generator,featureMap);
		}
		generator.writeEndObject();
	}

	// Extract the field value depending on its java type
	private static void writeFieldValue(FieldDescriptor fieldDesc,Object value, JsonGenerator generator,Map<Feature, Object> featureMap) throws IOException {
		
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
			generator.writeString(Base64.encodeBase64String(((ByteString)value).toByteArray()));
			break;
		case MESSAGE:
			generateJSONFields((Message) value, generator,featureMap);
			break;
		default:
			throw new UnsupportedEncodingException(
					String.format(
							"Unspupported protobuf java field type [%s] for field [%s] ",
							fieldDesc.getJavaType(), fieldDesc.getName()));

		}
	}
	
	private static void writeUnknownFieldSet(UnknownFieldSet unknownFields,JsonGenerator generator,Map<Feature,Object> featureMap) throws IOException{
		if(unknownFields!=null&& unknownFields.asMap().size()>0){
			generator.writeStringField(AbstractCodec.getUnknownFieldElementName(featureMap),
					AbstractCodec.encodeUnknownFieldsToString(unknownFields));
		}
	}
	
}