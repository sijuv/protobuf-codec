package protobuf.codec.xml;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import protobuf.codec.AbstractCodec;
import protobuf.codec.Codec.Feature;

import com.google.protobuf.Descriptors.EnumValueDescriptor;
import com.google.protobuf.Descriptors.FieldDescriptor;
import com.google.protobuf.Message;
import com.google.protobuf.UnknownFieldSet;

/**
 * 
 * @author sijuv
 *
 */
public class XmlWriter {
	

	public static void writeXml(Message message, XMLStreamWriter xmlwriter,Map<Feature,Object> featureMap) throws XMLStreamException, IOException {
		Iterator<Map.Entry<FieldDescriptor, Object>> iterator = message.getAllFields().entrySet().iterator(); // Get all set fields
		while (iterator.hasNext()) {
			Map.Entry<FieldDescriptor, Object> record = iterator.next();
			FieldDescriptor field = record.getKey();
			String fieldName=field.isExtension()?AbstractCodec.getExtensionFieldName(field.getName(), featureMap):field.getName();
			Object value = record.getValue();
			if (field.isRepeated()) {
				Iterator<?> iter = ((List<?>) value).iterator();
				while (iter.hasNext()) {
					xmlwriter.writeStartElement(fieldName);
					writeFieldValue(field, iter.next(), xmlwriter,featureMap);
					xmlwriter.writeEndElement();
				}
			} else {
				xmlwriter.writeStartElement(fieldName);
				writeFieldValue(field, value, xmlwriter,featureMap);
				xmlwriter.writeEndElement();
			}
		}
		if(AbstractCodec.supportUnknownFields(featureMap)){
			writeUnknownFields(message.getUnknownFields(), xmlwriter,featureMap);
		}

	}
	
	

	
	// Extract the field value depending on its java type
	private static void writeFieldValue(FieldDescriptor fieldDesc,Object value,
			XMLStreamWriter writer,Map<Feature,Object> featureMap) throws XMLStreamException,IOException {

		switch (fieldDesc.getJavaType()) {
		case INT:
		case LONG:
		case FLOAT:
		case DOUBLE:
		case BOOLEAN:
		case STRING:
			writer.writeCharacters(value.toString());
			break;
		case ENUM:
			writer.writeCharacters(((EnumValueDescriptor) value).getName());
			break;
		case BYTE_STRING:
			break;// What to do here? can be used for unknown fields?//TODO
					// UnknownFields?
		case MESSAGE:
			writeXml((Message) value, writer,featureMap);
			break;
		default:
			throw new UnsupportedEncodingException(
					String.format(
							"Unspupported protobuf java field type [%s] for field [%s] ",
							fieldDesc.getJavaType(), fieldDesc.getName()));

		}
	}
	
	public static void writeUnknownFields(UnknownFieldSet unknownFields,XMLStreamWriter xmlwriter,Map<Feature,Object> featureMap) throws XMLStreamException{
		if(unknownFields!=null&& unknownFields.asMap().size()>0){
			xmlwriter.writeStartElement(AbstractCodec.getUnknownFieldElementName(featureMap));
			xmlwriter.writeCharacters(AbstractCodec.encodeUnknownFieldsToHexString(unknownFields));
			xmlwriter.writeEndElement();
		}
	}
}
