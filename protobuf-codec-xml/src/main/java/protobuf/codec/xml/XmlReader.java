package protobuf.codec.xml;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Map;

import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import org.apache.commons.codec.binary.Base64;

import protobuf.codec.AbstractCodec;
import protobuf.codec.Codec.Feature;
import protobuf.codec.ParseException;

import com.google.protobuf.Descriptors.FieldDescriptor;
import com.google.protobuf.ByteString;
import com.google.protobuf.ExtensionRegistry;
import com.google.protobuf.ExtensionRegistry.ExtensionInfo;
import com.google.protobuf.Message;
import com.google.protobuf.Message.Builder;

/**
 * XML Reader
 * @author sijuv
 *
 */
public class XmlReader {

    public static Message parse(Builder builder, XMLStreamReader xmlreader,
            ExtensionRegistry extnRegistry, Map<Feature, Object> featureMap) throws IOException, XMLStreamException {

        xmlreader.nextTag(); // Start document.
        String rootElemName = xmlreader.getLocalName();
        if (!(rootElemName.equals(XmlCodec.getRootElementName(builder)))) {
            throw new IllegalArgumentException("The root element name does not match the required pattern");
        }

        while (xmlreader.getEventType() != XMLStreamConstants.END_DOCUMENT) {
            parseElement(builder, xmlreader, extnRegistry, featureMap);
            xmlreader.next();
        }
        return builder.build();
    }

    /** Parse the element */
    private static Builder parseElement(Builder builder, XMLStreamReader xmlreader,
            ExtensionRegistry extnRegistry, Map<Feature, Object> featureMap) throws XMLStreamException, IOException {

        int event = -1;
        while ((event = xmlreader.next()) != XMLStreamConstants.END_ELEMENT) {
            switch (event) {
                case XMLStreamConstants.START_ELEMENT:
                    String fieldName = xmlreader.getLocalName();
                    fieldName = AbstractCodec.stripFieldName(fieldName, featureMap);
                    fieldName = AbstractCodec.substituteFieldNameForReading(fieldName, featureMap);
                    FieldDescriptor field = null;
                    if (AbstractCodec.isExtensionFieldName(fieldName, featureMap)) {
                        String extnName = AbstractCodec.parseExtensionFieldName(fieldName, featureMap);
                        ExtensionInfo extnInfo = extnRegistry.findExtensionByName(extnName);
                        if (extnInfo == null) {
                            //Skip the extension element
                            while (!xmlreader.isEndElement()
                                    || !(xmlreader.isEndElement() && AbstractCodec.isExtensionFieldName(xmlreader.getLocalName(), featureMap))) {
                                xmlreader.next();
                            }
                            continue;
                        }
                        field = extnInfo.descriptor;
                    } else if (AbstractCodec.isFieldNameUnknownField(fieldName, featureMap)) {
                        xmlreader.next();
                        String unknownFieldsText = xmlreader.getText();
                        if (AbstractCodec.supportUnknownFields(featureMap)) {
                            AbstractCodec.mergeUnknownFieldsFromString(builder, extnRegistry, unknownFieldsText);
                        }
                        xmlreader.next();
                        continue;
                    } else {
                        field = builder.getDescriptorForType().findFieldByName(fieldName);
                    }
                    if (field == null) {
                        throw new ParseException("Field cannot be null, processing fieldName " + fieldName);
                    }
                    setFields(builder, field, xmlreader, extnRegistry, featureMap);
                    break;
            }
        }
        return builder;
    }

    private static Builder setFields(Builder builder, FieldDescriptor field, XMLStreamReader xmlreader,
            ExtensionRegistry extnRegistry, Map<Feature, Object> featureMap) throws IOException, XMLStreamException {

        Object value = getValue(builder, field, xmlreader, extnRegistry, featureMap);
        if (value == null) {
            // What to do in case of null values ? protobuf does not allow null.
        } else {
            if (field.isRepeated()) {
                builder.addRepeatedField(field, value);
            } else {
                try {
                    builder.setField(field, value);
                } catch (IllegalArgumentException e) {
                    throw new ParseException(String.format("Error while setting value [%s] on field [%s] ",
                            value, field.getFullName()), e);
                }

            }
        }
        return builder;
    }

    private static Object getValue(Builder builder, FieldDescriptor field, XMLStreamReader xmlreader,
            ExtensionRegistry extnRegistry, Map<Feature, Object> featureMap) throws XMLStreamException, IOException {

        Object value = null;
        String fieldValue = null;
        switch (field.getJavaType()) {
            case INT:
                xmlreader.next();// Move past
                fieldValue = xmlreader.getText();
                value = Integer.parseInt(fieldValue);
                xmlreader.next();
                break;
            case BOOLEAN:
                xmlreader.next();// Move past
                fieldValue = xmlreader.getText();
                value = Boolean.valueOf(fieldValue);
                xmlreader.next();
                break;
            case DOUBLE:
                xmlreader.next();// Move past
                fieldValue = xmlreader.getText();
                value = Double.valueOf(fieldValue);
                xmlreader.next();
                break;
            case FLOAT:
                xmlreader.next();// Move past
                fieldValue = xmlreader.getText();
                value = Float.valueOf(fieldValue);
                xmlreader.next();
                break;
            case LONG:
                xmlreader.next();// Move past
                fieldValue = xmlreader.getText();
                value = Long.valueOf(fieldValue);
                xmlreader.next();

                break;
            case STRING:
                xmlreader.next();// Move past
                fieldValue = xmlreader.getText();
                value = fieldValue;
                xmlreader.next();
                break;
            case ENUM:
                xmlreader.next();// Move past
                fieldValue = xmlreader.getText();
                value = field.getEnumType().findValueByName(fieldValue);
                xmlreader.next();
                break;
            case MESSAGE:
                Builder newBuilder = null;
                if (field.isExtension()) {
                    newBuilder = extnRegistry.findExtensionByName(field.getFullName()).defaultInstance.toBuilder();
                } else {
                    newBuilder = builder.newBuilderForField(field);
                }
                value = parseElement(newBuilder, xmlreader, extnRegistry, featureMap).build();
                break;
            case BYTE_STRING:
                xmlreader.next();// Move past
                value = ByteString.copyFrom(Base64.decodeBase64(xmlreader.getText()));
                xmlreader.next();
                break;
            default:
                throw new UnsupportedEncodingException(String.format(
                        "Unsupported java  type [%s]", field.getJavaType()));
        }
        return value;
    }
}
