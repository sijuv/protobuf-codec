package protobuf.codec.xml;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.XMLStreamWriter;

import protobuf.codec.AbstractCodec;
import protobuf.codec.ParseException;
import protobuf.codec.Codec.Feature;

import com.google.protobuf.ExtensionRegistry;
import com.google.protobuf.Message;
import com.google.protobuf.UnknownFieldSet;
import com.google.protobuf.Message.Builder;

/**
 * Xml-protobuf serializer/deserializer.Relies on stax(woodstox) as the underlying parser.
 * Supports {@link UnknownFieldSet}, extension field names are written as xml elements with name being
 * "extension" an attribute "type" containting the field name and the value as the element value.
 * In case the {@link Feature#UNKNOWN_FIELDS} is enabled ( this feature is enabled by default) each xml  element in 
 * could contain a field by name "unknownfields" or whatever value the {@value Feature#UNKNOWN_FIELD_ELEM_NAME} is
 * set to. The value of this field is hex encoded byte string.
 * @author sijuv
 * 
 */
//TODO Need to make namespace aware
public class XmlCodec extends AbstractCodec {

    private static XMLOutputFactory XML_OUTPUT_FACTORY = XMLOutputFactory.newInstance();
    private static XMLInputFactory XML_INPUT_FACTORY = XMLInputFactory.newInstance();

    static {
        XML_INPUT_FACTORY.setProperty("javax.xml.stream.isNamespaceAware", Boolean.FALSE);
    }

    /**
     * Reads the xml stream into a {@link Message} object. The root element of the xml should as per {@link #getRootElementName(Message)}.
     * Extension fields are identified by element name {@link #EXTENSION_ELEM_NAME} with the attribute {@link #EXTENTION_TYPE_ATTRIB_NAME} identifying
     * the type of extension. The value of this element is the field value. 
     * @param builder
     * @param reader
     * @param extnRegistry
     * @return
     * @throws IOException
     */
    @Override
    protected Message readFromStream(Builder builder, Reader reader, ExtensionRegistry extnRegistry) throws IOException {
        try {
            XMLStreamReader xmlreader = XML_INPUT_FACTORY.createXMLStreamReader(reader);
            return XmlReader.parse(builder, xmlreader, extnRegistry, getAllFeaturesSet());
        } catch (XMLStreamException e) {
            throw new ParseException(e);
        }
    }

    @Override
    protected void writeToStream(Message message, Writer writer)
            throws IOException {
        try {
            XMLStreamWriter xmlwriter = XML_OUTPUT_FACTORY.createXMLStreamWriter(writer);
            xmlwriter.writeStartDocument();
            xmlwriter.writeStartElement(getRootElementName(message));
            XmlWriter.writeXml(message, xmlwriter, getAllFeaturesSet());
            xmlwriter.writeEndElement();
            xmlwriter.writeEndDocument();

        } catch (XMLStreamException e) {
            throw new ParseException(e);
        }
    }

    @Override
    protected void validateAndSetFeature(Feature feature, Object value) {
        switch (feature) {
            case SUPPORT_UNKNOWN_FIELDS:
            case CLOSE_STREAM:
                if (!(Boolean.TRUE.equals(value) || Boolean.FALSE.equals(value))) {
                    throw new IllegalArgumentException(String.format("Unsupported value [%s] for feature [%s]", value, feature));
                }
                break;
            case UNKNOWN_FIELD_ELEM_NAME:
                if (value == null || !(String.class).isAssignableFrom(value.getClass())
                        || ((String) value).trim().equals("")) {
                    throw new IllegalArgumentException(String.format("Feature [%s] expected to be a non null string", feature));
                }
                break;
            case FIELD_NAME_READ_SUBSTITUTES:
            case FIELD_NAME_WRITE_SUBSTITUTES:
            case STRIP_FIELD_NAME_UNDERSCORES:
            //noop Already handled
                break;
            default:
                throw new IllegalArgumentException(String.format("Unsupported feature [%s]", feature));
        }
    }

    /**
     * Returns the element name to be used for the root element.
     * @param message
     * @return message.getClass().getSimpleName().toLowerCase()
     */
    protected static final String getRootElementName(Message message) {
        return message.getClass().getSimpleName().toLowerCase();
    }

    /**
     * @param builder
     * @return
     * @see XmlCodec#getRootElementName(Message)
     */
    protected static final String getRootElementName(Builder builder) {
        return getRootElementName(builder.getDefaultInstanceForType());
    }
}
