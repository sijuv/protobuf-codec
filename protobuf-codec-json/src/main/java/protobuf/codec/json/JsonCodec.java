package protobuf.codec.json;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.Writer;

import org.codehaus.jackson.JsonFactory;
import org.codehaus.jackson.JsonGenerator;
import org.codehaus.jackson.JsonParser;

import protobuf.codec.AbstractCodec;
import protobuf.codec.Codec;

import com.google.protobuf.ExtensionRegistry;
import com.google.protobuf.Message;
import com.google.protobuf.Message.Builder;
import com.google.protobuf.UnknownFieldSet;

/**
 * Json-protobuf serializer/deserializer.Relies on jackson as the underlying parser.
 * Supports {@link UnknownFieldSet}, extension field names prefixed with {@link Feature#EXTENSION_FIELD_NAME_PREFIX} and a "-"/
 * ex: "extension-name" idenfies an extension file by type name
 * In case the {@link Feature#UNKNOWN_FIELDS} is enabled ( this feature is enabled by default) each json object in 
 * could contain a field by name "unknownfields" or whatever value the {@value Feature#UNKNOWN_FIELD_ELEM_NAME} is
 * set to. The value of this field is hex encoded byte string.
 *
 * @author sijuv
 * 
 */
public class JsonCodec extends AbstractCodec {
	
	
	public JsonCodec(){
		setFeature(Feature.CLOSE_STREAM, true);
	}


	/**
	 * Fills in a message from the data from the stream, The stream is not closed once the message is read in, the extn field names 
	 * need to be boxed. {@link UnknownFieldSet}  passed is passed in the field defined by {@value Feature#UNKNOWN_FIELD_ELEM_NAME}. 
	 * The {@link UnknownFieldSet} provided is parsed in only if the {@link Feature#SUPPORT_UNKNOWN_FIELDS} is enabled. 
	 * The value passed in as unknown field should be corresponding hex encoded byte[]
	 * The {@link Feature#EXTENSION_FIELD_NAME_PREFIX} controlls how the extension field names need to be.
	 * @param builder the message builder
	 * @param reader the input stream,
	 * @param extnRegistry the extension registry to use
	 * @see Codec#toMessage(Class, Reader)
	 * @see AbstractCodec#mergeUnknownFieldsFromHexString(Builder, ExtensionRegistry, String)
	 */
	@Override
	protected Message  readFromStream(Builder builder, Reader reader,ExtensionRegistry extnRegistry)throws IOException {
		JsonFactory jsonFactory=new JsonFactory();
		JsonParser parser=jsonFactory.createJsonParser(reader);
		return JacksonJsonReader.parse(builder, parser,extnRegistry,getAllFeaturesSet());
	}
	
	/**
	 * Writes out the messages as a json object the provided stream.
	 * The stream is not closed once the message is written out. {@link UnknownFieldSet} is serialized out as a hex byte string.
	 * @param message the provided protobuf message
	 * @param os the output stream onto which the message is written to.
	 * @see Codec#toMessage(InputStream)
	 * Sample response:<br>
	 * {"id":1,"name":"elton john","extension-bar":1,"extension-id":24,"extension-place":"london"}
	 * bar and place are extension fields.
	 * @see AbstractCodec#encodeUnknownFieldsToHexString(UnknownFieldSet)
	 * 
	 */
	@Override
	protected void writeToStream(Message message, Writer writer)throws IOException {
		JsonFactory jsonFactory = new JsonFactory();
		JsonGenerator generator = jsonFactory.createJsonGenerator(writer);
		JacksonJsonWriter.generateJSONFields(message, generator,getAllFeaturesSet());
		generator.close();
	}
	
	
	@Override
	public void validateAndSetFeature(Feature feature, Object value) {
		switch(feature){
		case SUPPORT_UNKNOWN_FIELDS:
		case PRETTY_PRINT:
		case CLOSE_STREAM:
			if(!(Boolean.TRUE.equals(value)||Boolean.FALSE.equals(value))){
				throw new IllegalArgumentException(String.format("Unsupported value [%s] for feature [%s]",value,feature));
			}
			break;
		case UNKNOWN_FIELD_ELEM_NAME:
		case EXTENSION_FIELD_NAME_PREFIX:
			if(value==null||!(String.class).isAssignableFrom(value.getClass())
					||((String)value).trim().equals("")){
				throw new IllegalArgumentException(String.format("Feature [%s] expected to be a non null string",feature));
			}
			break;
		default:
			throw new IllegalArgumentException(String.format("Unsupported feature [%s]",feature));
		}	
	}	
}
