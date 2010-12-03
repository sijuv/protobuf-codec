package com.google.protobuf.codec.json;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.codehaus.jackson.JsonFactory;
import org.codehaus.jackson.JsonGenerator;
import org.codehaus.jackson.JsonParser;
import org.codehaus.jackson.JsonToken;

import com.google.protobuf.Descriptors.Descriptor;
import com.google.protobuf.Descriptors.EnumValueDescriptor;
import com.google.protobuf.Descriptors.FieldDescriptor;
import com.google.protobuf.Descriptors.FieldDescriptor.JavaType;
import com.google.protobuf.ExtensionRegistry;
import com.google.protobuf.Message;
import com.google.protobuf.Message.Builder;
import com.google.protobuf.UnknownFieldSet;
import com.google.protobuf.codec.AbstractCodec;
import com.google.protobuf.codec.Codec;

/**
 * Json-protobuf serializer/deserializer.Relies on jackson as the underlying parser.
 * Does <b>not</b> support {@link UnknownFieldSet}, extension field names are boxed ("[<extension_field_name]")
 *
 * @author sijuv
 * 
 */
public class JsonCodec extends AbstractCodec {
	
	public JsonCodec(){
		//By default jackson closes the stream.
		featureMap.put(Feature.CLOSE_STREAM, true);
	}


	/**
	 * Fills in a message from the data from the stream, The stream is not closed once the message is read in, the extn field names 
	 * need to be boxed.
	 * @param builder the message builder
	 * @param reader the input stream,
	 * @param extnRegistry the extension registry to use
	 * @see Codec#toMessage(Class, Reader)
	 */
	@Override
	protected Message readFromStream(Builder builder, Reader reader,ExtensionRegistry extnRegistry)throws IOException {
		JsonFactory jsonFactory=new JsonFactory();
		JsonParser parser=jsonFactory.createJsonParser(reader);
		if(isFeatureSet(Feature.CLOSE_STREAM)&&Boolean.FALSE.equals(getFeature(Feature.CLOSE_STREAM))){
			parser.configure(org.codehaus.jackson.JsonParser.Feature.AUTO_CLOSE_SOURCE, false);
		}
		return JacksonJsonReader.parse(builder, parser,extnRegistry);
	}
	
	/**
	 * Writes out the messages as a json object the provided stream.
	 * The stream is not closed once the message is written out. UnknownFieldSet and default values, whether extn or first class 
	 * are not serialized.
	 * @param message the provided protobuf message
	 * @param os the output stream onto which the message is written to.
	 * @see Codec#toMessage(InputStream)
	 * Sample response:<br>
	 * {"id":1,"name":"elton john","[bar]":1,"[id]":24,"[place]":"london"}
	 * [bar]and [place] are extension fields.
	 * 
	 */
	@Override
	protected void writeToStream(Message message, Writer writer)throws IOException {
		JsonFactory jsonFactory = new JsonFactory();
		JsonGenerator generator = jsonFactory.createJsonGenerator(writer);
		if(isFeatureSet(Feature.PRETTY_PRINT)&&Boolean.TRUE.equals(getFeature(Feature.PRETTY_PRINT))){
			generator.useDefaultPrettyPrinter(); 
		}
		if(isFeatureSet(Feature.CLOSE_STREAM)&&Boolean.FALSE.equals(getFeature(Feature.CLOSE_STREAM))){
			generator.configure(org.codehaus.jackson.JsonGenerator.Feature.AUTO_CLOSE_TARGET, false);
		}
		JacksonJsonWriter.generateJSONFields(message, generator);
		generator.close();
		
	}
	
	
	@Override
	public void validateAndSetFeature(Feature feature, Object value) {
		switch(feature){
		case PRETTY_PRINT:
			if(!(Boolean.TRUE.equals(value)||Boolean.FALSE.equals(value))){
				throw new IllegalArgumentException(String.format("Unsupported value [%s] for feature [%s]",value,feature));
			}
			break;
		case CLOSE_STREAM:
			if(!(Boolean.TRUE.equals(value)||Boolean.FALSE.equals(value))){
				throw new IllegalArgumentException(String.format("Unsupported value [%s] for feature [%s]",value,feature));
			}
			break;
		default:
			throw new IllegalArgumentException(String.format("Unsupported feature [%s]",feature.name()));
		}
		
	}
}
