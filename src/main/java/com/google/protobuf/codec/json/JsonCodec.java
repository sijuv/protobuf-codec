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
	 * @see Codec#toMessage(Class, Reader)
	 */
	@Override
	protected Message readFromStream(Builder builder, Reader reader)throws IOException {
		JsonFactory jsonFactory=new JsonFactory();
		
		JsonParser parser=jsonFactory.createJsonParser(reader);
		if(isFeatureSet(Feature.CLOSE_STREAM)&&Boolean.FALSE.equals(getFeature(Feature.CLOSE_STREAM))){
			parser.configure(org.codehaus.jackson.JsonParser.Feature.AUTO_CLOSE_SOURCE, false);
		}
		return JacksonJsonReader.parse(builder, parser);
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
	
	/**
	 * Jackson json reader
	 * @author sijuv
	 *
	 */
	private static class JacksonJsonReader{
		
		
		private static Message parse(Builder builder,JsonParser parser) throws IOException{
			parser.nextToken();
			parseObject(builder,parser);
			return builder.build();
		}
		
		
		private static Builder parseObject(Builder builder,JsonParser parser) throws IOException{
			assert(JsonToken.START_OBJECT.equals(parser.getCurrentToken())); // Ensure start token
			
			Descriptor descriptor =builder.getDescriptorForType();
			while(!parser.nextToken().equals(JsonToken.END_OBJECT)){
				JsonToken currToken=parser.getCurrentToken();
				assert(currToken.equals(JsonToken.FIELD_NAME));
				String fieldName=parser.getCurrentName();
				FieldDescriptor field=descriptor.findFieldByName(fieldName);
				parser.nextToken();
				setFields(builder, field, parser);	
			}
			return builder;
		}
		
		private static Builder setFields(Builder builder,FieldDescriptor field,JsonParser parser)throws IOException{
			Object value=getValue(builder, field, parser);
			if(value==null){
				//What to do in case of null values ? protobuf does not allow null.
			}else{
				builder.setField(field, value);
			}
			return builder;
		}

		
		private static void handleArray(Builder builder,FieldDescriptor arrayField,JsonParser parser) throws IOException{
			while(!JsonToken.END_ARRAY.equals(parser.nextToken())){
				JsonToken token=parser.getCurrentToken();
				if(JsonToken.START_ARRAY.equals(token)){
					//
				}else{
					Object value=getValue(builder, arrayField, parser);
					builder.addRepeatedField(arrayField, value);
				}
			}
		}
		
		private static Object getValue(Builder builder,FieldDescriptor field,JsonParser parser)throws IOException{
			JsonToken token=parser.getCurrentToken();
			Object value=null;
			switch(token){
			case VALUE_STRING:
				if(JavaType.ENUM.equals(field.getJavaType())){
					value=field.getEnumType().findValueByName(parser.getText());
				}else if(JavaType.STRING.equals(field.getJavaType())){
					value=parser.getText();
				}else{
					throw new UnsupportedEncodingException(String.format("Unsupported java type [%s] for field [%s] for json type VALUE_STRING",
																		field.getJavaType(),field.getName()));  
				}
				break;
			case VALUE_TRUE:
				value=Boolean.TRUE;
				break;
			case VALUE_FALSE:
				value=Boolean.FALSE;
				break;
			case VALUE_NUMBER_INT:
				if(field.getJavaType().equals(JavaType.INT)){
					value=parser.getIntValue();
				}else if(JavaType.LONG.equals(field.getJavaType())){
					value=parser.getLongValue();
				}else{
					throw new UnsupportedEncodingException(String.format("Unsupported java type [%s] for field [%s] for json type VALUE_NUMBER_INT",
							field.getJavaType(),field.getName()));
				}
				break;
			case VALUE_NUMBER_FLOAT:
				if(JavaType.DOUBLE.equals(field.getJavaType())){
					value=parser.getDoubleValue();
				}else if(JavaType.FLOAT.equals(field.getJavaType())){
					value=parser.getFloatValue();
				}else{
					throw new UnsupportedEncodingException(String.format("Unsupported java type [%s] for field [%s] for json type VALUE_NUMBER_FLOAT",
							field.getJavaType(),field.getName()));
				}
				break;
			case START_OBJECT:
				Builder newBuilder=builder.newBuilderForField(field);
				value=parseObject(newBuilder, parser).build();
				break;
			case START_ARRAY:
				handleArray(builder, field, parser);
				break;
				
			case VALUE_NULL:
				//protobuf does not support null, returning null here however.
				break;
			default:
				throw new UnsupportedEncodingException(String.format("Unsupported token type [%s]",token)); 
			}
			return value;
		}
	}


	/**
	 * Jackson json writer
	 * @author sijuv
	 *
	 */
	private static class JacksonJsonWriter {

		private static void generateJSONFields(Message message,JsonGenerator generator) throws IOException {
			
			generator.writeStartObject();
			Iterator<Map.Entry<FieldDescriptor, Object>> iterator = message.getAllFields().entrySet().iterator(); //Get all set fields
			while (iterator.hasNext()) {
				Map.Entry<FieldDescriptor, Object> record = iterator.next();
				FieldDescriptor field = record.getKey(); 
				String fieldName = field.isExtension() ? "[" + field.getName()+ "]" : field.getName(); //If extn field? box
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

		//Extract the field value depending on its java type
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
				break;// What to do here? can be used for unknown fields?//TODO UnknownFields?
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
}
