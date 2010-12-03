package com.google.protobuf.codec;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import com.google.protobuf.ExtensionRegistry;
import com.google.protobuf.Message;
import com.google.protobuf.Message.Builder;

/**
 * Codec base class, provides for common validations and functionality
 * @author sijuv
 *
 */
public abstract class AbstractCodec implements Codec {

	public static final String DEFAULT_CHARSET="UTF-8";
	
	protected Map<Feature, Object> featureMap = new HashMap<Feature, Object>();

	@Override
	public void fromMessage(Message message, OutputStream os)
			throws IOException {
		
		Writer writer=new BufferedWriter(new OutputStreamWriter(os ,DEFAULT_CHARSET));
		fromMessage(message, writer);
	}
	
	@Override
	public void fromMessage(Message message, Writer writer) throws IOException {
		if (!message.isInitialized()) {
			throw new IllegalArgumentException(
					"Provided protobuf message is not initialized, call build() on the Message");
		}
		writeToStream(message, writer);
		closeStreams(writer);
	}
	
	@Override
	public Message toMessage(Class<? extends Message> messageType,
			Reader reader, ExtensionRegistry extnRegistry) throws IOException {
		Builder builder=null;
		try{
			Method builderMethod=messageType.getMethod("newBuilder");
			builder=(Builder) builderMethod.invoke(messageType);
			return readFromStream(builder, reader,extnRegistry);
		}catch(IOException ioe){
			throw ioe;
		}catch(Exception e){
			throw new RuntimeException(e);
		}finally{
			closeStreams(reader);
		}
	}
	
	@Override
	public Message toMessage(Class<Message> messageType,InputStream in) throws IOException {
		Reader reader=new BufferedReader(new InputStreamReader(in,DEFAULT_CHARSET));
		return toMessage(messageType,reader);
	}
	
	@Override
	public Message toMessage(Class<? extends Message> messageType, Reader reader)throws IOException {
		return toMessage(messageType, reader,ExtensionRegistry.getEmptyRegistry());
	}
	
	@Override
	public Message toMessage(Class<Message> messageType, InputStream in,
			ExtensionRegistry extnRegistry) throws IOException {
		Reader reader=new BufferedReader(new InputStreamReader(in,DEFAULT_CHARSET));
		return toMessage(messageType,reader,extnRegistry);
	}
	
	protected void closeStreams(Closeable stream) throws IOException{
		if(isFeatureSet(Feature.CLOSE_STREAM)&&Boolean.TRUE.equals(Feature.CLOSE_STREAM)){
			stream.close();
		}
	}
	
	/**
	 * Whether the provied field name follows the naming scheme for extn fields.
	 * default pattern is "[<field_name>]"
	 * @param fieldName the field name
	 * @return <code>true</code> if the field name follows the naming scheme for extn fields, <code>false</code> otherwise
	 *         
	 */
	public  static boolean isExtensionFieldName(String fieldName){
		StringBuilder strb=new StringBuilder(fieldName);
		return (strb.charAt(0)=='[') && (strb.charAt(fieldName.length()-1)==']');
	}
	
	/**
	 * Returns the field protobuf extn field name for the provided field name.
	 * @param fieldName the provided field name
	 * @return the protobuf field name = <field_name> for "[<field_name>]"
	 * @throws IllegalArgumentException if {@link #isExtensionFieldName(String)} returns false
	 */
	public static String parseExtensionFieldName(String fieldName){
		if(!isExtensionFieldName(fieldName)){
			throw new IllegalArgumentException(String.format("Field [%s] does not follow the extn field naming scheme"));
		}
		StringBuilder strb=new StringBuilder(fieldName);
		return strb.substring(1,strb.length()-1);
	}
	
	/**
	 * The name this extn field needs to be written as 
	 * @param fieldName
	 * @return "[<fieldName>]"
	 */
	public static String getExtensionFieldName(String fieldName){
		StringBuilder strb=new StringBuilder(fieldName);
		return strb.insert(0,"[").append("]").toString();
	}
	
	protected abstract void writeToStream(Message message, Writer writer) throws IOException;	
	
	protected abstract Message readFromStream(Builder builder,Reader reader,ExtensionRegistry extnRegistry) throws IOException;
	
	protected abstract void validateAndSetFeature(Feature feature, Object value);
	

	
	@Override
	public void setFeature(Feature feature, Object value) {
		validateAndSetFeature(feature, value);
		featureMap.put(feature, value);
	}

	@Override
	public boolean isFeatureSet(Feature feature) {
		return featureMap.containsKey(feature);
	}
	
	@Override
	public Object getFeature(Feature feature) {
		return featureMap.get(feature);
	}

}
