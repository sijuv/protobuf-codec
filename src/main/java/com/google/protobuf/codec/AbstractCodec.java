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
	public Message toMessage(Class<Message> messageType,InputStream in) throws IOException {
		Reader reader=new BufferedReader(new InputStreamReader(in,DEFAULT_CHARSET));
		return toMessage(messageType,reader);
	}
	
	@Override
	public Message toMessage(Class<Message> messageType, Reader reader)throws IOException {
		Builder builder=null;
		try{
			Method builderMethod=messageType.getMethod("newBuilder");
			builder=(Builder) builderMethod.invoke(messageType);
			return readFromStream(builder, reader);
		}catch(IOException ioe){
			throw ioe;
		}catch(Exception e){
			throw new RuntimeException(e);
		}finally{
			closeStreams(reader);
		}
	}
	
	
	protected void closeStreams(Closeable stream) throws IOException{
		if(isFeatureSet(Feature.CLOSE_STREAM)&&Boolean.TRUE.equals(Feature.CLOSE_STREAM)){
			
			stream.close();
		}
	}
	
	protected abstract void writeToStream(Message message, Writer writer) throws IOException;	
	
	protected abstract Message readFromStream(Builder builder,Reader reader) throws IOException;
	
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
