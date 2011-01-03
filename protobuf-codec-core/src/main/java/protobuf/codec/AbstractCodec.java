package protobuf.codec;

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
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.codec.binary.Base64;

import com.google.protobuf.ExtensionRegistry;
import com.google.protobuf.InvalidProtocolBufferException;
import com.google.protobuf.Message;
import com.google.protobuf.Message.Builder;
import com.google.protobuf.UnknownFieldSet;

/**
 * Codec base class, provides for common validations and functionality
 * @author sijuv
 *
 */
//TODO Check Handle charset handling
public abstract class AbstractCodec implements Codec {

	public static final String DEFAULT_CHARSET="UTF-8";
	
	private Map<Feature, Object> featureMap = new HashMap<Feature, Object>();

	protected AbstractCodec(){
		featureMap.put(Feature.SUPPORT_UNKNOWN_FIELDS, Boolean.TRUE);
		featureMap.put(Feature.UNKNOWN_FIELD_ELEM_NAME, Codec.DEFAULT_UNKNOWN_FIELD_ELEM_NAME);
		featureMap.put(Feature.EXTENSION_FIELD_NAME_PREFIX,Codec.DEFAULT_EXTENSION_NAME_PREFIX);
		featureMap.put(Feature.CLOSE_STREAM, true);

	}
	
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
	@SuppressWarnings("unchecked")
	public <T extends Message> T toMessage(Class<T> messageType,
			Reader reader, ExtensionRegistry extnRegistry) throws IOException {
		Builder builder=null;
		try{
			Method builderMethod=messageType.getMethod("newBuilder");
			builder=(Builder) builderMethod.invoke(messageType);
			return (T)readFromStream(builder, reader,extnRegistry);
		}catch(IOException ioe){
			throw ioe;
		}catch(Exception e){
			throw new RuntimeException(e);
		}finally{
			closeStreams(reader);
		}
	}
	
	@Override
	public <T extends Message> T toMessage(Class<T> messageType,InputStream in) throws IOException {
		Reader reader=new BufferedReader(new InputStreamReader(in,DEFAULT_CHARSET));
		return toMessage(messageType,reader);
	}
	
	@Override
	public <T extends Message> T toMessage(Class<T> messageType, Reader reader)throws IOException {
		return toMessage(messageType, reader,ExtensionRegistry.getEmptyRegistry());
	}
	
	@Override
	public <T extends Message> T toMessage(Class<T> messageType, InputStream in,
			ExtensionRegistry extnRegistry) throws IOException {
		Reader reader=new BufferedReader(new InputStreamReader(in,DEFAULT_CHARSET));
		return toMessage(messageType,reader,extnRegistry);
	}
	
	/**
	 * Close streams
	 * @param stream
	 * @throws IOException
	 */
	protected void closeStreams(Closeable stream) throws IOException{
		if(isFeatureSet(Feature.CLOSE_STREAM)&&Boolean.TRUE.equals(Feature.CLOSE_STREAM)){
			stream.close();
		}
	}
	
	
	protected abstract void writeToStream(Message message, Writer writer) throws IOException;	
	
	protected abstract Message readFromStream(Builder builder,Reader reader,ExtensionRegistry extnRegistry) throws IOException;
	
	protected abstract void validateAndSetFeature(Feature feature, Object value);
	
	
	//TODO Rewrite 
	@Override
	public void setFeature(Feature feature, Object value) {
		if(Feature.SUPPORT_UNKNOWN_FIELDS.equals(feature)){
			if(Boolean.TRUE.equals(feature)||Boolean.FALSE.equals(value)){
				featureMap.put(feature, value);
			}else{
				throw new IllegalArgumentException(String.format("Feature [%s] does not support [%s] value",feature,value));
			}
		}
		if(Feature.UNKNOWN_FIELD_ELEM_NAME.equals(feature)){
			if(value==null){
				throw new IllegalArgumentException(String.format("Feature [%s] does not support [%s] value",feature,value));
			}else{
				featureMap.put(feature, value);
			}
		}
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
	
	
	/**
	 * Encodes the {@link UnknownFieldSet} to a base64 string
	 * @param unknownFields
	 * @return
	 */
	public static String encodeUnknownFieldsToString(UnknownFieldSet unknownFields){
		return new String(Base64.encodeBase64(unknownFields.toByteArray()));
	}
	
	/**
	 * Merges the unknownfield set, which was encoded as a base64 string to provided builder
	 * using the provided {@link ExtensionRegistry}
	 * @param builder
	 * @param extnReg
	 * @param unknownFieldText
	 * @return
	 * @throws InvalidProtocolBufferException
	 */
	public static  Builder mergeUnknownFieldsFromString(Builder builder,
			ExtensionRegistry extnReg, String unknownFieldText) throws InvalidProtocolBufferException{
		byte[] unknownFields = Base64.decodeBase64(unknownFieldText.getBytes());
		return builder.mergeFrom(unknownFields,extnReg);
	}
	
	@Override
	public Map<Feature, Object> getAllFeaturesSet() {
		return Collections.unmodifiableMap(featureMap);
	}
	
	
	/**
	 * Whether the provied field name follows the naming scheme for extn fields.
	 * default pattern is "extension-<field_name>". {@link Feature#EXTENSION_FIELD_NAME_PREFIX} can dictate the prefix
	 * @param fieldName the field name
	 * @return <code>true</code> if the field name follows the naming scheme for extn fields, <code>false</code> otherwise
	 *         
	 */
	public  static boolean isExtensionFieldName(String fieldName,Map<Feature, Object> featureMap){
		return fieldName.startsWith(((String)featureMap.get(Feature.EXTENSION_FIELD_NAME_PREFIX))+"-");
	}
	
	/**
	 * Returns the field protobuf extn field name for the provided field name.
	 * @param fieldName the provided field name
	 * @return the protobuf field name = <field_name> for "extension_field_name>"
	 * @throws IllegalArgumentException if {@link #isExtensionFieldName(String)} returns false
	 */
	public static String parseExtensionFieldName(String fieldName,Map<Feature, Object> featureMap){
		if(!isExtensionFieldName(fieldName,featureMap)){
			throw new IllegalArgumentException(String.format("Field [%s] does not follow the extn field naming scheme"));
		}
		StringBuilder strb=new StringBuilder(fieldName);
		return strb.substring((((String)featureMap.get(Feature.EXTENSION_FIELD_NAME_PREFIX))+"-").length(),fieldName.length());
	}
	
	/**
	 * The name this extn field needs to be written as 
	 * @param fieldName
	 * @return  {@link Feature#EXTENSION_FIELD_NAME_PREFIX}"_<fieldName>"
	 */
	public static String getExtensionFieldName(String fieldName,Map<Feature,Object> featureMap){
		StringBuilder strb=new StringBuilder((String)featureMap.get(Feature.EXTENSION_FIELD_NAME_PREFIX));
		strb.append("-");
		strb.append(fieldName);
		return strb.toString();
	}
	
	/**
	 * Should I support unknown fields
	 * @param featureMap
	 * @return
	 * @see Feature#SUPPORT_UNKNOWN_FIELDS
	 */
	public static boolean supportUnknownFields(Map<Feature,Object> featureMap){
		return Boolean.TRUE.equals(featureMap.get(Feature.SUPPORT_UNKNOWN_FIELDS));
	}
	
	/**
	 * Get the name for the element which should contain the unknown fields
	 * @param featureMap
	 * @return
	 * @see Feature#UNKNOWN_FIELD_ELEM_NAME
	 */
	public static String getUnknownFieldElementName(Map<Feature,Object> featureMap){
		return (String)featureMap.get(Feature.UNKNOWN_FIELD_ELEM_NAME);
	}
	
	/**
	 * Should I pretty print?
	 * @param featureMap
	 * @return
	 * @see Feature#PRETTY_PRINT
	 */
	public static boolean prettyPrint(Map<Feature,Object> featureMap){
		return featureMap.containsKey(Feature.PRETTY_PRINT)&&Boolean.TRUE.equals(featureMap.get(Feature.PRETTY_PRINT));
	}
	
	/**
	 * Should I close the underlying stream
	 * @param featureMap
	 * @return
	 * @see Feature#CLOSE_STREAM
	 */
	public static boolean closeStream(Map<Feature,Object> featureMap){
		return featureMap.containsKey(Feature.CLOSE_STREAM)&&Boolean.TRUE.equals(featureMap.get(Feature.CLOSE_STREAM));
	}
	
	/**
	 * Is the provided field name an unknown field ?
	 * @param fieldName
	 * @param featureMap
	 * @return
	 */
	public static boolean isFieldNameUnknownField(String fieldName,Map<Feature,Object> featureMap){
		return featureMap.get(Feature.UNKNOWN_FIELD_ELEM_NAME).equals(fieldName);
	}

}
