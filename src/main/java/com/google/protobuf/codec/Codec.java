package com.google.protobuf.codec;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;

import com.google.protobuf.ExtensionRegistry;
import com.google.protobuf.Message;

/**
 * Protobuf codec. 
 * Please read the codec implementation for exact details on features like support for unknownfields , print formats etc.
 * @author sijuv
 *
 */
public interface Codec {
	
	/**
	 * Write this Message {@link Message} to the provided output stream. The underlying stream is not closed by default, 
	 * user {@link Feature#CLOSE_STREAM} for the required settings.
	 * The default values are not written out, the reason being that the client should be aware of the protobuf schema.
	 * Extension fields are written out.
	 * @param message the {@link Message}
	 * @param os the output stream
	 * @throws IOException
	 * @throws IllegalArgumentException if the provided message is not initialized 
	 */
	void fromMessage(Message message,Writer writer) throws IOException;
	
	/**
	 * Create a {@link Message} off the provided input stream.The underlying stream is not closed by default, 
	 * user {@link Feature#CLOSE_STREAM} for the required settings.
	 * In case null values encountered are skipped since protobuf does not support null values yet 
	 * ( http://code.google.com/p/protobuf/issues/detail?id=57 )
	 * Extension fields are not written out
	 * @param messageType the {@link Class} corresponding to the {@link Message} the stream needs to be read into
	 * @param in the input stream
	 * @return the {@link Message}
	 * @throws IOException
	 */
	Message toMessage(Class<? extends Message> messageType,Reader reader) throws IOException;
	
	
	/**
	 * Create a {@link Message} off the provided input stream.The underlying stream is not closed by default, 
	 * user {@link Feature#CLOSE_STREAM} for the required settings.
	 * In case null values encountered are skipped since protobuf does not support null values yet 
	 * ( http://code.google.com/p/protobuf/issues/detail?id=57 )
	 * Extension fields looked up from the provided extension registry, if a field from the provided stream
	 * is identified to be an extension field, but a corresponding mapping is not found in the registry, then 
	 * the field is skipped.
	 * @param messageType the {@link Class} corresponding to the {@link Message} the stream needs to be read into
	 * @param in the input stream
	 * @param extnRegistry the extension registry
	 * @return the {@link Message}
	 * @throws IOException
	 */
	Message toMessage(Class<? extends Message> messageType,Reader reader,ExtensionRegistry extnRegistry) throws IOException;
	
	
	
	/**
	 * Write the message to the stream, uses UTF8 as the charset
	 * @param message
	 * @param os
	 * @throws IOException
	 * @see {@link #fromMessage(Message, Writer)}
	 */
	void fromMessage(Message message,OutputStream os) throws IOException;
	
	

	
	/**
	 * Read the message from the stream, uses UTF8 as the charset encoding.
	 * @param messageType the {@link Class} corresponding to the {@link Message} the stream needs to be read into
	 * @param in
	 * @return
	 * @throws IOException
	 * @see {@link #toMessage(Class, Reader)}
	 */
	Message toMessage(Class<Message> messageType,InputStream in) throws IOException;
	
	/**
	 * Read the message from the stream, uses UTF8 as the charset encoding.
	 * @param messageType the {@link Class} corresponding to the {@link Message} the stream needs to be read into
	 * @param in
	 * @param extnRegistry the extension registry
	 * @return
	 * @throws IOException
	 * @see {@link #toMessage(Reader)}
	 */
	Message toMessage(Class<Message> messageType,InputStream in,ExtensionRegistry extnRegistry) throws IOException;
	
	/**
	 * Set a codec feature, 
	 * @param feature the feature
	 * @param value the feature value.
	 * @throws IllegalArgumentException in case the codec does not support the provided feature
	 * 				in case the value is not applicable for the feature
	 */
	void setFeature(Feature feature,Object value);
	
	/**
	 * Get the value this feature is set to.
	 * @param feature the feature
	 * @return the value this feature is set to.
	 */
	Object getFeature(Feature feature);
	
	/**
	 * Indicates whether this feature is set on the codec.
	 * @param feature the feature
	 * @return <code>true</code> if feature is set, <code>false</code> otherwise
	 */
	boolean isFeatureSet(Feature feature);
	
	/**
	 * Features supported by the codec.
	 * @author sijuv
	 *
	 */
	public enum Feature{
		/** Pretty print **/
		PRETTY_PRINT,
		/** Close the underlying stream */
		CLOSE_STREAM,
		/** Write out the default values of fields in case those fields are not set */
		WRITE_DEFAULT_VALUES; 
		
	}
}
