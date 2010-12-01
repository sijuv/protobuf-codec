package com.google.protobuf.codec;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;

import com.google.protobuf.Message;

/**
 * Protobuf codec. 
 * Please read the codec implementation for exact details on features like support for unknownfields , print formats etc.
 * @author sijuv
 *
 */
public interface Codec {
	
	/**
	 * Write this Message {@link Message} to the provided output stream. The writer is <b>not</b> closed once the message is written out.
	 * The client should close the stream itself. 
	 * @param message the {@link Message}
	 * @param os the output stream
	 * @throws IOException
	 * @throws IllegalArgumentException if the provied message is not initialized 
	 */
	void fromMessage(Message message,Writer writer) throws IOException;
	
	/**
	 * Create a {@link Message} off the provided input stream. The reader is <b>not</b> closed once the message is read in.
	 * @param messageType the {@link Class} corresponding to the {@link Message} the stream needs to be read into
	 * @param in the input stream
	 * @return the {@link Message}
	 * @throws IOException
	 */
	Message toMessage(Class<Message> messageType,Reader reader) throws IOException;
	
	
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
	 * @see {@link #toMessage(Reader)}
	 */
	Message toMessage(Class<Message> messageType,InputStream in) throws IOException;
	
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
		CLOSE_STREAM; 
		
	}
}
