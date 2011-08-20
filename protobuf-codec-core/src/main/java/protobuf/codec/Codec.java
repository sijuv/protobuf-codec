package protobuf.codec;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.util.Map;

import com.google.protobuf.ExtensionRegistry;
import com.google.protobuf.Message;
import com.google.protobuf.UnknownFieldSet;

/**
 * Protobuf codec. 
 * Please read the codec implementation for exact details on features like support for unknownfields , print formats etc.
 * @author sijuv
 *
 */
public interface Codec {
	
	/**
	 * The default element name that the for the field in the response/input which contains 
	 * the unknown fields. This element name can be overridden with the value set to the 
	 * feature {@link Feature#UNKNOWN_FIELD_ELEM_NAME}
	 */
	public static final String DEFAULT_UNKNOWN_FIELD_ELEM_NAME="unknownfields";
	
	/**
	 * The default prefix to be used for extension fields. The value of this field can be controlled by the 
	 * feature {@link Feature#EXTENSION_FIELD_NAME_PREFIX}. The name of the extension field will be the value that this feature gives
	 * appended by "-<fieldname>"
	 */
	public static final String DEFAULT_EXTENSION_NAME_PREFIX="extension";

	
	/**
	 * Write this Message {@link Message} to the provided output stream. The underlying stream is not closed by default, 
	 * user {@link Feature#CLOSE_STREAM} for the required settings.
	 * The default values are not written out, the reason being that the client should be aware of the protobuf schema.
	 * The {@link UnknownFieldSet} are written out on whether the {@link Feature#UNKNOWN_FIELD_ELEM_NAME} is set. By default, {@link UnknownFieldSet}
	 * is written out Check the codec implementation on how the unknown field set is written out.
	 * Extension fields are written out and the naming depends on the {@link Feature#EXTENSION_FIELD_NAME_PREFIX} set. 
	 * If an extension field  not provided in the registry is encountered that field is skipped.
	 * @param message the {@link Message}
	 * @param writer the output stream writer
	 * @throws IOException
	 * @throws IllegalArgumentException if the provided message is not initialized 
	 */
	void fromMessage(Message message,Writer writer) throws IOException;
	
	/**
	 * Create a {@link Message} off the provided input stream.The underlying stream is not closed by default, 
	 * user {@link Feature#CLOSE_STREAM} for the required settings.
	 * In case null values encountered are skipped since protobuf does not support null values yet 
	 * ( http://code.google.com/p/protobuf/issues/detail?id=57 )
	 * The default values are not written out, the reason being that the client should be aware of the protobuf schema.
	 * The {@link UnknownFieldSet} are read in depending on whether the {@link Feature#UNKNOWN_FIELD_ELEM_NAME} is set. By default, {@link UnknownFieldSet}
	 * is read in. Check the codec implementation on how the unknown field need to be passed in.
	 * Extension field names depend on {@link Feature#EXTENSION_FIELD_NAME_PREFIX}
	 * If an extension field  not provided in the registry is encountered that field is skipped.
	 * @param messageType the {@link Class} corresponding to the {@link Message} the stream needs to be read into
	 * @param reader the input stream reader
	 * @return the {@link Message}
	 * @throws IOException
	 */
	<T extends Message> T toMessage(Class<T> messageType,Reader reader) throws IOException;
	
	
	/**
	 * Create a {@link Message} off the provided input stream.The underlying stream is not closed by default, 
	 * user {@link Feature#CLOSE_STREAM} for the required settings.
	 * In case null values encountered are skipped since protobuf does not support null values yet 
	 * ( http://code.google.com/p/protobuf/issues/detail?id=57 )
	 * Extension fields looked up from the provided extension registry, if a field from the provided stream
	 * is identified to be an extension field, but a corresponding mapping is not found in the registry, then 
	 * the field is skipped.
	 * @param messageType the {@link Class} corresponding to the {@link Message} the stream needs to be read into
	 * @param reader the input stream reader
	 * @param extnRegistry the extension registry which contains the defn for extension fields.
	 * @return the {@link Message}
	 * @throws IOException
	 */
	<T extends Message> T toMessage(Class<T> messageType,Reader reader,ExtensionRegistry extnRegistry) throws IOException;
	
	
	
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
	<T extends Message> T toMessage(Class<T> messageType,InputStream in) throws IOException;
	
	/**
	 * Read the message from the stream, uses UTF8 as the charset encoding.
	 * @param messageType the {@link Class} corresponding to the {@link Message} the stream needs to be read into
	 * @param in
	 * @param extnRegistry the extension registry
	 * @return
	 * @throws IOException
	 * @see {@link #toMessage(Class, Reader)}
	 */
	<T extends Message> T toMessage(Class<T> messageType,InputStream in,ExtensionRegistry extnRegistry) throws IOException;
	
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
	 * Returns an unmodifiable map containing all features and the values they are set to.
	 * @return
	 */
	Map<Feature,Object> getAllFeaturesSet();
	
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
		/** Support unknown fields */
		SUPPORT_UNKNOWN_FIELDS,
		/** Unknown field element name */
		UNKNOWN_FIELD_ELEM_NAME,
		/** Extension field name prefix */
		EXTENSION_FIELD_NAME_PREFIX,
        /** Strip leading and trailing underscores from field names */
        STRIP_FIELD_NAME_UNDERSCORES,
        /** Provide field name substitutes for reading and writing from/to a protobuf stream*/
        FIELD_NAME_READ_SUBSTITUTES,
        FIELD_NAME_WRITE_SUBSTITUTES;
	}
}
