package protobuf.codec.text;

import com.google.protobuf.ExtensionRegistry;
import com.google.protobuf.Message;
import com.google.protobuf.TextFormat;
import protobuf.codec.AbstractCodec;

import java.io.*;

/**
 * User: aantonov
 * Date: 7/21/11
 */
public class TextCodec extends AbstractCodec {
    @Override
    protected void writeToStream(Message message, Writer writer) throws IOException {
        TextFormat.print(message, writer);
    }

    @Override
    protected Message readFromStream(Message.Builder builder, Reader reader, ExtensionRegistry extnRegistry) throws IOException {
        TextFormat.merge(reader, extnRegistry, builder);
        return builder.build();
    }

    @Override
    protected void validateAndSetFeature(Feature feature, Object value) {
        // The features don't really matter, since there is no way to set any options to the Google's provided TextFormat
    }
}
