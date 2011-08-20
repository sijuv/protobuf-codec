package protobuf.codec.text;

import static org.junit.Assert.assertEquals;

import com.google.protobuf.ByteString;
import org.junit.Test;
import protobuf.codec.Codec;

import java.io.StringReader;
import java.io.StringWriter;

/**
 * User: aantonov
 * Date: 7/21/11
 */
public class TextCodecTest {

    @Test
    public void testBasics() throws Exception {
        TypesProtoBuf.Types types = TypesProtoBuf.Types.newBuilder()
				.setIdbool(true)
				.setIddouble(Double.MAX_VALUE)
				.setIdfixed32(Integer.MIN_VALUE)
				.setIdfixed64(Long.MIN_VALUE)
				.setIdfloat(Float.MAX_VALUE)
				.setIdint32(1)
				.setIdint64(5000000000000000000l)
				.setIdsfixed32(-56)
				.setIdsfixed64(-561234561435l)
				.setIdsint32(-100)
				.setIdsint64(Long.MAX_VALUE)
				.setIdstring("Hello World")
				.setIduint32(100)
				.setIduint64(100l)
				.setLang(TypesProtoBuf.Lang.HASKELL)
				.setIdbyte(ByteString.copyFromUtf8("HelloWorld"))
				.build();

        StringWriter data = new StringWriter();
        data.append("idstring: \"Hello World\"\n" +
                "idint32: 1\n" +
                "iddouble: 1.7976931348623157E308\n" +
                "idfloat: 3.4028235E38\n" +
                "idint64: 5000000000000000000\n" +
                "iduint32: 100\n" +
                "iduint64: 100\n" +
                "idsint32: -100\n" +
                "idsint64: 9223372036854775807\n" +
                "idfixed32: 2147483648\n" +
                "idfixed64: 9223372036854775808\n" +
                "idsfixed32: -56\n" +
                "idsfixed64: -561234561435\n" +
                "idbool: true\n" +
                "lang: HASKELL\n" +
                "idbyte: \"HelloWorld\"\n");
        data.flush();

        StringWriter writer = new StringWriter();
        Codec textCodec = new TextCodec();
        textCodec.fromMessage(types, writer);

        assertEquals(data.toString(), writer.toString());

        TypesProtoBuf.Types msg = textCodec.toMessage(TypesProtoBuf.Types.class, new StringReader(writer.toString()));

        assertEquals(types, msg);
    }
}
