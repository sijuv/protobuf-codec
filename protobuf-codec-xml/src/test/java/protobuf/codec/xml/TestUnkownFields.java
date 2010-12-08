package protobuf.codec.xml;

import static junit.framework.Assert.assertEquals;

import java.io.StringReader;
import java.io.StringWriter;

import org.junit.Before;
import org.junit.Test;

import protobuf.codec.Codec;
import protobuf.codec.xml.TypesProtoBuf.Unknown;
import protobuf.codec.xml.TypesProtoBuf.Version;

import com.google.protobuf.ExtensionRegistry;

public class TestUnkownFields {

	private Unknown typesUnknown;

	private protobuf.codec.xml.UnknownProtoBuf.Unknown unknownUnknown;

	@Before
	public void setUp() {
		typesUnknown = Unknown
				.newBuilder()
				.setId(1)
				.setName("HelloWorld")
				.addAlias("superman")
				.addAlias("spiderman")
				.addVerions(
						Version.newBuilder().setName("ver1").setVernum(1)
								.build())
				.addVerions(
						Version.newBuilder().setName("ver2").setVernum(2)
								.build())
				.setLiveversion(
						Version.newBuilder().setName("liveversion")
								.setVernum(100).build())
				.setExtension(TypesProtoBuf.othername, "other extn").build();
		unknownUnknown = protobuf.codec.xml.UnknownProtoBuf.Unknown
				.newBuilder().setName("Hello Unknown").build();
	}

	@Test
	public void serializeUnknownBasic() throws Exception {
		protobuf.codec.xml.UnknownProtoBuf.Unknown unknown1 = protobuf.codec.xml.UnknownProtoBuf.Unknown
				.newBuilder().mergeFrom(typesUnknown.toByteArray()).build();

		byte[] unknowns = unknown1.getUnknownFields().toByteArray();

		Codec codec = new XmlCodec();
		StringWriter writer = new StringWriter();
		codec.fromMessage(unknown1, writer);
		String xml = writer.toString();
		ExtensionRegistry extnReg = ExtensionRegistry.newInstance();
		extnReg.add(TypesProtoBuf.othername);
		assertEquals(typesUnknown,
				codec.toMessage(Unknown.class, new StringReader(xml), extnReg));
	}

}
