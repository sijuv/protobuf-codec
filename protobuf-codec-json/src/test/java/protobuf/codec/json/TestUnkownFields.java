package protobuf.codec.json;

import static junit.framework.Assert.assertEquals;

import java.io.StringReader;
import java.io.StringWriter;

import org.junit.Before;
import org.junit.Test;

import protobuf.codec.Codec;
import protobuf.codec.json.JsonCodec;
import protobuf.codec.json.TypesProtoBuf;
import protobuf.codec.json.TypesProtoBuf.Unknown;
import protobuf.codec.json.TypesProtoBuf.Version;

import com.google.protobuf.ExtensionRegistry;

public class TestUnkownFields {
	
	private Unknown typesUnknown;
	
	private protobuf.codec.json.UnknownProtoBuf.Unknown unknownUnknown;
	
	@Before
	public void setUp() {
		typesUnknown=Unknown.newBuilder()
						.setId(1)
						.setName("HelloWorld")
						.addAlias("superman")
						.addAlias("spiderman")
						.addVerions(Version.newBuilder().setName("ver1").setVernum(1).build())
						.addVerions(Version.newBuilder().setName("ver2").setVernum(2).build())
						.setLiveversion(Version.newBuilder().setName("liveversion").setVernum(100).build())
						.setExtension(TypesProtoBuf.othername, "other extn")
						.build();
		unknownUnknown=protobuf.codec.json.UnknownProtoBuf.Unknown.newBuilder()
						.setName("Hello Unknown")
						.build();
	}
	
	@Test
	public void serializeUnknownBasic() throws Exception{
		protobuf.codec.json.UnknownProtoBuf.Unknown unknown1 = protobuf.codec.json.UnknownProtoBuf.Unknown
				.newBuilder().mergeFrom(typesUnknown.toByteArray()).build();
				
		Codec codec=new JsonCodec();
		StringWriter writer=new StringWriter();
		codec.fromMessage(unknown1, writer);
		String json=writer.toString();
		System.out.println(json);
		ExtensionRegistry extnReg=ExtensionRegistry.newInstance();
		extnReg.add(TypesProtoBuf.othername);
		assertEquals(typesUnknown, codec.toMessage(Unknown.class, new StringReader(json),extnReg));
	}

}
