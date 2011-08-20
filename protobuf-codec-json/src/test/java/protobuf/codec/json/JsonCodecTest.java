package protobuf.codec.json;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.codec.binary.Base64;
import org.codehaus.jackson.JsonFactory;
import org.codehaus.jackson.JsonGenerator;
import org.junit.Before;
import org.junit.Test;

import protobuf.codec.Codec;
import protobuf.codec.Codec.Feature;
import protobuf.codec.json.JsonCodec;
import protobuf.codec.json.TypesProtoBuf;
import protobuf.codec.json.TypesProtoBuf.Foo;
import protobuf.codec.json.TypesProtoBuf.Lang;
import protobuf.codec.json.TypesProtoBuf.RepeatedFields;
import protobuf.codec.json.TypesProtoBuf.Types;
import protobuf.codec.json.TypesProtoBuf.Version;

import com.google.protobuf.ByteString;
import com.google.protobuf.ExtensionRegistry;


/**
 * Test cases for json codec
 * @author sijuv
 *
 */
//TODO Exten repeated objects
public class JsonCodecTest {

	private Types types;
	private String typesJson;
	private String helloworld="HelloWorld";
	private String helloworldInBase64=Base64.encodeBase64String(helloworld.getBytes());
	
	
	@Test(expected=IllegalArgumentException.class)
	public void ensureSafelyInitialized() throws IOException{
		RepeatedFields repFields=RepeatedFields.newBuilder()
		.addId(1)
		.addId(2)
		.addLangs(Lang.HASKELL)
		.addLangs(Lang.JAVA).buildPartial();
		new JsonCodec().fromMessage(repFields, new StringWriter());
		
	}
	
	
	@Before
	public void setupTypes() throws IOException {
		types=Types.newBuilder()
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
				.setLang(Lang.HASKELL)
				.setIdbyte(ByteString.copyFromUtf8(helloworld))
				.build();
		
		JsonFactory factory=new JsonFactory();
		
		StringWriter writer=new StringWriter();
		JsonGenerator generator=factory.createJsonGenerator(writer);
		generator.writeStartObject();

		generator.writeStringField("idstring", types.getIdstring());
		generator.writeNumberField("idint32", types.getIdint32());
		generator.writeNumberField("iddouble", types.getIddouble());
		generator.writeNumberField("idfloat", types.getIdfloat());
		generator.writeNumberField("idint64", types.getIdint64());
		generator.writeNumberField("iduint32", types.getIduint32());
		generator.writeNumberField("iduint64", types.getIduint64());
		generator.writeNumberField("idsint32", types.getIdsint32());
		generator.writeNumberField("idsint64", types.getIdsint64());
		generator.writeNumberField("idfixed32", types.getIdfixed32());
		generator.writeNumberField("idfixed64", types.getIdfixed64());
		generator.writeNumberField("idsfixed32", types.getIdsfixed32());
		generator.writeNumberField("idsfixed64", types.getIdsfixed64());
		generator.writeBooleanField("idbool", types.getIdbool());
		generator.writeStringField("lang", Lang.HASKELL.name());
		generator.writeStringField("idbyte", helloworldInBase64);

		generator.writeEndObject();
		generator.close();
		typesJson=writer.toString();
	}

	@Test
	public void ensureTypes() throws IOException{
		Codec codec=new JsonCodec();
		codec.setFeature(Feature.CLOSE_STREAM, true);
		ByteArrayOutputStream bos=new ByteArrayOutputStream();
		codec.fromMessage(types, bos);
		String jsonResponse=new String(bos.toByteArray());
		assertEquals(typesJson, jsonResponse);
		assertEquals(types, codec.toMessage(Types.class, new StringReader(typesJson)));
	}
	
	@Test
	public void ensureRepeatedFields() throws IOException{
		RepeatedFields repFields=RepeatedFields.newBuilder()
								.addId(1)
								.addId(2)
								.addLangs(Lang.HASKELL)
								.addLangs(Lang.JAVA)
								.addVersions(Version.newBuilder()
												.setName("release")
												.setVernum(1)
												.build())
								.addVersions(Version.newBuilder()
												.setName("nightly")
												.setVernum(2)
												.build())
								.addNames("funny")
								.addNames("bones")
								.setFieldId(5)
								.build();
		StringWriter writer1=new StringWriter();
		new JsonCodec().fromMessage(repFields, writer1);
		
		StringWriter writer=new StringWriter();
		JsonGenerator generator=(new JsonFactory()).createJsonGenerator(writer);
		generator.writeStartObject();
		generator.writeNumberField("fieldId", 5);

		generator.writeArrayFieldStart("id");
		generator.writeNumber( 1);
		generator.writeNumber( 2);
		generator.writeEndArray();
		generator.writeArrayFieldStart("names");
		generator.writeString("funny");
		generator.writeString("bones");
		generator.writeEndArray();
		generator.writeArrayFieldStart("versions");
		generator.writeStartObject();
		generator.writeStringField("name", "release");
		generator.writeNumberField("vernum", 1);
		generator.writeEndObject();
		generator.writeStartObject();
		generator.writeStringField("name", "nightly");
		generator.writeNumberField("vernum", 2);
		generator.writeEndObject();
		generator.writeEndArray();
		generator.writeArrayFieldStart("langs");
		generator.writeString( Lang.HASKELL.name());
		generator.writeString( Lang.JAVA.name());
		generator.writeEndArray();
		generator.writeEndObject();
		generator.close();
		assertEquals(writer1.toString(), writer.toString());
		assertEquals(repFields, new JsonCodec().toMessage(RepeatedFields.class, new StringReader(writer1.toString())));
	}
	
	@Test
	public void testForExtensionsWithRegistry() throws IOException{
		Foo foo=Foo.newBuilder()
					.setId(1)
					.setName("Hello World")
					.setExtension(TypesProtoBuf.langs, new ArrayList<Lang>(){{
						add(Lang.HASKELL);
						add(Lang.JAVA);
					}})
					.setExtension(TypesProtoBuf.alias, "Hi There")
					.build();
		
		StringWriter writer=new StringWriter();
		JsonGenerator generator=new JsonFactory().createJsonGenerator(writer);
		generator.writeStartObject();
		generator.writeNumberField("id", 1);
		generator.writeStringField("name", "Hello World");
		generator.writeStringField(Codec.DEFAULT_EXTENSION_NAME_PREFIX+"-"+"alias", "Hi There");
		generator.writeArrayFieldStart(Codec.DEFAULT_EXTENSION_NAME_PREFIX+"-"+"langs");
		generator.writeString(Lang.HASKELL.name());
		generator.writeString(Lang.JAVA.name());
		generator.writeEndArray();
		generator.writeEndObject();
		generator.close();
		
		StringWriter codecout=new StringWriter();
		new JsonCodec().fromMessage(foo, codecout);
		assertEquals(codecout.toString(),writer.toString());
		ExtensionRegistry reg=ExtensionRegistry.newInstance();
		reg.add(TypesProtoBuf.alias);
		reg.add(TypesProtoBuf.langs);
		assertEquals(new JsonCodec().toMessage(Foo.class, new StringReader(codecout.toString()),reg),foo);
	}
	
	@Test
	public void testForExtensionsWithoutRegistry() throws IOException{
		Foo foo=Foo.newBuilder()
					.setId(1)
					.setName("Hello World")
					.setExtension(TypesProtoBuf.langs, new ArrayList<Lang>(){{
						add(Lang.HASKELL);
						add(Lang.JAVA);
					}})
					.setExtension(TypesProtoBuf.alias, "Hi There")
					.build();
		
		StringWriter writer=new StringWriter();
		JsonGenerator generator=new JsonFactory().createJsonGenerator(writer);
		generator.writeStartObject();
		generator.writeNumberField("id", 1);
		generator.writeStringField("name", "Hello World");
		generator.writeStringField(Codec.DEFAULT_EXTENSION_NAME_PREFIX+"-"+"alias", "Hi There");
		generator.writeArrayFieldStart(Codec.DEFAULT_EXTENSION_NAME_PREFIX+"-"+"langs");
		generator.writeString(Lang.HASKELL.name());
		generator.writeString(Lang.JAVA.name());
		generator.writeEndArray();
		generator.writeEndObject();
		generator.close();
		
		StringWriter codecout=new StringWriter();
		new JsonCodec().fromMessage(foo, codecout);
		assertEquals(codecout.toString(),writer.toString());
		Foo foo1=Foo.newBuilder()
		.setId(1)
		.setName("Hello World").build();
		assertEquals(new JsonCodec().toMessage(Foo.class, new StringReader(codecout.toString())),foo1);
	}
	
	@Test
	public void testExtensionObjects()throws Exception{
		Foo foo1=Foo.newBuilder()
		.setId(1)
		.setName("Hello World")
		.setExtension(TypesProtoBuf.langs, new ArrayList<Lang>(){{
			add(Lang.HASKELL);
			add(Lang.JAVA);
		}})
		.addExtension(TypesProtoBuf.version,Version.newBuilder()
				.setName("release")
				.setVernum(1)
				.build())
		.addExtension(TypesProtoBuf.version,Version.newBuilder()
				.setName("nightly")
				.setVernum(2)
				.build())
		.setExtension(TypesProtoBuf.version1,Version.newBuilder()
				.setName("snapshot")
				.setVernum(10)
				.build())
		.setExtension(TypesProtoBuf.alias, "Hi There")
		.build();
		StringWriter sw=new StringWriter();
		Codec codec=new JsonCodec();
		codec.fromMessage(foo1, sw);
		
		StringWriter writer=new StringWriter();
		JsonGenerator generator=new JsonFactory().createJsonGenerator(writer);
		generator.writeStartObject();
		generator.writeNumberField("id", 1);
		generator.writeStringField("name", "Hello World");
		generator.writeStringField(Codec.DEFAULT_EXTENSION_NAME_PREFIX+"-"+"alias", "Hi There");
		generator.writeArrayFieldStart(Codec.DEFAULT_EXTENSION_NAME_PREFIX+"-"+"langs");
		generator.writeString(Lang.HASKELL.name());
		generator.writeString(Lang.JAVA.name());
		generator.writeEndArray();
		generator.writeArrayFieldStart(Codec.DEFAULT_EXTENSION_NAME_PREFIX+"-"+"version");
		generator.writeStartObject();
		generator.writeStringField("name", "release");
		generator.writeNumberField("vernum", 1);
		generator.writeEndObject();
		generator.writeStartObject();
		generator.writeStringField("name", "nightly");
		generator.writeNumberField("vernum", 2);
		generator.writeEndObject();
		generator.writeEndArray();		
		generator.writeFieldName(Codec.DEFAULT_EXTENSION_NAME_PREFIX+"-"+"version1");
		generator.writeStartObject();
		generator.writeStringField("name", "snapshot");
		generator.writeNumberField("vernum", 10);
		generator.writeEndObject();		
		generator.writeEndObject();
		generator.close();
		
		assertEquals(writer.toString(), sw.toString());
		//System.out.println(writer.toString());
		
		Foo foo2= codec.toMessage(Foo.class, new StringReader(writer.toString()));
		Foo foonoextn=Foo.newBuilder().setId(1).setName("Hello World").build();
		assertEquals(foo2,foonoextn);
		
		ExtensionRegistry reg=ExtensionRegistry.newInstance();
		reg.add(TypesProtoBuf.alias);
		reg.add(TypesProtoBuf.langs);
		reg.add(TypesProtoBuf.version);
		reg.add(TypesProtoBuf.version1);
		foo2=codec.toMessage(Foo.class, new StringReader(writer.toString()),reg);
		assertEquals(foo1, foo2);
	}

    @Test
    public void testForFieldNameReplacements() throws Exception {
        Foo obj = Foo.newBuilder().setId(1).setName("FooName").build();

        StringWriter writer = new StringWriter();
        JsonGenerator generator = new JsonFactory().createJsonGenerator(writer);
        generator.writeStartObject();
        generator.writeNumberField("_id", 1);
        generator.writeStringField("name_", "FooName");
        generator.writeEndObject();
        generator.close();

        Codec codec = new JsonCodec();
        Map<String, String> replaces = new HashMap<String, String>();
        replaces.put("name", "name_");
        replaces.put("id", "_id");
        codec.setFeature(Codec.Feature.FIELD_NAME_WRITE_SUBSTITUTES, replaces);
        replaces = new HashMap<String, String>();
        replaces.put("name_", "name");
        replaces.put("_id", "id");
        codec.setFeature(Codec.Feature.FIELD_NAME_READ_SUBSTITUTES, replaces);

        StringWriter out = new StringWriter();
        codec.fromMessage(obj, out);

        assertEquals(writer.toString(), out.toString());

        Foo msg = codec.toMessage(Foo.class, new StringReader(out.toString()));

        assertEquals(obj, msg);
    }

}
