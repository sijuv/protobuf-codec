package com.google.protobuf.codec.json;

import static org.junit.Assert.*;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;

import org.codehaus.jackson.JsonEncoding;
import org.codehaus.jackson.JsonFactory;
import org.codehaus.jackson.JsonGenerator;
import org.junit.Before;
import org.junit.Test;

import com.google.protobuf.ExtensionRegistry;
import com.google.protobuf.codec.Codec;
import com.google.protobuf.codec.Codec.Feature;
import com.google.protobuf.codec.json.TypesProtoBuf.Foo;
import com.google.protobuf.codec.json.TypesProtoBuf.Lang;
import com.google.protobuf.codec.json.TypesProtoBuf.RepeatedFields;
import com.google.protobuf.codec.json.TypesProtoBuf.Types;
import com.google.protobuf.codec.json.TypesProtoBuf.Version;

/**
 * Test cases for json codec
 * @author sijuv
 *
 */
public class JsonCodecTest {

	private Types types;
	private String typesJson;
	
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
				.setLang(Lang.HASKELL).build();
		
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
		generator.writeStringField("[alias]", "Hi There");
		generator.writeArrayFieldStart("[langs]");
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
		generator.writeStringField("[alias]", "Hi There");
		generator.writeArrayFieldStart("[langs]");
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
}
