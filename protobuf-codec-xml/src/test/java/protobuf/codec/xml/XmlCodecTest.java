package protobuf.codec.xml;

import static junit.framework.Assert.assertEquals;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import org.junit.Before;
import org.junit.Test;

import protobuf.codec.Codec;
import protobuf.codec.xml.TypesProtoBuf;
import protobuf.codec.xml.XmlCodec;
import protobuf.codec.xml.TypesProtoBuf.Foo;
import protobuf.codec.xml.TypesProtoBuf.Lang;
import protobuf.codec.xml.TypesProtoBuf.RepeatedFields;
import protobuf.codec.xml.TypesProtoBuf.Types;
import protobuf.codec.xml.TypesProtoBuf.Version;

import com.google.protobuf.ExtensionRegistry;

public class XmlCodecTest {
	private Types types;
	private String typesXml;
	private Foo foo;
	private String fooXml;
	
	
	@Before
	public void setupTypes() throws IOException,XMLStreamException{
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
		
		XMLOutputFactory factory=XMLOutputFactory.newInstance();
		StringWriter writer=new StringWriter();

		XMLStreamWriter xmlwriter=factory.createXMLStreamWriter(writer);
		
		xmlwriter.writeStartDocument();
		xmlwriter.writeStartElement("types");
		xmlwriter.writeStartElement("idstring");
		xmlwriter.writeCharacters(types.getIdstring());
		xmlwriter.writeEndElement();

		xmlwriter.writeStartElement("idint32");
		xmlwriter.writeCharacters(""+types.getIdint32());
		xmlwriter.writeEndElement();

		xmlwriter.writeStartElement("iddouble");
		xmlwriter.writeCharacters(""+types.getIddouble());
		xmlwriter.writeEndElement();

		xmlwriter.writeStartElement("idfloat");
		xmlwriter.writeCharacters(""+types.getIdfloat());
		xmlwriter.writeEndElement();

		xmlwriter.writeStartElement("idint64");
		xmlwriter.writeCharacters(""+types.getIdint64());
		xmlwriter.writeEndElement();

		xmlwriter.writeStartElement("iduint32");
		xmlwriter.writeCharacters(""+types.getIduint32());			
		xmlwriter.writeEndElement();

		xmlwriter.writeStartElement("iduint64");
		xmlwriter.writeCharacters(""+types.getIduint64());			
		xmlwriter.writeEndElement();

		xmlwriter.writeStartElement("idsint32");
		xmlwriter.writeCharacters(""+types.getIdsint32());			
		xmlwriter.writeEndElement();

		xmlwriter.writeStartElement("idsint64");
		xmlwriter.writeCharacters(""+types.getIdsint64());			
		xmlwriter.writeEndElement();

		xmlwriter.writeStartElement("idfixed32");
		xmlwriter.writeCharacters(""+types.getIdfixed32());			
		xmlwriter.writeEndElement();

		xmlwriter.writeStartElement("idfixed64");
		xmlwriter.writeCharacters(""+types.getIdfixed64());			
		xmlwriter.writeEndElement();

		xmlwriter.writeStartElement("idsfixed32");
		xmlwriter.writeCharacters(""+types.getIdsfixed32());	
		xmlwriter.writeEndElement();

		xmlwriter.writeStartElement("idsfixed64");
		xmlwriter.writeCharacters(""+types.getIdsfixed64());	
		xmlwriter.writeEndElement();

		xmlwriter.writeStartElement("idbool");
		xmlwriter.writeCharacters(""+types.getIdbool());	
		xmlwriter.writeEndElement();

		xmlwriter.writeStartElement("lang");
		xmlwriter.writeCharacters(Lang.HASKELL.name());	
		xmlwriter.writeEndElement();
		xmlwriter.writeEndElement();
		xmlwriter.writeEndDocument();
		xmlwriter.close();
		typesXml=writer.toString();
		
		foo=Foo.newBuilder()
		.setId(1)
		.setName("Hello World")
		.setExtension(TypesProtoBuf.langs, new ArrayList<Lang>(){{
			add(Lang.HASKELL);
			add(Lang.JAVA);
		}})
		.setExtension(TypesProtoBuf.alias, "Hi There")
		.build();

		StringWriter writer1=new StringWriter();

		XMLStreamWriter xmlwriter1=factory.createXMLStreamWriter(writer1);
		
		xmlwriter1.writeStartDocument();
		
		xmlwriter1.writeStartElement("foo");
		
		xmlwriter1.writeStartElement("id");
		xmlwriter1.writeCharacters(""+foo.getId());
		xmlwriter1.writeEndElement();
		
		xmlwriter1.writeStartElement("name");
		xmlwriter1.writeCharacters(""+foo.getName());
		xmlwriter1.writeEndElement();
		xmlwriter1.writeStartElement(Codec.DEFAULT_EXTENSION_NAME_PREFIX+"-"+"alias");
		xmlwriter1.writeCharacters("Hi There");
		xmlwriter1.writeEndElement();		
		xmlwriter1.writeStartElement(Codec.DEFAULT_EXTENSION_NAME_PREFIX+"-"+"langs");
		xmlwriter1.writeCharacters(""+Lang.HASKELL.name());
		xmlwriter1.writeEndElement();
		xmlwriter1.writeStartElement(Codec.DEFAULT_EXTENSION_NAME_PREFIX+"-"+"langs");
		xmlwriter1.writeCharacters(""+Lang.JAVA.name());
		xmlwriter1.writeEndElement();
		
		xmlwriter1.writeEndElement();
		xmlwriter1.writeEndDocument();
		xmlwriter1.close();
		
		fooXml=writer1.toString();
		
	}
	
	@Test
	public void testEnsureTypes() throws Exception{
		Codec xmlcodec=new XmlCodec();
		StringWriter writer=new StringWriter();
		xmlcodec.fromMessage(types, writer);
		writer.close();
		assertEquals(typesXml, writer.toString());
		assertEquals(types, xmlcodec.toMessage(Types.class, new StringReader(writer.toString())));
	}
	
	@Test
	public void validateExtensions() throws Exception{
		
		Codec xmlcodec=new XmlCodec();
		StringWriter writer=new StringWriter();
		xmlcodec.fromMessage(foo, writer);
		writer.close();
		assertEquals(writer.toString(), fooXml);
		ExtensionRegistry reg=ExtensionRegistry.newInstance();
		reg.add(TypesProtoBuf.alias);
		reg.add(TypesProtoBuf.langs);
		assertEquals(foo, xmlcodec.toMessage(Foo.class, new StringReader(writer.toString()),reg));
	}

	@Test
	public void ensureRepeatedFields() throws Exception{
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
		StringWriter wr=new StringWriter();
		Codec xmlcodec=new XmlCodec();
		xmlcodec.fromMessage(repFields, wr);

		StringWriter wr1=new StringWriter();
		XMLOutputFactory factory=XMLOutputFactory.newInstance();
		XMLStreamWriter xmlwriter1=factory.createXMLStreamWriter(wr1);
		
		xmlwriter1.writeStartDocument();
		
		xmlwriter1.writeStartElement("repeatedfields");
		
		xmlwriter1.writeStartElement("fieldId");
		xmlwriter1.writeCharacters(""+repFields.getFieldId());
		xmlwriter1.writeEndElement();
		xmlwriter1.writeStartElement("id");
		xmlwriter1.writeCharacters("1");
		xmlwriter1.writeEndElement();
		xmlwriter1.writeStartElement("id");
		xmlwriter1.writeCharacters("2");
		xmlwriter1.writeEndElement();
		xmlwriter1.writeStartElement("names");
		xmlwriter1.writeCharacters("funny");
		xmlwriter1.writeEndElement();
		xmlwriter1.writeStartElement("names");
		xmlwriter1.writeCharacters("bones");
		xmlwriter1.writeEndElement();
		
		xmlwriter1.writeStartElement("versions");
		xmlwriter1.writeStartElement("name");
		xmlwriter1.writeCharacters("release");
		xmlwriter1.writeEndElement();
		xmlwriter1.writeStartElement("vernum");
		xmlwriter1.writeCharacters("1");
		xmlwriter1.writeEndElement();
		xmlwriter1.writeEndElement();
		
		xmlwriter1.writeStartElement("versions");
		xmlwriter1.writeStartElement("name");
		xmlwriter1.writeCharacters("nightly");
		xmlwriter1.writeEndElement();
		xmlwriter1.writeStartElement("vernum");
		xmlwriter1.writeCharacters("2");
		xmlwriter1.writeEndElement();
		xmlwriter1.writeEndElement();
		
		xmlwriter1.writeStartElement("langs");
		xmlwriter1.writeCharacters("HASKELL");
		xmlwriter1.writeEndElement();
		
		xmlwriter1.writeStartElement("langs");
		xmlwriter1.writeCharacters("JAVA");
		xmlwriter1.writeEndElement();
		
		xmlwriter1.writeEndElement();
		xmlwriter1.writeEndDocument();
		xmlwriter1.close();
		
		assertEquals(wr1.toString(), wr.toString());
		
		RepeatedFields fromStream=(RepeatedFields) xmlcodec.toMessage(RepeatedFields.class,
				new StringReader(wr1.toString()));
		
		assertEquals(repFields, fromStream);
	}
	
	@Test
	public void testRepeatedExtensions() throws Exception{
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
		
		StringWriter writer1=new StringWriter();
		XMLOutputFactory factory=XMLOutputFactory.newInstance();

		XMLStreamWriter xmlwriter1=factory.createXMLStreamWriter(writer1);
		
		xmlwriter1.writeStartDocument();
		
		xmlwriter1.writeStartElement("foo");
		
		xmlwriter1.writeStartElement("id");
		xmlwriter1.writeCharacters(""+foo.getId());
		xmlwriter1.writeEndElement();
		
		xmlwriter1.writeStartElement("name");
		xmlwriter1.writeCharacters(""+foo.getName());
		xmlwriter1.writeEndElement();
		xmlwriter1.writeStartElement(Codec.DEFAULT_EXTENSION_NAME_PREFIX+"-"+"alias");
		xmlwriter1.writeCharacters("Hi There");
		xmlwriter1.writeEndElement();		
		xmlwriter1.writeStartElement(Codec.DEFAULT_EXTENSION_NAME_PREFIX+"-"+"langs");
		xmlwriter1.writeCharacters(""+Lang.HASKELL.name());
		xmlwriter1.writeEndElement();
		xmlwriter1.writeStartElement(Codec.DEFAULT_EXTENSION_NAME_PREFIX+"-"+"langs");
		xmlwriter1.writeCharacters(""+Lang.JAVA.name());
		xmlwriter1.writeEndElement();
		
		xmlwriter1.writeStartElement(Codec.DEFAULT_EXTENSION_NAME_PREFIX+"-"+"version");
		xmlwriter1.writeStartElement("name");
		xmlwriter1.writeCharacters("release");
		xmlwriter1.writeEndElement();
		xmlwriter1.writeStartElement("vernum");
		xmlwriter1.writeCharacters("1");
		xmlwriter1.writeEndElement();		
		xmlwriter1.writeEndElement();
		
		xmlwriter1.writeStartElement(Codec.DEFAULT_EXTENSION_NAME_PREFIX+"-"+"version");
		xmlwriter1.writeStartElement("name");
		xmlwriter1.writeCharacters("nightly");
		xmlwriter1.writeEndElement();
		xmlwriter1.writeStartElement("vernum");
		xmlwriter1.writeCharacters("2");
		xmlwriter1.writeEndElement();		
		xmlwriter1.writeEndElement();
		
		xmlwriter1.writeStartElement(Codec.DEFAULT_EXTENSION_NAME_PREFIX+"-"+"version1");
		xmlwriter1.writeStartElement("name");
		xmlwriter1.writeCharacters("snapshot");
		xmlwriter1.writeEndElement();
		xmlwriter1.writeStartElement("vernum");
		xmlwriter1.writeCharacters("10");
		xmlwriter1.writeEndElement();		
		xmlwriter1.writeEndElement();
		
		xmlwriter1.writeEndElement();
		xmlwriter1.writeEndDocument();
		xmlwriter1.close();
		
		StringWriter sw=new StringWriter();
		Codec codec=new XmlCodec();
		codec.fromMessage(foo1, sw);
		assertEquals(writer1.toString(), sw.toString());
		Foo foo2=Foo.newBuilder()
		.setId(1)
		.setName("Hello World").build();
		Foo foo3=(Foo) codec.toMessage(Foo.class, new StringReader(sw.toString()));
		assertEquals(foo2, foo3);
		
		ExtensionRegistry reg=ExtensionRegistry.newInstance();
		reg.add(TypesProtoBuf.alias);
		reg.add(TypesProtoBuf.langs);
		reg.add(TypesProtoBuf.version);
		reg.add(TypesProtoBuf.version1);
		//System.out.println(XmlFormat.printToString(foo1));
		//XmlFormat.merge(XmlFormat.printToString(foo1), Foo.newBuilder());
		foo3=(Foo) codec.toMessage(Foo.class, new StringReader(sw.toString()),reg);

		assertEquals(foo1, foo3);

		
	}
}
