package com.google.protobuf.codec.json;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import org.junit.Before;
import org.junit.Test;

import com.google.protobuf.codec.Codec;
import com.google.protobuf.codec.Codec.Feature;
import com.google.protobuf.codec.json.TypesProtoBuf.Types;


public class JsonWriteTest {

	private Types types;
	
	@Before
	public void setupTypes() {
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
				.setIduint64(100l).build();
	}
	
	@Test
	public void ensureTypes() throws IOException{
		Codec codec=new JsonCodec();
		codec.setFeature(Feature.CLOSE_STREAM, true);
		ByteArrayOutputStream bos=new ByteArrayOutputStream();
		codec.fromMessage(types, bos);
		System.out.println(new String(bos.toByteArray()));
		
	}
}
