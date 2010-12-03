package com.google.protobuf.codec.json;

import java.io.IOException;
import java.io.StringWriter;

import org.junit.Test;

import com.google.protobuf.codec.json.TypesProtoBuf.Lang;
import com.google.protobuf.codec.json.TypesProtoBuf.RepeatedFields;

/**
 * Test cases for the features that the base class offers
 * @author sijuv
 *
 */
public class AbstractCodecTest {
	
	@Test(expected=IllegalArgumentException.class)
	public void ensureSafelyInitialized() throws IOException{
		RepeatedFields repFields=RepeatedFields.newBuilder()
		.addId(1)
		.addId(2)
		.addLangs(Lang.HASKELL)
		.addLangs(Lang.JAVA).buildPartial();
		new JsonCodec().fromMessage(repFields, new StringWriter());
		
	}
	
}
