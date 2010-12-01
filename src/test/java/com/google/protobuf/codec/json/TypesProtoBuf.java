// Generated by the protocol buffer compiler.  DO NOT EDIT!
// source: src/test/resources/user.proto

package com.google.protobuf.codec.json;

public final class TypesProtoBuf {
  private TypesProtoBuf() {}
  public static void registerAllExtensions(
      com.google.protobuf.ExtensionRegistry registry) {
  }
  public static final class Types extends
      com.google.protobuf.GeneratedMessage {
    // Use Types.newBuilder() to construct.
    private Types() {
      initFields();
    }
    private Types(boolean noInit) {}
    
    private static final Types defaultInstance;
    public static Types getDefaultInstance() {
      return defaultInstance;
    }
    
    public Types getDefaultInstanceForType() {
      return defaultInstance;
    }
    
    public static final com.google.protobuf.Descriptors.Descriptor
        getDescriptor() {
      return com.google.protobuf.codec.json.TypesProtoBuf.internal_static_Types_descriptor;
    }
    
    protected com.google.protobuf.GeneratedMessage.FieldAccessorTable
        internalGetFieldAccessorTable() {
      return com.google.protobuf.codec.json.TypesProtoBuf.internal_static_Types_fieldAccessorTable;
    }
    
    // optional string idstring = 1;
    public static final int IDSTRING_FIELD_NUMBER = 1;
    private boolean hasIdstring;
    private java.lang.String idstring_ = "";
    public boolean hasIdstring() { return hasIdstring; }
    public java.lang.String getIdstring() { return idstring_; }
    
    // optional int32 idint32 = 2;
    public static final int IDINT32_FIELD_NUMBER = 2;
    private boolean hasIdint32;
    private int idint32_ = 0;
    public boolean hasIdint32() { return hasIdint32; }
    public int getIdint32() { return idint32_; }
    
    // optional double iddouble = 3;
    public static final int IDDOUBLE_FIELD_NUMBER = 3;
    private boolean hasIddouble;
    private double iddouble_ = 0D;
    public boolean hasIddouble() { return hasIddouble; }
    public double getIddouble() { return iddouble_; }
    
    // optional float idfloat = 4;
    public static final int IDFLOAT_FIELD_NUMBER = 4;
    private boolean hasIdfloat;
    private float idfloat_ = 0F;
    public boolean hasIdfloat() { return hasIdfloat; }
    public float getIdfloat() { return idfloat_; }
    
    // optional int64 idint64 = 5;
    public static final int IDINT64_FIELD_NUMBER = 5;
    private boolean hasIdint64;
    private long idint64_ = 0L;
    public boolean hasIdint64() { return hasIdint64; }
    public long getIdint64() { return idint64_; }
    
    // optional uint32 iduint32 = 6;
    public static final int IDUINT32_FIELD_NUMBER = 6;
    private boolean hasIduint32;
    private int iduint32_ = 0;
    public boolean hasIduint32() { return hasIduint32; }
    public int getIduint32() { return iduint32_; }
    
    // optional uint64 iduint64 = 7;
    public static final int IDUINT64_FIELD_NUMBER = 7;
    private boolean hasIduint64;
    private long iduint64_ = 0L;
    public boolean hasIduint64() { return hasIduint64; }
    public long getIduint64() { return iduint64_; }
    
    // optional sint32 idsint32 = 8;
    public static final int IDSINT32_FIELD_NUMBER = 8;
    private boolean hasIdsint32;
    private int idsint32_ = 0;
    public boolean hasIdsint32() { return hasIdsint32; }
    public int getIdsint32() { return idsint32_; }
    
    // optional sint64 idsint64 = 9;
    public static final int IDSINT64_FIELD_NUMBER = 9;
    private boolean hasIdsint64;
    private long idsint64_ = 0L;
    public boolean hasIdsint64() { return hasIdsint64; }
    public long getIdsint64() { return idsint64_; }
    
    // optional fixed32 idfixed32 = 10;
    public static final int IDFIXED32_FIELD_NUMBER = 10;
    private boolean hasIdfixed32;
    private int idfixed32_ = 0;
    public boolean hasIdfixed32() { return hasIdfixed32; }
    public int getIdfixed32() { return idfixed32_; }
    
    // optional fixed64 idfixed64 = 11;
    public static final int IDFIXED64_FIELD_NUMBER = 11;
    private boolean hasIdfixed64;
    private long idfixed64_ = 0L;
    public boolean hasIdfixed64() { return hasIdfixed64; }
    public long getIdfixed64() { return idfixed64_; }
    
    // optional sfixed32 idsfixed32 = 12;
    public static final int IDSFIXED32_FIELD_NUMBER = 12;
    private boolean hasIdsfixed32;
    private int idsfixed32_ = 0;
    public boolean hasIdsfixed32() { return hasIdsfixed32; }
    public int getIdsfixed32() { return idsfixed32_; }
    
    // optional sfixed64 idsfixed64 = 13;
    public static final int IDSFIXED64_FIELD_NUMBER = 13;
    private boolean hasIdsfixed64;
    private long idsfixed64_ = 0L;
    public boolean hasIdsfixed64() { return hasIdsfixed64; }
    public long getIdsfixed64() { return idsfixed64_; }
    
    // optional bool idbool = 14;
    public static final int IDBOOL_FIELD_NUMBER = 14;
    private boolean hasIdbool;
    private boolean idbool_ = false;
    public boolean hasIdbool() { return hasIdbool; }
    public boolean getIdbool() { return idbool_; }
    
    private void initFields() {
    }
    public final boolean isInitialized() {
      return true;
    }
    
    public void writeTo(com.google.protobuf.CodedOutputStream output)
                        throws java.io.IOException {
      getSerializedSize();
      if (hasIdstring()) {
        output.writeString(1, getIdstring());
      }
      if (hasIdint32()) {
        output.writeInt32(2, getIdint32());
      }
      if (hasIddouble()) {
        output.writeDouble(3, getIddouble());
      }
      if (hasIdfloat()) {
        output.writeFloat(4, getIdfloat());
      }
      if (hasIdint64()) {
        output.writeInt64(5, getIdint64());
      }
      if (hasIduint32()) {
        output.writeUInt32(6, getIduint32());
      }
      if (hasIduint64()) {
        output.writeUInt64(7, getIduint64());
      }
      if (hasIdsint32()) {
        output.writeSInt32(8, getIdsint32());
      }
      if (hasIdsint64()) {
        output.writeSInt64(9, getIdsint64());
      }
      if (hasIdfixed32()) {
        output.writeFixed32(10, getIdfixed32());
      }
      if (hasIdfixed64()) {
        output.writeFixed64(11, getIdfixed64());
      }
      if (hasIdsfixed32()) {
        output.writeSFixed32(12, getIdsfixed32());
      }
      if (hasIdsfixed64()) {
        output.writeSFixed64(13, getIdsfixed64());
      }
      if (hasIdbool()) {
        output.writeBool(14, getIdbool());
      }
      getUnknownFields().writeTo(output);
    }
    
    private int memoizedSerializedSize = -1;
    public int getSerializedSize() {
      int size = memoizedSerializedSize;
      if (size != -1) return size;
    
      size = 0;
      if (hasIdstring()) {
        size += com.google.protobuf.CodedOutputStream
          .computeStringSize(1, getIdstring());
      }
      if (hasIdint32()) {
        size += com.google.protobuf.CodedOutputStream
          .computeInt32Size(2, getIdint32());
      }
      if (hasIddouble()) {
        size += com.google.protobuf.CodedOutputStream
          .computeDoubleSize(3, getIddouble());
      }
      if (hasIdfloat()) {
        size += com.google.protobuf.CodedOutputStream
          .computeFloatSize(4, getIdfloat());
      }
      if (hasIdint64()) {
        size += com.google.protobuf.CodedOutputStream
          .computeInt64Size(5, getIdint64());
      }
      if (hasIduint32()) {
        size += com.google.protobuf.CodedOutputStream
          .computeUInt32Size(6, getIduint32());
      }
      if (hasIduint64()) {
        size += com.google.protobuf.CodedOutputStream
          .computeUInt64Size(7, getIduint64());
      }
      if (hasIdsint32()) {
        size += com.google.protobuf.CodedOutputStream
          .computeSInt32Size(8, getIdsint32());
      }
      if (hasIdsint64()) {
        size += com.google.protobuf.CodedOutputStream
          .computeSInt64Size(9, getIdsint64());
      }
      if (hasIdfixed32()) {
        size += com.google.protobuf.CodedOutputStream
          .computeFixed32Size(10, getIdfixed32());
      }
      if (hasIdfixed64()) {
        size += com.google.protobuf.CodedOutputStream
          .computeFixed64Size(11, getIdfixed64());
      }
      if (hasIdsfixed32()) {
        size += com.google.protobuf.CodedOutputStream
          .computeSFixed32Size(12, getIdsfixed32());
      }
      if (hasIdsfixed64()) {
        size += com.google.protobuf.CodedOutputStream
          .computeSFixed64Size(13, getIdsfixed64());
      }
      if (hasIdbool()) {
        size += com.google.protobuf.CodedOutputStream
          .computeBoolSize(14, getIdbool());
      }
      size += getUnknownFields().getSerializedSize();
      memoizedSerializedSize = size;
      return size;
    }
    
    public static com.google.protobuf.codec.json.TypesProtoBuf.Types parseFrom(
        com.google.protobuf.ByteString data)
        throws com.google.protobuf.InvalidProtocolBufferException {
      return newBuilder().mergeFrom(data).buildParsed();
    }
    public static com.google.protobuf.codec.json.TypesProtoBuf.Types parseFrom(
        com.google.protobuf.ByteString data,
        com.google.protobuf.ExtensionRegistryLite extensionRegistry)
        throws com.google.protobuf.InvalidProtocolBufferException {
      return newBuilder().mergeFrom(data, extensionRegistry)
               .buildParsed();
    }
    public static com.google.protobuf.codec.json.TypesProtoBuf.Types parseFrom(byte[] data)
        throws com.google.protobuf.InvalidProtocolBufferException {
      return newBuilder().mergeFrom(data).buildParsed();
    }
    public static com.google.protobuf.codec.json.TypesProtoBuf.Types parseFrom(
        byte[] data,
        com.google.protobuf.ExtensionRegistryLite extensionRegistry)
        throws com.google.protobuf.InvalidProtocolBufferException {
      return newBuilder().mergeFrom(data, extensionRegistry)
               .buildParsed();
    }
    public static com.google.protobuf.codec.json.TypesProtoBuf.Types parseFrom(java.io.InputStream input)
        throws java.io.IOException {
      return newBuilder().mergeFrom(input).buildParsed();
    }
    public static com.google.protobuf.codec.json.TypesProtoBuf.Types parseFrom(
        java.io.InputStream input,
        com.google.protobuf.ExtensionRegistryLite extensionRegistry)
        throws java.io.IOException {
      return newBuilder().mergeFrom(input, extensionRegistry)
               .buildParsed();
    }
    public static com.google.protobuf.codec.json.TypesProtoBuf.Types parseDelimitedFrom(java.io.InputStream input)
        throws java.io.IOException {
      Builder builder = newBuilder();
      if (builder.mergeDelimitedFrom(input)) {
        return builder.buildParsed();
      } else {
        return null;
      }
    }
    public static com.google.protobuf.codec.json.TypesProtoBuf.Types parseDelimitedFrom(
        java.io.InputStream input,
        com.google.protobuf.ExtensionRegistryLite extensionRegistry)
        throws java.io.IOException {
      Builder builder = newBuilder();
      if (builder.mergeDelimitedFrom(input, extensionRegistry)) {
        return builder.buildParsed();
      } else {
        return null;
      }
    }
    public static com.google.protobuf.codec.json.TypesProtoBuf.Types parseFrom(
        com.google.protobuf.CodedInputStream input)
        throws java.io.IOException {
      return newBuilder().mergeFrom(input).buildParsed();
    }
    public static com.google.protobuf.codec.json.TypesProtoBuf.Types parseFrom(
        com.google.protobuf.CodedInputStream input,
        com.google.protobuf.ExtensionRegistryLite extensionRegistry)
        throws java.io.IOException {
      return newBuilder().mergeFrom(input, extensionRegistry)
               .buildParsed();
    }
    
    public static Builder newBuilder() { return Builder.create(); }
    public Builder newBuilderForType() { return newBuilder(); }
    public static Builder newBuilder(com.google.protobuf.codec.json.TypesProtoBuf.Types prototype) {
      return newBuilder().mergeFrom(prototype);
    }
    public Builder toBuilder() { return newBuilder(this); }
    
    public static final class Builder extends
        com.google.protobuf.GeneratedMessage.Builder<Builder> {
      private com.google.protobuf.codec.json.TypesProtoBuf.Types result;
      
      // Construct using com.google.protobuf.codec.json.TypesProtoBuf.Types.newBuilder()
      private Builder() {}
      
      private static Builder create() {
        Builder builder = new Builder();
        builder.result = new com.google.protobuf.codec.json.TypesProtoBuf.Types();
        return builder;
      }
      
      protected com.google.protobuf.codec.json.TypesProtoBuf.Types internalGetResult() {
        return result;
      }
      
      public Builder clear() {
        if (result == null) {
          throw new IllegalStateException(
            "Cannot call clear() after build().");
        }
        result = new com.google.protobuf.codec.json.TypesProtoBuf.Types();
        return this;
      }
      
      public Builder clone() {
        return create().mergeFrom(result);
      }
      
      public com.google.protobuf.Descriptors.Descriptor
          getDescriptorForType() {
        return com.google.protobuf.codec.json.TypesProtoBuf.Types.getDescriptor();
      }
      
      public com.google.protobuf.codec.json.TypesProtoBuf.Types getDefaultInstanceForType() {
        return com.google.protobuf.codec.json.TypesProtoBuf.Types.getDefaultInstance();
      }
      
      public boolean isInitialized() {
        return result.isInitialized();
      }
      public com.google.protobuf.codec.json.TypesProtoBuf.Types build() {
        if (result != null && !isInitialized()) {
          throw newUninitializedMessageException(result);
        }
        return buildPartial();
      }
      
      private com.google.protobuf.codec.json.TypesProtoBuf.Types buildParsed()
          throws com.google.protobuf.InvalidProtocolBufferException {
        if (!isInitialized()) {
          throw newUninitializedMessageException(
            result).asInvalidProtocolBufferException();
        }
        return buildPartial();
      }
      
      public com.google.protobuf.codec.json.TypesProtoBuf.Types buildPartial() {
        if (result == null) {
          throw new IllegalStateException(
            "build() has already been called on this Builder.");
        }
        com.google.protobuf.codec.json.TypesProtoBuf.Types returnMe = result;
        result = null;
        return returnMe;
      }
      
      public Builder mergeFrom(com.google.protobuf.Message other) {
        if (other instanceof com.google.protobuf.codec.json.TypesProtoBuf.Types) {
          return mergeFrom((com.google.protobuf.codec.json.TypesProtoBuf.Types)other);
        } else {
          super.mergeFrom(other);
          return this;
        }
      }
      
      public Builder mergeFrom(com.google.protobuf.codec.json.TypesProtoBuf.Types other) {
        if (other == com.google.protobuf.codec.json.TypesProtoBuf.Types.getDefaultInstance()) return this;
        if (other.hasIdstring()) {
          setIdstring(other.getIdstring());
        }
        if (other.hasIdint32()) {
          setIdint32(other.getIdint32());
        }
        if (other.hasIddouble()) {
          setIddouble(other.getIddouble());
        }
        if (other.hasIdfloat()) {
          setIdfloat(other.getIdfloat());
        }
        if (other.hasIdint64()) {
          setIdint64(other.getIdint64());
        }
        if (other.hasIduint32()) {
          setIduint32(other.getIduint32());
        }
        if (other.hasIduint64()) {
          setIduint64(other.getIduint64());
        }
        if (other.hasIdsint32()) {
          setIdsint32(other.getIdsint32());
        }
        if (other.hasIdsint64()) {
          setIdsint64(other.getIdsint64());
        }
        if (other.hasIdfixed32()) {
          setIdfixed32(other.getIdfixed32());
        }
        if (other.hasIdfixed64()) {
          setIdfixed64(other.getIdfixed64());
        }
        if (other.hasIdsfixed32()) {
          setIdsfixed32(other.getIdsfixed32());
        }
        if (other.hasIdsfixed64()) {
          setIdsfixed64(other.getIdsfixed64());
        }
        if (other.hasIdbool()) {
          setIdbool(other.getIdbool());
        }
        this.mergeUnknownFields(other.getUnknownFields());
        return this;
      }
      
      public Builder mergeFrom(
          com.google.protobuf.CodedInputStream input,
          com.google.protobuf.ExtensionRegistryLite extensionRegistry)
          throws java.io.IOException {
        com.google.protobuf.UnknownFieldSet.Builder unknownFields =
          com.google.protobuf.UnknownFieldSet.newBuilder(
            this.getUnknownFields());
        while (true) {
          int tag = input.readTag();
          switch (tag) {
            case 0:
              this.setUnknownFields(unknownFields.build());
              return this;
            default: {
              if (!parseUnknownField(input, unknownFields,
                                     extensionRegistry, tag)) {
                this.setUnknownFields(unknownFields.build());
                return this;
              }
              break;
            }
            case 10: {
              setIdstring(input.readString());
              break;
            }
            case 16: {
              setIdint32(input.readInt32());
              break;
            }
            case 25: {
              setIddouble(input.readDouble());
              break;
            }
            case 37: {
              setIdfloat(input.readFloat());
              break;
            }
            case 40: {
              setIdint64(input.readInt64());
              break;
            }
            case 48: {
              setIduint32(input.readUInt32());
              break;
            }
            case 56: {
              setIduint64(input.readUInt64());
              break;
            }
            case 64: {
              setIdsint32(input.readSInt32());
              break;
            }
            case 72: {
              setIdsint64(input.readSInt64());
              break;
            }
            case 85: {
              setIdfixed32(input.readFixed32());
              break;
            }
            case 89: {
              setIdfixed64(input.readFixed64());
              break;
            }
            case 101: {
              setIdsfixed32(input.readSFixed32());
              break;
            }
            case 105: {
              setIdsfixed64(input.readSFixed64());
              break;
            }
            case 112: {
              setIdbool(input.readBool());
              break;
            }
          }
        }
      }
      
      
      // optional string idstring = 1;
      public boolean hasIdstring() {
        return result.hasIdstring();
      }
      public java.lang.String getIdstring() {
        return result.getIdstring();
      }
      public Builder setIdstring(java.lang.String value) {
        if (value == null) {
    throw new NullPointerException();
  }
  result.hasIdstring = true;
        result.idstring_ = value;
        return this;
      }
      public Builder clearIdstring() {
        result.hasIdstring = false;
        result.idstring_ = getDefaultInstance().getIdstring();
        return this;
      }
      
      // optional int32 idint32 = 2;
      public boolean hasIdint32() {
        return result.hasIdint32();
      }
      public int getIdint32() {
        return result.getIdint32();
      }
      public Builder setIdint32(int value) {
        result.hasIdint32 = true;
        result.idint32_ = value;
        return this;
      }
      public Builder clearIdint32() {
        result.hasIdint32 = false;
        result.idint32_ = 0;
        return this;
      }
      
      // optional double iddouble = 3;
      public boolean hasIddouble() {
        return result.hasIddouble();
      }
      public double getIddouble() {
        return result.getIddouble();
      }
      public Builder setIddouble(double value) {
        result.hasIddouble = true;
        result.iddouble_ = value;
        return this;
      }
      public Builder clearIddouble() {
        result.hasIddouble = false;
        result.iddouble_ = 0D;
        return this;
      }
      
      // optional float idfloat = 4;
      public boolean hasIdfloat() {
        return result.hasIdfloat();
      }
      public float getIdfloat() {
        return result.getIdfloat();
      }
      public Builder setIdfloat(float value) {
        result.hasIdfloat = true;
        result.idfloat_ = value;
        return this;
      }
      public Builder clearIdfloat() {
        result.hasIdfloat = false;
        result.idfloat_ = 0F;
        return this;
      }
      
      // optional int64 idint64 = 5;
      public boolean hasIdint64() {
        return result.hasIdint64();
      }
      public long getIdint64() {
        return result.getIdint64();
      }
      public Builder setIdint64(long value) {
        result.hasIdint64 = true;
        result.idint64_ = value;
        return this;
      }
      public Builder clearIdint64() {
        result.hasIdint64 = false;
        result.idint64_ = 0L;
        return this;
      }
      
      // optional uint32 iduint32 = 6;
      public boolean hasIduint32() {
        return result.hasIduint32();
      }
      public int getIduint32() {
        return result.getIduint32();
      }
      public Builder setIduint32(int value) {
        result.hasIduint32 = true;
        result.iduint32_ = value;
        return this;
      }
      public Builder clearIduint32() {
        result.hasIduint32 = false;
        result.iduint32_ = 0;
        return this;
      }
      
      // optional uint64 iduint64 = 7;
      public boolean hasIduint64() {
        return result.hasIduint64();
      }
      public long getIduint64() {
        return result.getIduint64();
      }
      public Builder setIduint64(long value) {
        result.hasIduint64 = true;
        result.iduint64_ = value;
        return this;
      }
      public Builder clearIduint64() {
        result.hasIduint64 = false;
        result.iduint64_ = 0L;
        return this;
      }
      
      // optional sint32 idsint32 = 8;
      public boolean hasIdsint32() {
        return result.hasIdsint32();
      }
      public int getIdsint32() {
        return result.getIdsint32();
      }
      public Builder setIdsint32(int value) {
        result.hasIdsint32 = true;
        result.idsint32_ = value;
        return this;
      }
      public Builder clearIdsint32() {
        result.hasIdsint32 = false;
        result.idsint32_ = 0;
        return this;
      }
      
      // optional sint64 idsint64 = 9;
      public boolean hasIdsint64() {
        return result.hasIdsint64();
      }
      public long getIdsint64() {
        return result.getIdsint64();
      }
      public Builder setIdsint64(long value) {
        result.hasIdsint64 = true;
        result.idsint64_ = value;
        return this;
      }
      public Builder clearIdsint64() {
        result.hasIdsint64 = false;
        result.idsint64_ = 0L;
        return this;
      }
      
      // optional fixed32 idfixed32 = 10;
      public boolean hasIdfixed32() {
        return result.hasIdfixed32();
      }
      public int getIdfixed32() {
        return result.getIdfixed32();
      }
      public Builder setIdfixed32(int value) {
        result.hasIdfixed32 = true;
        result.idfixed32_ = value;
        return this;
      }
      public Builder clearIdfixed32() {
        result.hasIdfixed32 = false;
        result.idfixed32_ = 0;
        return this;
      }
      
      // optional fixed64 idfixed64 = 11;
      public boolean hasIdfixed64() {
        return result.hasIdfixed64();
      }
      public long getIdfixed64() {
        return result.getIdfixed64();
      }
      public Builder setIdfixed64(long value) {
        result.hasIdfixed64 = true;
        result.idfixed64_ = value;
        return this;
      }
      public Builder clearIdfixed64() {
        result.hasIdfixed64 = false;
        result.idfixed64_ = 0L;
        return this;
      }
      
      // optional sfixed32 idsfixed32 = 12;
      public boolean hasIdsfixed32() {
        return result.hasIdsfixed32();
      }
      public int getIdsfixed32() {
        return result.getIdsfixed32();
      }
      public Builder setIdsfixed32(int value) {
        result.hasIdsfixed32 = true;
        result.idsfixed32_ = value;
        return this;
      }
      public Builder clearIdsfixed32() {
        result.hasIdsfixed32 = false;
        result.idsfixed32_ = 0;
        return this;
      }
      
      // optional sfixed64 idsfixed64 = 13;
      public boolean hasIdsfixed64() {
        return result.hasIdsfixed64();
      }
      public long getIdsfixed64() {
        return result.getIdsfixed64();
      }
      public Builder setIdsfixed64(long value) {
        result.hasIdsfixed64 = true;
        result.idsfixed64_ = value;
        return this;
      }
      public Builder clearIdsfixed64() {
        result.hasIdsfixed64 = false;
        result.idsfixed64_ = 0L;
        return this;
      }
      
      // optional bool idbool = 14;
      public boolean hasIdbool() {
        return result.hasIdbool();
      }
      public boolean getIdbool() {
        return result.getIdbool();
      }
      public Builder setIdbool(boolean value) {
        result.hasIdbool = true;
        result.idbool_ = value;
        return this;
      }
      public Builder clearIdbool() {
        result.hasIdbool = false;
        result.idbool_ = false;
        return this;
      }
      
      // @@protoc_insertion_point(builder_scope:Types)
    }
    
    static {
      defaultInstance = new Types(true);
      com.google.protobuf.codec.json.TypesProtoBuf.internalForceInit();
      defaultInstance.initFields();
    }
    
    // @@protoc_insertion_point(class_scope:Types)
  }
  
  private static com.google.protobuf.Descriptors.Descriptor
    internal_static_Types_descriptor;
  private static
    com.google.protobuf.GeneratedMessage.FieldAccessorTable
      internal_static_Types_fieldAccessorTable;
  
  public static com.google.protobuf.Descriptors.FileDescriptor
      getDescriptor() {
    return descriptor;
  }
  private static com.google.protobuf.Descriptors.FileDescriptor
      descriptor;
  static {
    java.lang.String[] descriptorData = {
      "\n\035src/test/resources/user.proto\"\204\002\n\005Type" +
      "s\022\020\n\010idstring\030\001 \001(\t\022\017\n\007idint32\030\002 \001(\005\022\020\n\010" +
      "iddouble\030\003 \001(\001\022\017\n\007idfloat\030\004 \001(\002\022\017\n\007idint" +
      "64\030\005 \001(\003\022\020\n\010iduint32\030\006 \001(\r\022\020\n\010iduint64\030\007" +
      " \001(\004\022\020\n\010idsint32\030\010 \001(\021\022\020\n\010idsint64\030\t \001(\022" +
      "\022\021\n\tidfixed32\030\n \001(\007\022\021\n\tidfixed64\030\013 \001(\006\022\022" +
      "\n\nidsfixed32\030\014 \001(\017\022\022\n\nidsfixed64\030\r \001(\020\022\016" +
      "\n\006idbool\030\016 \001(\010B/\n\036com.google.protobuf.co" +
      "dec.jsonB\rTypesProtoBuf"
    };
    com.google.protobuf.Descriptors.FileDescriptor.InternalDescriptorAssigner assigner =
      new com.google.protobuf.Descriptors.FileDescriptor.InternalDescriptorAssigner() {
        public com.google.protobuf.ExtensionRegistry assignDescriptors(
            com.google.protobuf.Descriptors.FileDescriptor root) {
          descriptor = root;
          internal_static_Types_descriptor =
            getDescriptor().getMessageTypes().get(0);
          internal_static_Types_fieldAccessorTable = new
            com.google.protobuf.GeneratedMessage.FieldAccessorTable(
              internal_static_Types_descriptor,
              new java.lang.String[] { "Idstring", "Idint32", "Iddouble", "Idfloat", "Idint64", "Iduint32", "Iduint64", "Idsint32", "Idsint64", "Idfixed32", "Idfixed64", "Idsfixed32", "Idsfixed64", "Idbool", },
              com.google.protobuf.codec.json.TypesProtoBuf.Types.class,
              com.google.protobuf.codec.json.TypesProtoBuf.Types.Builder.class);
          return null;
        }
      };
    com.google.protobuf.Descriptors.FileDescriptor
      .internalBuildGeneratedFileFrom(descriptorData,
        new com.google.protobuf.Descriptors.FileDescriptor[] {
        }, assigner);
  }
  
  public static void internalForceInit() {}
  
  // @@protoc_insertion_point(outer_class_scope)
}
