package hydra.ext.protobuf.type;

/**
 * Basic field types.
 */
public abstract class Kind {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/protobuf/type.Kind");
  
  private Kind () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Unknown instance) ;
    
    R visit(Double_ instance) ;
    
    R visit(Float_ instance) ;
    
    R visit(Int64 instance) ;
    
    R visit(Uint64 instance) ;
    
    R visit(Int32 instance) ;
    
    R visit(Fixed64 instance) ;
    
    R visit(Fixed32 instance) ;
    
    R visit(Bool instance) ;
    
    R visit(String_ instance) ;
    
    R visit(Group instance) ;
    
    R visit(Message instance) ;
    
    R visit(Bytes instance) ;
    
    R visit(Uint32 instance) ;
    
    R visit(Enum_ instance) ;
    
    R visit(Sfixed32 instance) ;
    
    R visit(Sfixed64 instance) ;
    
    R visit(Sint32 instance) ;
    
    R visit(Sint64 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Kind instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Unknown instance) {
      return otherwise((instance));
    }
    
    default R visit(Double_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Float_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Int64 instance) {
      return otherwise((instance));
    }
    
    default R visit(Uint64 instance) {
      return otherwise((instance));
    }
    
    default R visit(Int32 instance) {
      return otherwise((instance));
    }
    
    default R visit(Fixed64 instance) {
      return otherwise((instance));
    }
    
    default R visit(Fixed32 instance) {
      return otherwise((instance));
    }
    
    default R visit(Bool instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Group instance) {
      return otherwise((instance));
    }
    
    default R visit(Message instance) {
      return otherwise((instance));
    }
    
    default R visit(Bytes instance) {
      return otherwise((instance));
    }
    
    default R visit(Uint32 instance) {
      return otherwise((instance));
    }
    
    default R visit(Enum_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Sfixed32 instance) {
      return otherwise((instance));
    }
    
    default R visit(Sfixed64 instance) {
      return otherwise((instance));
    }
    
    default R visit(Sint32 instance) {
      return otherwise((instance));
    }
    
    default R visit(Sint64 instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * Field type unknown.
   */
  public static final class Unknown extends hydra.ext.protobuf.type.Kind {
    public Unknown () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unknown)) {
        return false;
      }
      Unknown o = (Unknown) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Field type double.
   */
  public static final class Double_ extends hydra.ext.protobuf.type.Kind {
    public Double_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Double_)) {
        return false;
      }
      Double_ o = (Double_) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Field type float.
   */
  public static final class Float_ extends hydra.ext.protobuf.type.Kind {
    public Float_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Float_)) {
        return false;
      }
      Float_ o = (Float_) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Field type int64.
   */
  public static final class Int64 extends hydra.ext.protobuf.type.Kind {
    public Int64 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int64)) {
        return false;
      }
      Int64 o = (Int64) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Field type uint64.
   */
  public static final class Uint64 extends hydra.ext.protobuf.type.Kind {
    public Uint64 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint64)) {
        return false;
      }
      Uint64 o = (Uint64) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Field type int32.
   */
  public static final class Int32 extends hydra.ext.protobuf.type.Kind {
    public Int32 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int32)) {
        return false;
      }
      Int32 o = (Int32) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Field type fixed64.
   */
  public static final class Fixed64 extends hydra.ext.protobuf.type.Kind {
    public Fixed64 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fixed64)) {
        return false;
      }
      Fixed64 o = (Fixed64) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Field type fixed32.
   */
  public static final class Fixed32 extends hydra.ext.protobuf.type.Kind {
    public Fixed32 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fixed32)) {
        return false;
      }
      Fixed32 o = (Fixed32) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Field type bool.
   */
  public static final class Bool extends hydra.ext.protobuf.type.Kind {
    public Bool () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bool)) {
        return false;
      }
      Bool o = (Bool) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Field type string.
   */
  public static final class String_ extends hydra.ext.protobuf.type.Kind {
    public String_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Field type group. Proto2 syntax only, and deprecated.
   */
  public static final class Group extends hydra.ext.protobuf.type.Kind {
    public Group () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Group)) {
        return false;
      }
      Group o = (Group) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Field type message.
   */
  public static final class Message extends hydra.ext.protobuf.type.Kind {
    public Message () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Message)) {
        return false;
      }
      Message o = (Message) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Field type bytes.
   */
  public static final class Bytes extends hydra.ext.protobuf.type.Kind {
    public Bytes () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bytes)) {
        return false;
      }
      Bytes o = (Bytes) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Field type uint32.
   */
  public static final class Uint32 extends hydra.ext.protobuf.type.Kind {
    public Uint32 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint32)) {
        return false;
      }
      Uint32 o = (Uint32) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Field type enum.
   */
  public static final class Enum_ extends hydra.ext.protobuf.type.Kind {
    public Enum_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Enum_)) {
        return false;
      }
      Enum_ o = (Enum_) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Field type sfixed32.
   */
  public static final class Sfixed32 extends hydra.ext.protobuf.type.Kind {
    public Sfixed32 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sfixed32)) {
        return false;
      }
      Sfixed32 o = (Sfixed32) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Field type sfixed64.
   */
  public static final class Sfixed64 extends hydra.ext.protobuf.type.Kind {
    public Sfixed64 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sfixed64)) {
        return false;
      }
      Sfixed64 o = (Sfixed64) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Field type sint32.
   */
  public static final class Sint32 extends hydra.ext.protobuf.type.Kind {
    public Sint32 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sint32)) {
        return false;
      }
      Sint32 o = (Sint32) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Field type sint64.
   */
  public static final class Sint64 extends hydra.ext.protobuf.type.Kind {
    public Sint64 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sint64)) {
        return false;
      }
      Sint64 o = (Sint64) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
