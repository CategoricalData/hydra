package hydra.ext.protobuf.type;

/**
 * A single field of a message type.
 */
public abstract class Field {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/protobuf/type.Field");
  
  private Field () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Kind instance) ;
    
    R visit(Cardinality instance) ;
    
    R visit(Number_ instance) ;
    
    R visit(Name instance) ;
    
    R visit(TypeUrl instance) ;
    
    R visit(OneofIndex instance) ;
    
    R visit(Packed instance) ;
    
    R visit(Options instance) ;
    
    R visit(JsonName instance) ;
    
    R visit(DefaultValue instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Field instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Kind instance) {
      return otherwise((instance));
    }
    
    default R visit(Cardinality instance) {
      return otherwise((instance));
    }
    
    default R visit(Number_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Name instance) {
      return otherwise((instance));
    }
    
    default R visit(TypeUrl instance) {
      return otherwise((instance));
    }
    
    default R visit(OneofIndex instance) {
      return otherwise((instance));
    }
    
    default R visit(Packed instance) {
      return otherwise((instance));
    }
    
    default R visit(Options instance) {
      return otherwise((instance));
    }
    
    default R visit(JsonName instance) {
      return otherwise((instance));
    }
    
    default R visit(DefaultValue instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * The field type.
   */
  public static final class Kind extends hydra.ext.protobuf.type.Field {
    /**
     * The field type.
     */
    public final hydra.ext.protobuf.type.Kind value;
    
    public Kind (hydra.ext.protobuf.type.Kind value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Kind)) {
        return false;
      }
      Kind o = (Kind) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * The field cardinality.
   */
  public static final class Cardinality extends hydra.ext.protobuf.type.Field {
    /**
     * The field cardinality.
     */
    public final hydra.ext.protobuf.type.Cardinality value;
    
    public Cardinality (hydra.ext.protobuf.type.Cardinality value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Cardinality)) {
        return false;
      }
      Cardinality o = (Cardinality) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * The field number.
   */
  public static final class Number_ extends hydra.ext.protobuf.type.Field {
    /**
     * The field number.
     */
    public final Integer value;
    
    public Number_ (Integer value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Number_)) {
        return false;
      }
      Number_ o = (Number_) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * The field name.
   */
  public static final class Name extends hydra.ext.protobuf.type.Field {
    /**
     * The field name.
     */
    public final String value;
    
    public Name (String value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Name)) {
        return false;
      }
      Name o = (Name) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * The field type URL, without the scheme, for message or enumeration types. Example: `"type.googleapis.com/google.protobuf.Timestamp"`.
   */
  public static final class TypeUrl extends hydra.ext.protobuf.type.Field {
    /**
     * The field type URL, without the scheme, for message or enumeration types. Example: `"type.googleapis.com/google.protobuf.Timestamp"`.
     */
    public final String value;
    
    public TypeUrl (String value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeUrl)) {
        return false;
      }
      TypeUrl o = (TypeUrl) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * The index of the field type in `Type.oneofs`, for message or enumeration types. The first type has index 1; zero means the type is not in the list.
   */
  public static final class OneofIndex extends hydra.ext.protobuf.type.Field {
    /**
     * The index of the field type in `Type.oneofs`, for message or enumeration types. The first type has index 1; zero means the type is not in the list.
     */
    public final Integer value;
    
    public OneofIndex (Integer value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OneofIndex)) {
        return false;
      }
      OneofIndex o = (OneofIndex) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Whether to use alternative packed wire representation.
   */
  public static final class Packed extends hydra.ext.protobuf.type.Field {
    /**
     * Whether to use alternative packed wire representation.
     */
    public final Boolean value;
    
    public Packed (Boolean value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Packed)) {
        return false;
      }
      Packed o = (Packed) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * The protocol buffer options.
   */
  public static final class Options extends hydra.ext.protobuf.type.Field {
    /**
     * The protocol buffer options.
     */
    public final java.util.List<hydra.ext.protobuf.type.Option> value;
    
    public Options (java.util.List<hydra.ext.protobuf.type.Option> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Options)) {
        return false;
      }
      Options o = (Options) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * The field JSON name.
   */
  public static final class JsonName extends hydra.ext.protobuf.type.Field {
    /**
     * The field JSON name.
     */
    public final String value;
    
    public JsonName (String value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof JsonName)) {
        return false;
      }
      JsonName o = (JsonName) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * The string value of the default value of this field. Proto2 syntax only.
   */
  public static final class DefaultValue extends hydra.ext.protobuf.type.Field {
    /**
     * The string value of the default value of this field. Proto2 syntax only.
     */
    public final String value;
    
    public DefaultValue (String value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DefaultValue)) {
        return false;
      }
      DefaultValue o = (DefaultValue) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
