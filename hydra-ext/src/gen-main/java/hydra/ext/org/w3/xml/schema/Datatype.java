// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.xml.schema;

import java.io.Serializable;

public abstract class Datatype implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.w3.xml.schema.Datatype");
  
  public static final hydra.core.Name FIELD_NAME_ANY_SIMPLE_TYPE = new hydra.core.Name("anySimpleType");
  
  public static final hydra.core.Name FIELD_NAME_ANY_TYPE = new hydra.core.Name("anyType");
  
  public static final hydra.core.Name FIELD_NAME_ANY_U_R_I = new hydra.core.Name("anyURI");
  
  public static final hydra.core.Name FIELD_NAME_BASE64_BINARY = new hydra.core.Name("base64Binary");
  
  public static final hydra.core.Name FIELD_NAME_BOOLEAN = new hydra.core.Name("boolean");
  
  public static final hydra.core.Name FIELD_NAME_BYTE = new hydra.core.Name("byte");
  
  public static final hydra.core.Name FIELD_NAME_DATE = new hydra.core.Name("date");
  
  public static final hydra.core.Name FIELD_NAME_DATE_TIME = new hydra.core.Name("dateTime");
  
  public static final hydra.core.Name FIELD_NAME_DECIMAL = new hydra.core.Name("decimal");
  
  public static final hydra.core.Name FIELD_NAME_DOUBLE = new hydra.core.Name("double");
  
  public static final hydra.core.Name FIELD_NAME_DURATION = new hydra.core.Name("duration");
  
  public static final hydra.core.Name FIELD_NAME_E_N_T_I_T_I_E_S = new hydra.core.Name("ENTITIES");
  
  public static final hydra.core.Name FIELD_NAME_E_N_T_I_T_Y = new hydra.core.Name("ENTITY");
  
  public static final hydra.core.Name FIELD_NAME_FLOAT = new hydra.core.Name("float");
  
  public static final hydra.core.Name FIELD_NAME_G_DAY = new hydra.core.Name("gDay");
  
  public static final hydra.core.Name FIELD_NAME_G_MONTH = new hydra.core.Name("gMonth");
  
  public static final hydra.core.Name FIELD_NAME_G_MONTH_DAY = new hydra.core.Name("gMonthDay");
  
  public static final hydra.core.Name FIELD_NAME_G_YEAR = new hydra.core.Name("gYear");
  
  public static final hydra.core.Name FIELD_NAME_G_YEAR_MONTH = new hydra.core.Name("gYearMonth");
  
  public static final hydra.core.Name FIELD_NAME_HEX_BINARY = new hydra.core.Name("hexBinary");
  
  public static final hydra.core.Name FIELD_NAME_I_D = new hydra.core.Name("ID");
  
  public static final hydra.core.Name FIELD_NAME_I_D_R_E_F = new hydra.core.Name("IDREF");
  
  public static final hydra.core.Name FIELD_NAME_I_D_R_E_F_S = new hydra.core.Name("IDREFS");
  
  public static final hydra.core.Name FIELD_NAME_INT = new hydra.core.Name("int");
  
  public static final hydra.core.Name FIELD_NAME_INTEGER = new hydra.core.Name("integer");
  
  public static final hydra.core.Name FIELD_NAME_LANGUAGE = new hydra.core.Name("language");
  
  public static final hydra.core.Name FIELD_NAME_LONG = new hydra.core.Name("long");
  
  public static final hydra.core.Name FIELD_NAME_N_M_T_O_K_E_N = new hydra.core.Name("NMTOKEN");
  
  public static final hydra.core.Name FIELD_NAME_N_O_T_A_T_I_O_N = new hydra.core.Name("NOTATION");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_NEGATIVE_INTEGER = new hydra.core.Name("negativeInteger");
  
  public static final hydra.core.Name FIELD_NAME_NON_NEGATIVE_INTEGER = new hydra.core.Name("nonNegativeInteger");
  
  public static final hydra.core.Name FIELD_NAME_NON_POSITIVE_INTEGER = new hydra.core.Name("nonPositiveInteger");
  
  public static final hydra.core.Name FIELD_NAME_NORMALIZED_STRING = new hydra.core.Name("normalizedString");
  
  public static final hydra.core.Name FIELD_NAME_POSITIVE_INTEGER = new hydra.core.Name("positiveInteger");
  
  public static final hydra.core.Name FIELD_NAME_Q_NAME = new hydra.core.Name("qName");
  
  public static final hydra.core.Name FIELD_NAME_SHORT = new hydra.core.Name("short");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name FIELD_NAME_TIME = new hydra.core.Name("time");
  
  public static final hydra.core.Name FIELD_NAME_TOKEN = new hydra.core.Name("token");
  
  public static final hydra.core.Name FIELD_NAME_UNSIGNED_BYTE = new hydra.core.Name("unsignedByte");
  
  public static final hydra.core.Name FIELD_NAME_UNSIGNED_INT = new hydra.core.Name("unsignedInt");
  
  public static final hydra.core.Name FIELD_NAME_UNSIGNED_LONG = new hydra.core.Name("unsignedLong");
  
  public static final hydra.core.Name FIELD_NAME_UNSIGNED_SHORT = new hydra.core.Name("unsignedShort");
  
  private Datatype () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(AnySimpleType instance) ;
    
    R visit(AnyType instance) ;
    
    R visit(AnyURI instance) ;
    
    R visit(Base64Binary instance) ;
    
    R visit(Boolean_ instance) ;
    
    R visit(Byte_ instance) ;
    
    R visit(Date instance) ;
    
    R visit(DateTime instance) ;
    
    R visit(Decimal instance) ;
    
    R visit(Double_ instance) ;
    
    R visit(Duration instance) ;
    
    R visit(ENTITIES instance) ;
    
    R visit(ENTITY instance) ;
    
    R visit(Float_ instance) ;
    
    R visit(GDay instance) ;
    
    R visit(GMonth instance) ;
    
    R visit(GMonthDay instance) ;
    
    R visit(GYear instance) ;
    
    R visit(GYearMonth instance) ;
    
    R visit(HexBinary instance) ;
    
    R visit(ID instance) ;
    
    R visit(IDREF instance) ;
    
    R visit(IDREFS instance) ;
    
    R visit(Int instance) ;
    
    R visit(Integer_ instance) ;
    
    R visit(Language instance) ;
    
    R visit(Long_ instance) ;
    
    R visit(NMTOKEN instance) ;
    
    R visit(NOTATION instance) ;
    
    R visit(Name instance) ;
    
    R visit(NegativeInteger instance) ;
    
    R visit(NonNegativeInteger instance) ;
    
    R visit(NonPositiveInteger instance) ;
    
    R visit(NormalizedString instance) ;
    
    R visit(PositiveInteger instance) ;
    
    R visit(QName instance) ;
    
    R visit(Short_ instance) ;
    
    R visit(String_ instance) ;
    
    R visit(Time instance) ;
    
    R visit(Token instance) ;
    
    R visit(UnsignedByte instance) ;
    
    R visit(UnsignedInt instance) ;
    
    R visit(UnsignedLong instance) ;
    
    R visit(UnsignedShort instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Datatype instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(AnySimpleType instance) {
      return otherwise((instance));
    }
    
    default R visit(AnyType instance) {
      return otherwise((instance));
    }
    
    default R visit(AnyURI instance) {
      return otherwise((instance));
    }
    
    default R visit(Base64Binary instance) {
      return otherwise((instance));
    }
    
    default R visit(Boolean_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Byte_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Date instance) {
      return otherwise((instance));
    }
    
    default R visit(DateTime instance) {
      return otherwise((instance));
    }
    
    default R visit(Decimal instance) {
      return otherwise((instance));
    }
    
    default R visit(Double_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Duration instance) {
      return otherwise((instance));
    }
    
    default R visit(ENTITIES instance) {
      return otherwise((instance));
    }
    
    default R visit(ENTITY instance) {
      return otherwise((instance));
    }
    
    default R visit(Float_ instance) {
      return otherwise((instance));
    }
    
    default R visit(GDay instance) {
      return otherwise((instance));
    }
    
    default R visit(GMonth instance) {
      return otherwise((instance));
    }
    
    default R visit(GMonthDay instance) {
      return otherwise((instance));
    }
    
    default R visit(GYear instance) {
      return otherwise((instance));
    }
    
    default R visit(GYearMonth instance) {
      return otherwise((instance));
    }
    
    default R visit(HexBinary instance) {
      return otherwise((instance));
    }
    
    default R visit(ID instance) {
      return otherwise((instance));
    }
    
    default R visit(IDREF instance) {
      return otherwise((instance));
    }
    
    default R visit(IDREFS instance) {
      return otherwise((instance));
    }
    
    default R visit(Int instance) {
      return otherwise((instance));
    }
    
    default R visit(Integer_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Language instance) {
      return otherwise((instance));
    }
    
    default R visit(Long_ instance) {
      return otherwise((instance));
    }
    
    default R visit(NMTOKEN instance) {
      return otherwise((instance));
    }
    
    default R visit(NOTATION instance) {
      return otherwise((instance));
    }
    
    default R visit(Name instance) {
      return otherwise((instance));
    }
    
    default R visit(NegativeInteger instance) {
      return otherwise((instance));
    }
    
    default R visit(NonNegativeInteger instance) {
      return otherwise((instance));
    }
    
    default R visit(NonPositiveInteger instance) {
      return otherwise((instance));
    }
    
    default R visit(NormalizedString instance) {
      return otherwise((instance));
    }
    
    default R visit(PositiveInteger instance) {
      return otherwise((instance));
    }
    
    default R visit(QName instance) {
      return otherwise((instance));
    }
    
    default R visit(Short_ instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Time instance) {
      return otherwise((instance));
    }
    
    default R visit(Token instance) {
      return otherwise((instance));
    }
    
    default R visit(UnsignedByte instance) {
      return otherwise((instance));
    }
    
    default R visit(UnsignedInt instance) {
      return otherwise((instance));
    }
    
    default R visit(UnsignedLong instance) {
      return otherwise((instance));
    }
    
    default R visit(UnsignedShort instance) {
      return otherwise((instance));
    }
  }
  
  public static final class AnySimpleType extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public AnySimpleType () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnySimpleType)) {
        return false;
      }
      AnySimpleType o = (AnySimpleType) (other);
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
  
  public static final class AnyType extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public AnyType () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnyType)) {
        return false;
      }
      AnyType o = (AnyType) (other);
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
  
  public static final class AnyURI extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public AnyURI () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnyURI)) {
        return false;
      }
      AnyURI o = (AnyURI) (other);
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
  
  public static final class Base64Binary extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public Base64Binary () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Base64Binary)) {
        return false;
      }
      Base64Binary o = (Base64Binary) (other);
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
  
  public static final class Boolean_ extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public Boolean_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Boolean_)) {
        return false;
      }
      Boolean_ o = (Boolean_) (other);
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
  
  public static final class Byte_ extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public Byte_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Byte_)) {
        return false;
      }
      Byte_ o = (Byte_) (other);
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
  
  public static final class Date extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public Date () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Date)) {
        return false;
      }
      Date o = (Date) (other);
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
  
  public static final class DateTime extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public DateTime () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DateTime)) {
        return false;
      }
      DateTime o = (DateTime) (other);
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
  
  public static final class Decimal extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public Decimal () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Decimal)) {
        return false;
      }
      Decimal o = (Decimal) (other);
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
  
  public static final class Double_ extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
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
  
  public static final class Duration extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public Duration () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Duration)) {
        return false;
      }
      Duration o = (Duration) (other);
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
  
  public static final class ENTITIES extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public ENTITIES () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ENTITIES)) {
        return false;
      }
      ENTITIES o = (ENTITIES) (other);
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
  
  public static final class ENTITY extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public ENTITY () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ENTITY)) {
        return false;
      }
      ENTITY o = (ENTITY) (other);
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
  
  public static final class Float_ extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
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
  
  public static final class GDay extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public GDay () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GDay)) {
        return false;
      }
      GDay o = (GDay) (other);
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
  
  public static final class GMonth extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public GMonth () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GMonth)) {
        return false;
      }
      GMonth o = (GMonth) (other);
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
  
  public static final class GMonthDay extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public GMonthDay () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GMonthDay)) {
        return false;
      }
      GMonthDay o = (GMonthDay) (other);
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
  
  public static final class GYear extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public GYear () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GYear)) {
        return false;
      }
      GYear o = (GYear) (other);
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
  
  public static final class GYearMonth extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public GYearMonth () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GYearMonth)) {
        return false;
      }
      GYearMonth o = (GYearMonth) (other);
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
  
  public static final class HexBinary extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public HexBinary () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HexBinary)) {
        return false;
      }
      HexBinary o = (HexBinary) (other);
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
  
  public static final class ID extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public ID () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ID)) {
        return false;
      }
      ID o = (ID) (other);
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
  
  public static final class IDREF extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public IDREF () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IDREF)) {
        return false;
      }
      IDREF o = (IDREF) (other);
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
  
  public static final class IDREFS extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public IDREFS () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IDREFS)) {
        return false;
      }
      IDREFS o = (IDREFS) (other);
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
  
  public static final class Int extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public Int () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int)) {
        return false;
      }
      Int o = (Int) (other);
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
  
  public static final class Integer_ extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public Integer_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Integer_)) {
        return false;
      }
      Integer_ o = (Integer_) (other);
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
  
  public static final class Language extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public Language () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Language)) {
        return false;
      }
      Language o = (Language) (other);
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
  
  public static final class Long_ extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public Long_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Long_)) {
        return false;
      }
      Long_ o = (Long_) (other);
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
  
  public static final class NMTOKEN extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public NMTOKEN () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NMTOKEN)) {
        return false;
      }
      NMTOKEN o = (NMTOKEN) (other);
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
  
  public static final class NOTATION extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public NOTATION () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NOTATION)) {
        return false;
      }
      NOTATION o = (NOTATION) (other);
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
  
  public static final class Name extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public Name () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Name)) {
        return false;
      }
      Name o = (Name) (other);
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
  
  public static final class NegativeInteger extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public NegativeInteger () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NegativeInteger)) {
        return false;
      }
      NegativeInteger o = (NegativeInteger) (other);
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
  
  public static final class NonNegativeInteger extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public NonNegativeInteger () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NonNegativeInteger)) {
        return false;
      }
      NonNegativeInteger o = (NonNegativeInteger) (other);
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
  
  public static final class NonPositiveInteger extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public NonPositiveInteger () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NonPositiveInteger)) {
        return false;
      }
      NonPositiveInteger o = (NonPositiveInteger) (other);
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
  
  public static final class NormalizedString extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public NormalizedString () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NormalizedString)) {
        return false;
      }
      NormalizedString o = (NormalizedString) (other);
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
  
  public static final class PositiveInteger extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public PositiveInteger () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PositiveInteger)) {
        return false;
      }
      PositiveInteger o = (PositiveInteger) (other);
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
  
  public static final class QName extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public QName () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof QName)) {
        return false;
      }
      QName o = (QName) (other);
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
  
  public static final class Short_ extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public Short_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Short_)) {
        return false;
      }
      Short_ o = (Short_) (other);
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
  
  public static final class String_ extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
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
  
  public static final class Time extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public Time () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Time)) {
        return false;
      }
      Time o = (Time) (other);
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
  
  public static final class Token extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public Token () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Token)) {
        return false;
      }
      Token o = (Token) (other);
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
  
  public static final class UnsignedByte extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public UnsignedByte () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnsignedByte)) {
        return false;
      }
      UnsignedByte o = (UnsignedByte) (other);
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
  
  public static final class UnsignedInt extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public UnsignedInt () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnsignedInt)) {
        return false;
      }
      UnsignedInt o = (UnsignedInt) (other);
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
  
  public static final class UnsignedLong extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public UnsignedLong () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnsignedLong)) {
        return false;
      }
      UnsignedLong o = (UnsignedLong) (other);
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
  
  public static final class UnsignedShort extends hydra.ext.org.w3.xml.schema.Datatype implements Serializable {
    public UnsignedShort () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnsignedShort)) {
        return false;
      }
      UnsignedShort o = (UnsignedShort) (other);
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