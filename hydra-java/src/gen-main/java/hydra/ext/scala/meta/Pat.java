// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public abstract class Pat implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Pat");
  
  public static final hydra.core.Name FIELD_NAME_VAR = new hydra.core.Name("var");
  
  public static final hydra.core.Name FIELD_NAME_WILDCARD = new hydra.core.Name("wildcard");
  
  public static final hydra.core.Name FIELD_NAME_SEQ_WILDCARD = new hydra.core.Name("seqWildcard");
  
  public static final hydra.core.Name FIELD_NAME_BIND = new hydra.core.Name("bind");
  
  public static final hydra.core.Name FIELD_NAME_ALTERNATIVE = new hydra.core.Name("alternative");
  
  public static final hydra.core.Name FIELD_NAME_TUPLE = new hydra.core.Name("tuple");
  
  public static final hydra.core.Name FIELD_NAME_REPEATED = new hydra.core.Name("repeated");
  
  public static final hydra.core.Name FIELD_NAME_EXTRACT = new hydra.core.Name("extract");
  
  public static final hydra.core.Name FIELD_NAME_EXTRACT_INFIX = new hydra.core.Name("extractInfix");
  
  public static final hydra.core.Name FIELD_NAME_INTERPOLATE = new hydra.core.Name("interpolate");
  
  public static final hydra.core.Name FIELD_NAME_XML = new hydra.core.Name("xml");
  
  public static final hydra.core.Name FIELD_NAME_TYPED = new hydra.core.Name("typed");
  
  public static final hydra.core.Name FIELD_NAME_MACRO = new hydra.core.Name("macro");
  
  public static final hydra.core.Name FIELD_NAME_GIVEN = new hydra.core.Name("given");
  
  private Pat () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Var instance) ;
    
    R visit(Wildcard instance) ;
    
    R visit(SeqWildcard instance) ;
    
    R visit(Bind instance) ;
    
    R visit(Alternative instance) ;
    
    R visit(Tuple instance) ;
    
    R visit(Repeated instance) ;
    
    R visit(Extract instance) ;
    
    R visit(ExtractInfix instance) ;
    
    R visit(Interpolate instance) ;
    
    R visit(Xml instance) ;
    
    R visit(Typed instance) ;
    
    R visit(Macro instance) ;
    
    R visit(Given instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Pat instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Var instance) {
      return otherwise((instance));
    }
    
    default R visit(Wildcard instance) {
      return otherwise((instance));
    }
    
    default R visit(SeqWildcard instance) {
      return otherwise((instance));
    }
    
    default R visit(Bind instance) {
      return otherwise((instance));
    }
    
    default R visit(Alternative instance) {
      return otherwise((instance));
    }
    
    default R visit(Tuple instance) {
      return otherwise((instance));
    }
    
    default R visit(Repeated instance) {
      return otherwise((instance));
    }
    
    default R visit(Extract instance) {
      return otherwise((instance));
    }
    
    default R visit(ExtractInfix instance) {
      return otherwise((instance));
    }
    
    default R visit(Interpolate instance) {
      return otherwise((instance));
    }
    
    default R visit(Xml instance) {
      return otherwise((instance));
    }
    
    default R visit(Typed instance) {
      return otherwise((instance));
    }
    
    default R visit(Macro instance) {
      return otherwise((instance));
    }
    
    default R visit(Given instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Var extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Var value;
    
    public Var (hydra.ext.scala.meta.Pat_Var value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Var)) {
        return false;
      }
      Var o = (Var) (other);
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
  
  public static final class Wildcard extends hydra.ext.scala.meta.Pat implements Serializable {
    public Wildcard () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Wildcard)) {
        return false;
      }
      Wildcard o = (Wildcard) (other);
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
  
  public static final class SeqWildcard extends hydra.ext.scala.meta.Pat implements Serializable {
    public SeqWildcard () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SeqWildcard)) {
        return false;
      }
      SeqWildcard o = (SeqWildcard) (other);
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
  
  public static final class Bind extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Bind value;
    
    public Bind (hydra.ext.scala.meta.Pat_Bind value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bind)) {
        return false;
      }
      Bind o = (Bind) (other);
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
  
  public static final class Alternative extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Alternative value;
    
    public Alternative (hydra.ext.scala.meta.Pat_Alternative value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Alternative)) {
        return false;
      }
      Alternative o = (Alternative) (other);
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
  
  public static final class Tuple extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Tuple value;
    
    public Tuple (hydra.ext.scala.meta.Pat_Tuple value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tuple)) {
        return false;
      }
      Tuple o = (Tuple) (other);
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
  
  public static final class Repeated extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Repeated value;
    
    public Repeated (hydra.ext.scala.meta.Pat_Repeated value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Repeated)) {
        return false;
      }
      Repeated o = (Repeated) (other);
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
  
  public static final class Extract extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Extract value;
    
    public Extract (hydra.ext.scala.meta.Pat_Extract value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Extract)) {
        return false;
      }
      Extract o = (Extract) (other);
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
  
  public static final class ExtractInfix extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_ExtractInfix value;
    
    public ExtractInfix (hydra.ext.scala.meta.Pat_ExtractInfix value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ExtractInfix)) {
        return false;
      }
      ExtractInfix o = (ExtractInfix) (other);
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
  
  public static final class Interpolate extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Interpolate value;
    
    public Interpolate (hydra.ext.scala.meta.Pat_Interpolate value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Interpolate)) {
        return false;
      }
      Interpolate o = (Interpolate) (other);
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
  
  public static final class Xml extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Xml value;
    
    public Xml (hydra.ext.scala.meta.Pat_Xml value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Xml)) {
        return false;
      }
      Xml o = (Xml) (other);
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
  
  public static final class Typed extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Typed value;
    
    public Typed (hydra.ext.scala.meta.Pat_Typed value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Typed)) {
        return false;
      }
      Typed o = (Typed) (other);
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
  
  public static final class Macro extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Macro value;
    
    public Macro (hydra.ext.scala.meta.Pat_Macro value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Macro)) {
        return false;
      }
      Macro o = (Macro) (other);
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
  
  public static final class Given extends hydra.ext.scala.meta.Pat implements Serializable {
    public final hydra.ext.scala.meta.Pat_Given value;
    
    public Given (hydra.ext.scala.meta.Pat_Given value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Given)) {
        return false;
      }
      Given o = (Given) (other);
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