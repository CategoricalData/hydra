// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public abstract class Pattern0 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.Pattern0");
  
  public static final hydra.core.Name FIELD_NAME_QUALID = new hydra.core.Name("qualid");
  
  public static final hydra.core.Name FIELD_NAME_QUAL_ID_AND_PATTERN = new hydra.core.Name("qualIdAndPattern");
  
  public static final hydra.core.Name FIELD_NAME_PLACEHOLDER = new hydra.core.Name("placeholder");
  
  public static final hydra.core.Name FIELD_NAME_PARENS = new hydra.core.Name("parens");
  
  public static final hydra.core.Name FIELD_NAME_NUMBER = new hydra.core.Name("number");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  private Pattern0 () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Qualid instance) ;
    
    R visit(QualIdAndPattern instance) ;
    
    R visit(Placeholder instance) ;
    
    R visit(Parens instance) ;
    
    R visit(Number_ instance) ;
    
    R visit(String_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Pattern0 instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Qualid instance) {
      return otherwise((instance));
    }
    
    default R visit(QualIdAndPattern instance) {
      return otherwise((instance));
    }
    
    default R visit(Placeholder instance) {
      return otherwise((instance));
    }
    
    default R visit(Parens instance) {
      return otherwise((instance));
    }
    
    default R visit(Number_ instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Qualid extends hydra.ext.fr.inria.coq.syntax.Pattern0 implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Qualid value;
    
    public Qualid (hydra.ext.fr.inria.coq.syntax.Qualid value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Qualid)) {
        return false;
      }
      Qualid o = (Qualid) (other);
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
  
  public static final class QualIdAndPattern extends hydra.ext.fr.inria.coq.syntax.Pattern0 implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.QualidAndPattern value;
    
    public QualIdAndPattern (hydra.ext.fr.inria.coq.syntax.QualidAndPattern value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof QualIdAndPattern)) {
        return false;
      }
      QualIdAndPattern o = (QualIdAndPattern) (other);
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
  
  public static final class Placeholder extends hydra.ext.fr.inria.coq.syntax.Pattern0 implements Serializable {
    public Placeholder () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Placeholder)) {
        return false;
      }
      Placeholder o = (Placeholder) (other);
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
  
  public static final class Parens extends hydra.ext.fr.inria.coq.syntax.Pattern0 implements Serializable {
    public final java.util.List<hydra.ext.fr.inria.coq.syntax.Pattern> value;
    
    public Parens (java.util.List<hydra.ext.fr.inria.coq.syntax.Pattern> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parens)) {
        return false;
      }
      Parens o = (Parens) (other);
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
  
  public static final class Number_ extends hydra.ext.fr.inria.coq.syntax.Pattern0 implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Number_ value;
    
    public Number_ (hydra.ext.fr.inria.coq.syntax.Number_ value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class String_ extends hydra.ext.fr.inria.coq.syntax.Pattern0 implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.String_ value;
    
    public String_ (hydra.ext.fr.inria.coq.syntax.String_ value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) (other);
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