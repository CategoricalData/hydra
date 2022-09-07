package hydra.ext.coq.syntax;

public abstract class Pattern0 {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Pattern0");
  
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
  
  public static final class Qualid extends hydra.ext.coq.syntax.Pattern0 {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Qualid");
    
    public final hydra.ext.coq.syntax.Qualid value;
    
    public Qualid (hydra.ext.coq.syntax.Qualid value) {
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
  
  public static final class QualIdAndPattern extends hydra.ext.coq.syntax.Pattern0 {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.QualIdAndPattern");
    
    public final hydra.ext.coq.syntax.QualidAndPattern value;
    
    public QualIdAndPattern (hydra.ext.coq.syntax.QualidAndPattern value) {
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
  
  public static final class Placeholder extends hydra.ext.coq.syntax.Pattern0 {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Placeholder");
    
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
  
  public static final class Parens extends hydra.ext.coq.syntax.Pattern0 {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Parens");
    
    public final java.util.List<hydra.ext.coq.syntax.Pattern> value;
    
    public Parens (java.util.List<hydra.ext.coq.syntax.Pattern> value) {
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
  
  public static final class Number_ extends hydra.ext.coq.syntax.Pattern0 {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Number");
    
    public final hydra.ext.coq.syntax.Number_ value;
    
    public Number_ (hydra.ext.coq.syntax.Number_ value) {
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
  
  public static final class String_ extends hydra.ext.coq.syntax.Pattern0 {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.String");
    
    public final hydra.ext.coq.syntax.String_ value;
    
    public String_ (hydra.ext.coq.syntax.String_ value) {
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