package hydra.ext.scala.meta;

public abstract class Pat {
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
  
  public static final class Var extends Pat {
    public final Pat_Var value;
    
    public Var (Pat_Var value) {
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
  
  public static final class Wildcard extends Pat {
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
  
  public static final class SeqWildcard extends Pat {
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
  
  public static final class Bind extends Pat {
    public final Pat_Bind value;
    
    public Bind (Pat_Bind value) {
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
  
  public static final class Alternative extends Pat {
    public final Pat_Alternative value;
    
    public Alternative (Pat_Alternative value) {
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
  
  public static final class Tuple extends Pat {
    public final Pat_Tuple value;
    
    public Tuple (Pat_Tuple value) {
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
  
  public static final class Repeated extends Pat {
    public final Pat_Repeated value;
    
    public Repeated (Pat_Repeated value) {
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
  
  public static final class Extract extends Pat {
    public final Pat_Extract value;
    
    public Extract (Pat_Extract value) {
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
  
  public static final class ExtractInfix extends Pat {
    public final Pat_ExtractInfix value;
    
    public ExtractInfix (Pat_ExtractInfix value) {
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
  
  public static final class Interpolate extends Pat {
    public final Pat_Interpolate value;
    
    public Interpolate (Pat_Interpolate value) {
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
  
  public static final class Xml extends Pat {
    public final Pat_Xml value;
    
    public Xml (Pat_Xml value) {
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
  
  public static final class Typed extends Pat {
    public final Pat_Typed value;
    
    public Typed (Pat_Typed value) {
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
  
  public static final class Macro extends Pat {
    public final Pat_Macro value;
    
    public Macro (Pat_Macro value) {
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
  
  public static final class Given extends Pat {
    public final Pat_Given value;
    
    public Given (Pat_Given value) {
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