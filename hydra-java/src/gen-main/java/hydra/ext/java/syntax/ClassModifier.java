package hydra.ext.java.syntax;

public abstract class ClassModifier {
  private ClassModifier () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Annotation instance) ;
    
    R visit(Public instance) ;
    
    R visit(Protected instance) ;
    
    R visit(Private instance) ;
    
    R visit(Abstract instance) ;
    
    R visit(Static instance) ;
    
    R visit(Final instance) ;
    
    R visit(Strictfp instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ClassModifier instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Annotation instance) {
      return otherwise((instance));
    }
    
    default R visit(Public instance) {
      return otherwise((instance));
    }
    
    default R visit(Protected instance) {
      return otherwise((instance));
    }
    
    default R visit(Private instance) {
      return otherwise((instance));
    }
    
    default R visit(Abstract instance) {
      return otherwise((instance));
    }
    
    default R visit(Static instance) {
      return otherwise((instance));
    }
    
    default R visit(Final instance) {
      return otherwise((instance));
    }
    
    default R visit(Strictfp instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Annotation extends ClassModifier {
    public final Annotation value;
    
    public Annotation (Annotation value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Annotation)) {
        return false;
      }
      Annotation o = (Annotation) (other);
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
  
  public static final class Public extends ClassModifier {
    public Public () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Public)) {
        return false;
      }
      Public o = (Public) (other);
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
  
  public static final class Protected extends ClassModifier {
    public Protected () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Protected)) {
        return false;
      }
      Protected o = (Protected) (other);
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
  
  public static final class Private extends ClassModifier {
    public Private () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Private)) {
        return false;
      }
      Private o = (Private) (other);
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
  
  public static final class Abstract extends ClassModifier {
    public Abstract () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Abstract)) {
        return false;
      }
      Abstract o = (Abstract) (other);
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
  
  public static final class Static extends ClassModifier {
    public Static () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Static)) {
        return false;
      }
      Static o = (Static) (other);
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
  
  public static final class Final extends ClassModifier {
    public Final () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Final)) {
        return false;
      }
      Final o = (Final) (other);
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
  
  public static final class Strictfp extends ClassModifier {
    public Strictfp () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Strictfp)) {
        return false;
      }
      Strictfp o = (Strictfp) (other);
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