package hydra.ext.java.syntax;

public abstract class ClassBodyDeclaration {
  private ClassBodyDeclaration () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(ClassMember instance) ;
    
    R visit(InstanceInitializer instance) ;
    
    R visit(StaticInitializer instance) ;
    
    R visit(ConstructorDeclaration instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ClassBodyDeclaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(ClassMember instance) {
      return otherwise((instance));
    }
    
    default R visit(InstanceInitializer instance) {
      return otherwise((instance));
    }
    
    default R visit(StaticInitializer instance) {
      return otherwise((instance));
    }
    
    default R visit(ConstructorDeclaration instance) {
      return otherwise((instance));
    }
  }
  
  public static final class ClassMember extends ClassBodyDeclaration {
    public final ClassMemberDeclaration value;
    
    public ClassMember (ClassMemberDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClassMember)) {
        return false;
      }
      ClassMember o = (ClassMember) (other);
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
  
  public static final class InstanceInitializer extends ClassBodyDeclaration {
    public final InstanceInitializer value;
    
    public InstanceInitializer (InstanceInitializer value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InstanceInitializer)) {
        return false;
      }
      InstanceInitializer o = (InstanceInitializer) (other);
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
  
  public static final class StaticInitializer extends ClassBodyDeclaration {
    public final StaticInitializer value;
    
    public StaticInitializer (StaticInitializer value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StaticInitializer)) {
        return false;
      }
      StaticInitializer o = (StaticInitializer) (other);
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
  
  public static final class ConstructorDeclaration extends ClassBodyDeclaration {
    public final ConstructorDeclaration value;
    
    public ConstructorDeclaration (ConstructorDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ConstructorDeclaration)) {
        return false;
      }
      ConstructorDeclaration o = (ConstructorDeclaration) (other);
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