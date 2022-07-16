package hydra.ext.java.syntax;

public abstract class BlockStatement {
  private BlockStatement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(LocalVariableDeclaration instance) ;
    
    R visit(Class_ instance) ;
    
    R visit(Statement instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BlockStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(LocalVariableDeclaration instance) {
      return otherwise((instance));
    }
    
    default R visit(Class_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Statement instance) {
      return otherwise((instance));
    }
  }
  
  public static final class LocalVariableDeclaration extends BlockStatement {
    public final LocalVariableDeclarationStatement value;
    
    public LocalVariableDeclaration (LocalVariableDeclarationStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LocalVariableDeclaration)) {
        return false;
      }
      LocalVariableDeclaration o = (LocalVariableDeclaration) (other);
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
  
  public static final class Class_ extends BlockStatement {
    public final ClassDeclaration value;
    
    public Class_ (ClassDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Class_)) {
        return false;
      }
      Class_ o = (Class_) (other);
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
  
  public static final class Statement extends BlockStatement {
    public final Statement value;
    
    public Statement (Statement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Statement)) {
        return false;
      }
      Statement o = (Statement) (other);
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