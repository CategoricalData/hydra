package hydra.ext.scala.meta;

public abstract class Member_Data {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Member.Data");
  
  private Member_Data () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Pkg instance) ;
    
    R visit(Object_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Member_Data instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Pkg instance) {
      return otherwise((instance));
    }
    
    default R visit(Object_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Pkg extends hydra.ext.scala.meta.Member_Data {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Pkg");
    
    public final hydra.ext.scala.meta.Pkg value;
    
    public Pkg (hydra.ext.scala.meta.Pkg value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pkg)) {
        return false;
      }
      Pkg o = (Pkg) (other);
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
  
  public static final class Object_ extends hydra.ext.scala.meta.Member_Data {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Object");
    
    public final hydra.ext.scala.meta.Pkg_Object value;
    
    public Object_ (hydra.ext.scala.meta.Pkg_Object value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Object_)) {
        return false;
      }
      Object_ o = (Object_) (other);
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