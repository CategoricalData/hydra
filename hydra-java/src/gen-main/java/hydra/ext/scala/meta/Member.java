package hydra.ext.scala.meta;

public abstract class Member {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Member");
  
  private Member () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Term instance) ;
    
    R visit(Type instance) ;
    
    R visit(TermParam instance) ;
    
    R visit(TypeParam instance) ;
    
    R visit(Self instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Member instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Term instance) {
      return otherwise((instance));
    }
    
    default R visit(Type instance) {
      return otherwise((instance));
    }
    
    default R visit(TermParam instance) {
      return otherwise((instance));
    }
    
    default R visit(TypeParam instance) {
      return otherwise((instance));
    }
    
    default R visit(Self instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Term extends hydra.ext.scala.meta.Member {
    public final hydra.ext.scala.meta.Member_Data value;
    
    public Term (hydra.ext.scala.meta.Member_Data value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Term)) {
        return false;
      }
      Term o = (Term) (other);
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
  
  public static final class Type extends hydra.ext.scala.meta.Member {
    public final hydra.ext.scala.meta.Member_Type value;
    
    public Type (hydra.ext.scala.meta.Member_Type value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Type)) {
        return false;
      }
      Type o = (Type) (other);
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
  
  public static final class TermParam extends hydra.ext.scala.meta.Member {
    public final hydra.ext.scala.meta.Data_Param value;
    
    public TermParam (hydra.ext.scala.meta.Data_Param value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TermParam)) {
        return false;
      }
      TermParam o = (TermParam) (other);
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
  
  public static final class TypeParam extends hydra.ext.scala.meta.Member {
    public final hydra.ext.scala.meta.Type_Param value;
    
    public TypeParam (hydra.ext.scala.meta.Type_Param value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeParam)) {
        return false;
      }
      TypeParam o = (TypeParam) (other);
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
  
  public static final class Self extends hydra.ext.scala.meta.Member {
    public final hydra.ext.scala.meta.Self value;
    
    public Self (hydra.ext.scala.meta.Self value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Self)) {
        return false;
      }
      Self o = (Self) (other);
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