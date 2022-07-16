package hydra.ext.scala.meta;

public abstract class Tree {
  private Tree () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Ref instance) ;
    
    R visit(Stat instance) ;
    
    R visit(Type instance) ;
    
    R visit(Bounds instance) ;
    
    R visit(Pat instance) ;
    
    R visit(Member instance) ;
    
    R visit(Ctor instance) ;
    
    R visit(Template instance) ;
    
    R visit(Mod instance) ;
    
    R visit(Enumerator instance) ;
    
    R visit(Importer instance) ;
    
    R visit(Importee instance) ;
    
    R visit(CaseTree instance) ;
    
    R visit(Source instance) ;
    
    R visit(Quasi instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Tree instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Ref instance) {
      return otherwise((instance));
    }
    
    default R visit(Stat instance) {
      return otherwise((instance));
    }
    
    default R visit(Type instance) {
      return otherwise((instance));
    }
    
    default R visit(Bounds instance) {
      return otherwise((instance));
    }
    
    default R visit(Pat instance) {
      return otherwise((instance));
    }
    
    default R visit(Member instance) {
      return otherwise((instance));
    }
    
    default R visit(Ctor instance) {
      return otherwise((instance));
    }
    
    default R visit(Template instance) {
      return otherwise((instance));
    }
    
    default R visit(Mod instance) {
      return otherwise((instance));
    }
    
    default R visit(Enumerator instance) {
      return otherwise((instance));
    }
    
    default R visit(Importer instance) {
      return otherwise((instance));
    }
    
    default R visit(Importee instance) {
      return otherwise((instance));
    }
    
    default R visit(CaseTree instance) {
      return otherwise((instance));
    }
    
    default R visit(Source instance) {
      return otherwise((instance));
    }
    
    default R visit(Quasi instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Ref extends Tree {
    public final Ref value;
    
    public Ref (Ref value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ref)) {
        return false;
      }
      Ref o = (Ref) (other);
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
  
  public static final class Stat extends Tree {
    public final Stat value;
    
    public Stat (Stat value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Stat)) {
        return false;
      }
      Stat o = (Stat) (other);
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
  
  public static final class Type extends Tree {
    public final Type value;
    
    public Type (Type value) {
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
  
  public static final class Bounds extends Tree {
    public final Type_Bounds value;
    
    public Bounds (Type_Bounds value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bounds)) {
        return false;
      }
      Bounds o = (Bounds) (other);
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
  
  public static final class Pat extends Tree {
    public final Pat value;
    
    public Pat (Pat value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pat)) {
        return false;
      }
      Pat o = (Pat) (other);
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
  
  public static final class Member extends Tree {
    public final Member value;
    
    public Member (Member value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Member)) {
        return false;
      }
      Member o = (Member) (other);
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
  
  public static final class Ctor extends Tree {
    public final Ctor value;
    
    public Ctor (Ctor value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ctor)) {
        return false;
      }
      Ctor o = (Ctor) (other);
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
  
  public static final class Template extends Tree {
    public final Template value;
    
    public Template (Template value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Template)) {
        return false;
      }
      Template o = (Template) (other);
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
  
  public static final class Mod extends Tree {
    public final Mod value;
    
    public Mod (Mod value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Mod)) {
        return false;
      }
      Mod o = (Mod) (other);
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
  
  public static final class Enumerator extends Tree {
    public final Enumerator value;
    
    public Enumerator (Enumerator value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Enumerator)) {
        return false;
      }
      Enumerator o = (Enumerator) (other);
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
  
  public static final class Importer extends Tree {
    public final Importer value;
    
    public Importer (Importer value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Importer)) {
        return false;
      }
      Importer o = (Importer) (other);
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
  
  public static final class Importee extends Tree {
    public final Importee value;
    
    public Importee (Importee value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Importee)) {
        return false;
      }
      Importee o = (Importee) (other);
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
  
  public static final class CaseTree extends Tree {
    public final CaseTree value;
    
    public CaseTree (CaseTree value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CaseTree)) {
        return false;
      }
      CaseTree o = (CaseTree) (other);
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
  
  public static final class Source extends Tree {
    public final Source value;
    
    public Source (Source value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Source)) {
        return false;
      }
      Source o = (Source) (other);
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
  
  public static final class Quasi extends Tree {
    public final Quasi value;
    
    public Quasi (Quasi value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Quasi)) {
        return false;
      }
      Quasi o = (Quasi) (other);
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