// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public abstract class Tree implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Tree");
  
  public static final hydra.core.Name FIELD_NAME_REF = new hydra.core.Name("ref");
  
  public static final hydra.core.Name FIELD_NAME_STAT = new hydra.core.Name("stat");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_BOUNDS = new hydra.core.Name("bounds");
  
  public static final hydra.core.Name FIELD_NAME_PAT = new hydra.core.Name("pat");
  
  public static final hydra.core.Name FIELD_NAME_MEMBER = new hydra.core.Name("member");
  
  public static final hydra.core.Name FIELD_NAME_CTOR = new hydra.core.Name("ctor");
  
  public static final hydra.core.Name FIELD_NAME_TEMPLATE = new hydra.core.Name("template");
  
  public static final hydra.core.Name FIELD_NAME_MOD = new hydra.core.Name("mod");
  
  public static final hydra.core.Name FIELD_NAME_ENUMERATOR = new hydra.core.Name("enumerator");
  
  public static final hydra.core.Name FIELD_NAME_IMPORTER = new hydra.core.Name("importer");
  
  public static final hydra.core.Name FIELD_NAME_IMPORTEE = new hydra.core.Name("importee");
  
  public static final hydra.core.Name FIELD_NAME_CASE_TREE = new hydra.core.Name("caseTree");
  
  public static final hydra.core.Name FIELD_NAME_SOURCE = new hydra.core.Name("source");
  
  public static final hydra.core.Name FIELD_NAME_QUASI = new hydra.core.Name("quasi");
  
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
  
  public static final class Ref extends hydra.ext.scala.meta.Tree implements Serializable {
    public final hydra.ext.scala.meta.Ref value;
    
    public Ref (hydra.ext.scala.meta.Ref value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Stat extends hydra.ext.scala.meta.Tree implements Serializable {
    public final hydra.ext.scala.meta.Stat value;
    
    public Stat (hydra.ext.scala.meta.Stat value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Type extends hydra.ext.scala.meta.Tree implements Serializable {
    public final hydra.ext.scala.meta.Type value;
    
    public Type (hydra.ext.scala.meta.Type value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Bounds extends hydra.ext.scala.meta.Tree implements Serializable {
    public final hydra.ext.scala.meta.Type_Bounds value;
    
    public Bounds (hydra.ext.scala.meta.Type_Bounds value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Pat extends hydra.ext.scala.meta.Tree implements Serializable {
    public final hydra.ext.scala.meta.Pat value;
    
    public Pat (hydra.ext.scala.meta.Pat value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Member extends hydra.ext.scala.meta.Tree implements Serializable {
    public final hydra.ext.scala.meta.Member value;
    
    public Member (hydra.ext.scala.meta.Member value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Ctor extends hydra.ext.scala.meta.Tree implements Serializable {
    public final hydra.ext.scala.meta.Ctor value;
    
    public Ctor (hydra.ext.scala.meta.Ctor value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Template extends hydra.ext.scala.meta.Tree implements Serializable {
    public final hydra.ext.scala.meta.Template value;
    
    public Template (hydra.ext.scala.meta.Template value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Mod extends hydra.ext.scala.meta.Tree implements Serializable {
    public final hydra.ext.scala.meta.Mod value;
    
    public Mod (hydra.ext.scala.meta.Mod value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Enumerator extends hydra.ext.scala.meta.Tree implements Serializable {
    public final hydra.ext.scala.meta.Enumerator value;
    
    public Enumerator (hydra.ext.scala.meta.Enumerator value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Importer extends hydra.ext.scala.meta.Tree implements Serializable {
    public final hydra.ext.scala.meta.Importer value;
    
    public Importer (hydra.ext.scala.meta.Importer value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Importee extends hydra.ext.scala.meta.Tree implements Serializable {
    public final hydra.ext.scala.meta.Importee value;
    
    public Importee (hydra.ext.scala.meta.Importee value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class CaseTree extends hydra.ext.scala.meta.Tree implements Serializable {
    public final hydra.ext.scala.meta.CaseTree value;
    
    public CaseTree (hydra.ext.scala.meta.CaseTree value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Source extends hydra.ext.scala.meta.Tree implements Serializable {
    public final hydra.ext.scala.meta.Source value;
    
    public Source (hydra.ext.scala.meta.Source value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Quasi extends hydra.ext.scala.meta.Tree implements Serializable {
    public final hydra.ext.scala.meta.Quasi value;
    
    public Quasi (hydra.ext.scala.meta.Quasi value) {
      java.util.Objects.requireNonNull((value));
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
