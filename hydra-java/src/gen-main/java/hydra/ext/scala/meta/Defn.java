package hydra.ext.scala.meta;

public abstract class Defn {
  private Defn () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Val instance) ;
    
    R visit(Var instance) ;
    
    R visit(Given instance) ;
    
    R visit(Enum_ instance) ;
    
    R visit(EnumCase instance) ;
    
    R visit(RepeatedEnumCase instance) ;
    
    R visit(GivenAlias instance) ;
    
    R visit(ExtensionGroup instance) ;
    
    R visit(Def instance) ;
    
    R visit(Macro instance) ;
    
    R visit(Type instance) ;
    
    R visit(Class_ instance) ;
    
    R visit(Trait instance) ;
    
    R visit(Object_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Defn instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Val instance) {
      return otherwise((instance));
    }
    
    default R visit(Var instance) {
      return otherwise((instance));
    }
    
    default R visit(Given instance) {
      return otherwise((instance));
    }
    
    default R visit(Enum_ instance) {
      return otherwise((instance));
    }
    
    default R visit(EnumCase instance) {
      return otherwise((instance));
    }
    
    default R visit(RepeatedEnumCase instance) {
      return otherwise((instance));
    }
    
    default R visit(GivenAlias instance) {
      return otherwise((instance));
    }
    
    default R visit(ExtensionGroup instance) {
      return otherwise((instance));
    }
    
    default R visit(Def instance) {
      return otherwise((instance));
    }
    
    default R visit(Macro instance) {
      return otherwise((instance));
    }
    
    default R visit(Type instance) {
      return otherwise((instance));
    }
    
    default R visit(Class_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Trait instance) {
      return otherwise((instance));
    }
    
    default R visit(Object_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Val extends Defn {
    public final Defn_Val value;
    
    public Val (Defn_Val value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Val)) {
        return false;
      }
      Val o = (Val) (other);
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
  
  public static final class Var extends Defn {
    public final Defn_Var value;
    
    public Var (Defn_Var value) {
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
  
  public static final class Given extends Defn {
    public final Defn_Given value;
    
    public Given (Defn_Given value) {
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
  
  public static final class Enum_ extends Defn {
    public final Defn_Enum value;
    
    public Enum_ (Defn_Enum value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Enum_)) {
        return false;
      }
      Enum_ o = (Enum_) (other);
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
  
  public static final class EnumCase extends Defn {
    public final Defn_EnumCase value;
    
    public EnumCase (Defn_EnumCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EnumCase)) {
        return false;
      }
      EnumCase o = (EnumCase) (other);
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
  
  public static final class RepeatedEnumCase extends Defn {
    public final Defn_RepeatedEnumCase value;
    
    public RepeatedEnumCase (Defn_RepeatedEnumCase value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RepeatedEnumCase)) {
        return false;
      }
      RepeatedEnumCase o = (RepeatedEnumCase) (other);
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
  
  public static final class GivenAlias extends Defn {
    public final Defn_GivenAlias value;
    
    public GivenAlias (Defn_GivenAlias value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GivenAlias)) {
        return false;
      }
      GivenAlias o = (GivenAlias) (other);
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
  
  public static final class ExtensionGroup extends Defn {
    public final Defn_ExtensionGroup value;
    
    public ExtensionGroup (Defn_ExtensionGroup value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ExtensionGroup)) {
        return false;
      }
      ExtensionGroup o = (ExtensionGroup) (other);
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
  
  public static final class Def extends Defn {
    public final Defn_Def value;
    
    public Def (Defn_Def value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Def)) {
        return false;
      }
      Def o = (Def) (other);
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
  
  public static final class Macro extends Defn {
    public final Defn_Macro value;
    
    public Macro (Defn_Macro value) {
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
  
  public static final class Type extends Defn {
    public final Defn_Type value;
    
    public Type (Defn_Type value) {
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
  
  public static final class Class_ extends Defn {
    public final Defn_Class value;
    
    public Class_ (Defn_Class value) {
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
  
  public static final class Trait extends Defn {
    public final Defn_Trait value;
    
    public Trait (Defn_Trait value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Trait)) {
        return false;
      }
      Trait o = (Trait) (other);
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
  
  public static final class Object_ extends Defn {
    public final Defn_Object value;
    
    public Object_ (Defn_Object value) {
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