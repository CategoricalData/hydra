// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class Parameters implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Parameters");
  
  public static final hydra.core.Name FIELD_NAME_SLASH_NO_DEFAULT = new hydra.core.Name("slashNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_SLASH_WITH_DEFAULT = new hydra.core.Name("slashWithDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_NO_DEFAULT = new hydra.core.Name("paramNoDefault");
  
  public static final hydra.core.Name FIELD_NAME_PARAM_WITH_DEFAULT = new hydra.core.Name("paramWithDefault");
  
  public static final hydra.core.Name FIELD_NAME_STAR_ETC = new hydra.core.Name("starEtc");
  
  private Parameters () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(SlashNoDefault instance) ;
    
    R visit(SlashWithDefault instance) ;
    
    R visit(ParamNoDefault instance) ;
    
    R visit(ParamWithDefault instance) ;
    
    R visit(StarEtc instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Parameters instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(SlashNoDefault instance) {
      return otherwise((instance));
    }
    
    default R visit(SlashWithDefault instance) {
      return otherwise((instance));
    }
    
    default R visit(ParamNoDefault instance) {
      return otherwise((instance));
    }
    
    default R visit(ParamWithDefault instance) {
      return otherwise((instance));
    }
    
    default R visit(StarEtc instance) {
      return otherwise((instance));
    }
  }
  
  public static final class SlashNoDefault extends hydra.ext.python.syntax.Parameters implements Serializable {
    public final hydra.ext.python.syntax.SlashNoDefaultParameters value;
    
    public SlashNoDefault (hydra.ext.python.syntax.SlashNoDefaultParameters value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SlashNoDefault)) {
        return false;
      }
      SlashNoDefault o = (SlashNoDefault) (other);
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
  
  public static final class SlashWithDefault extends hydra.ext.python.syntax.Parameters implements Serializable {
    public final hydra.ext.python.syntax.SlashWithDefaultParameters value;
    
    public SlashWithDefault (hydra.ext.python.syntax.SlashWithDefaultParameters value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SlashWithDefault)) {
        return false;
      }
      SlashWithDefault o = (SlashWithDefault) (other);
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
  
  public static final class ParamNoDefault extends hydra.ext.python.syntax.Parameters implements Serializable {
    public final hydra.ext.python.syntax.ParamNoDefaultParameters value;
    
    public ParamNoDefault (hydra.ext.python.syntax.ParamNoDefaultParameters value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ParamNoDefault)) {
        return false;
      }
      ParamNoDefault o = (ParamNoDefault) (other);
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
  
  public static final class ParamWithDefault extends hydra.ext.python.syntax.Parameters implements Serializable {
    public final hydra.ext.python.syntax.ParamWithDefaultParameters value;
    
    public ParamWithDefault (hydra.ext.python.syntax.ParamWithDefaultParameters value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ParamWithDefault)) {
        return false;
      }
      ParamWithDefault o = (ParamWithDefault) (other);
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
  
  public static final class StarEtc extends hydra.ext.python.syntax.Parameters implements Serializable {
    public final hydra.ext.python.syntax.StarEtc value;
    
    public StarEtc (hydra.ext.python.syntax.StarEtc value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StarEtc)) {
        return false;
      }
      StarEtc o = (StarEtc) (other);
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