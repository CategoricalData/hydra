// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class AugAssign implements Serializable, Comparable<AugAssign> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.AugAssign");
  
  public static final hydra.core.Name FIELD_NAME_PLUS_EQUAL = new hydra.core.Name("plusEqual");
  
  public static final hydra.core.Name FIELD_NAME_MINUS_EQUAL = new hydra.core.Name("minusEqual");
  
  public static final hydra.core.Name FIELD_NAME_TIMES_EQUAL = new hydra.core.Name("timesEqual");
  
  public static final hydra.core.Name FIELD_NAME_AT_EQUAL = new hydra.core.Name("atEqual");
  
  public static final hydra.core.Name FIELD_NAME_SLASH_EQUAL = new hydra.core.Name("slashEqual");
  
  public static final hydra.core.Name FIELD_NAME_PERCENT_EQUAL = new hydra.core.Name("percentEqual");
  
  public static final hydra.core.Name FIELD_NAME_AMPERSAND_EQUAL = new hydra.core.Name("ampersandEqual");
  
  public static final hydra.core.Name FIELD_NAME_BAR_EQUAL = new hydra.core.Name("barEqual");
  
  public static final hydra.core.Name FIELD_NAME_CARET_EQUAL = new hydra.core.Name("caretEqual");
  
  public static final hydra.core.Name FIELD_NAME_LEFT_SHIFT_EQUAL = new hydra.core.Name("leftShiftEqual");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT_SHIFT_EQUAL = new hydra.core.Name("rightShiftEqual");
  
  public static final hydra.core.Name FIELD_NAME_STAR_STAR_EQUAL = new hydra.core.Name("starStarEqual");
  
  public static final hydra.core.Name FIELD_NAME_DOUBLE_SLASH_EQUAL = new hydra.core.Name("doubleSlashEqual");
  
  private AugAssign () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(PlusEqual instance) ;
    
    R visit(MinusEqual instance) ;
    
    R visit(TimesEqual instance) ;
    
    R visit(AtEqual instance) ;
    
    R visit(SlashEqual instance) ;
    
    R visit(PercentEqual instance) ;
    
    R visit(AmpersandEqual instance) ;
    
    R visit(BarEqual instance) ;
    
    R visit(CaretEqual instance) ;
    
    R visit(LeftShiftEqual instance) ;
    
    R visit(RightShiftEqual instance) ;
    
    R visit(StarStarEqual instance) ;
    
    R visit(DoubleSlashEqual instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AugAssign instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(PlusEqual instance) {
      return otherwise(instance);
    }
    
    default R visit(MinusEqual instance) {
      return otherwise(instance);
    }
    
    default R visit(TimesEqual instance) {
      return otherwise(instance);
    }
    
    default R visit(AtEqual instance) {
      return otherwise(instance);
    }
    
    default R visit(SlashEqual instance) {
      return otherwise(instance);
    }
    
    default R visit(PercentEqual instance) {
      return otherwise(instance);
    }
    
    default R visit(AmpersandEqual instance) {
      return otherwise(instance);
    }
    
    default R visit(BarEqual instance) {
      return otherwise(instance);
    }
    
    default R visit(CaretEqual instance) {
      return otherwise(instance);
    }
    
    default R visit(LeftShiftEqual instance) {
      return otherwise(instance);
    }
    
    default R visit(RightShiftEqual instance) {
      return otherwise(instance);
    }
    
    default R visit(StarStarEqual instance) {
      return otherwise(instance);
    }
    
    default R visit(DoubleSlashEqual instance) {
      return otherwise(instance);
    }
  }
  
  public static final class PlusEqual extends hydra.ext.python.syntax.AugAssign implements Serializable {
    public PlusEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PlusEqual)) {
        return false;
      }
      PlusEqual o = (PlusEqual) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AugAssign other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class MinusEqual extends hydra.ext.python.syntax.AugAssign implements Serializable {
    public MinusEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MinusEqual)) {
        return false;
      }
      MinusEqual o = (MinusEqual) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AugAssign other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class TimesEqual extends hydra.ext.python.syntax.AugAssign implements Serializable {
    public TimesEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TimesEqual)) {
        return false;
      }
      TimesEqual o = (TimesEqual) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AugAssign other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class AtEqual extends hydra.ext.python.syntax.AugAssign implements Serializable {
    public AtEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AtEqual)) {
        return false;
      }
      AtEqual o = (AtEqual) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AugAssign other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class SlashEqual extends hydra.ext.python.syntax.AugAssign implements Serializable {
    public SlashEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SlashEqual)) {
        return false;
      }
      SlashEqual o = (SlashEqual) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AugAssign other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class PercentEqual extends hydra.ext.python.syntax.AugAssign implements Serializable {
    public PercentEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PercentEqual)) {
        return false;
      }
      PercentEqual o = (PercentEqual) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AugAssign other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class AmpersandEqual extends hydra.ext.python.syntax.AugAssign implements Serializable {
    public AmpersandEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AmpersandEqual)) {
        return false;
      }
      AmpersandEqual o = (AmpersandEqual) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AugAssign other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class BarEqual extends hydra.ext.python.syntax.AugAssign implements Serializable {
    public BarEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BarEqual)) {
        return false;
      }
      BarEqual o = (BarEqual) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AugAssign other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class CaretEqual extends hydra.ext.python.syntax.AugAssign implements Serializable {
    public CaretEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CaretEqual)) {
        return false;
      }
      CaretEqual o = (CaretEqual) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AugAssign other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class LeftShiftEqual extends hydra.ext.python.syntax.AugAssign implements Serializable {
    public LeftShiftEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LeftShiftEqual)) {
        return false;
      }
      LeftShiftEqual o = (LeftShiftEqual) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AugAssign other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class RightShiftEqual extends hydra.ext.python.syntax.AugAssign implements Serializable {
    public RightShiftEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RightShiftEqual)) {
        return false;
      }
      RightShiftEqual o = (RightShiftEqual) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AugAssign other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class StarStarEqual extends hydra.ext.python.syntax.AugAssign implements Serializable {
    public StarStarEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StarStarEqual)) {
        return false;
      }
      StarStarEqual o = (StarStarEqual) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AugAssign other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class DoubleSlashEqual extends hydra.ext.python.syntax.AugAssign implements Serializable {
    public DoubleSlashEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DoubleSlashEqual)) {
        return false;
      }
      DoubleSlashEqual o = (DoubleSlashEqual) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AugAssign other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
