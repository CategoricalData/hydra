// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shacl.model;

import java.io.Serializable;

/**
 * A number of constraint parameters which are specific to property shapes, and cannot be applied to node shapes
 */
public abstract class PropertyShapeConstraint implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shacl/model.PropertyShapeConstraint");
  
  public static final hydra.core.Name FIELD_NAME_LESS_THAN = new hydra.core.Name("lessThan");
  
  public static final hydra.core.Name FIELD_NAME_LESS_THAN_OR_EQUALS = new hydra.core.Name("lessThanOrEquals");
  
  public static final hydra.core.Name FIELD_NAME_MAX_COUNT = new hydra.core.Name("maxCount");
  
  public static final hydra.core.Name FIELD_NAME_MIN_COUNT = new hydra.core.Name("minCount");
  
  public static final hydra.core.Name FIELD_NAME_UNIQUE_LANG = new hydra.core.Name("uniqueLang");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIED_VALUE_SHAPE = new hydra.core.Name("qualifiedValueShape");
  
  private PropertyShapeConstraint () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(LessThan instance) ;
    
    R visit(LessThanOrEquals instance) ;
    
    R visit(MaxCount instance) ;
    
    R visit(MinCount instance) ;
    
    R visit(UniqueLang instance) ;
    
    R visit(QualifiedValueShape instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PropertyShapeConstraint instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(LessThan instance) {
      return otherwise((instance));
    }
    
    default R visit(LessThanOrEquals instance) {
      return otherwise((instance));
    }
    
    default R visit(MaxCount instance) {
      return otherwise((instance));
    }
    
    default R visit(MinCount instance) {
      return otherwise((instance));
    }
    
    default R visit(UniqueLang instance) {
      return otherwise((instance));
    }
    
    default R visit(QualifiedValueShape instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * See https://www.w3.org/TR/shacl/#LessThanConstraintComponent
   */
  public static final class LessThan extends hydra.ext.shacl.model.PropertyShapeConstraint implements Serializable {
    public final java.util.Set<hydra.ext.rdf.syntax.Property> value;
    
    public LessThan (java.util.Set<hydra.ext.rdf.syntax.Property> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LessThan)) {
        return false;
      }
      LessThan o = (LessThan) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#LessThanOrEqualsConstraintComponent
   */
  public static final class LessThanOrEquals extends hydra.ext.shacl.model.PropertyShapeConstraint implements Serializable {
    public final java.util.Set<hydra.ext.rdf.syntax.Property> value;
    
    public LessThanOrEquals (java.util.Set<hydra.ext.rdf.syntax.Property> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LessThanOrEquals)) {
        return false;
      }
      LessThanOrEquals o = (LessThanOrEquals) (other);
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
  
  /**
   * The maximum cardinality. Node shapes cannot have any value for sh:maxCount. See https://www.w3.org/TR/shacl/#MaxCountConstraintComponent
   */
  public static final class MaxCount extends hydra.ext.shacl.model.PropertyShapeConstraint implements Serializable {
    public final java.math.BigInteger value;
    
    public MaxCount (java.math.BigInteger value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MaxCount)) {
        return false;
      }
      MaxCount o = (MaxCount) (other);
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
  
  /**
   * The minimum cardinality. Node shapes cannot have any value for sh:minCount. See https://www.w3.org/TR/shacl/#MinCountConstraintComponent
   */
  public static final class MinCount extends hydra.ext.shacl.model.PropertyShapeConstraint implements Serializable {
    public final java.math.BigInteger value;
    
    public MinCount (java.math.BigInteger value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MinCount)) {
        return false;
      }
      MinCount o = (MinCount) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#UniqueLangConstraintComponent
   */
  public static final class UniqueLang extends hydra.ext.shacl.model.PropertyShapeConstraint implements Serializable {
    public final Boolean value;
    
    public UniqueLang (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UniqueLang)) {
        return false;
      }
      UniqueLang o = (UniqueLang) (other);
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
  
  /**
   * See https://www.w3.org/TR/shacl/#QualifiedValueShapeConstraintComponent
   */
  public static final class QualifiedValueShape extends hydra.ext.shacl.model.PropertyShapeConstraint implements Serializable {
    public final hydra.ext.shacl.model.QualifiedValueShape value;
    
    public QualifiedValueShape (hydra.ext.shacl.model.QualifiedValueShape value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof QualifiedValueShape)) {
        return false;
      }
      QualifiedValueShape o = (QualifiedValueShape) (other);
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
