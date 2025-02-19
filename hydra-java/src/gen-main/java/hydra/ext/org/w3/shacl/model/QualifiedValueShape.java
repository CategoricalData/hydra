// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.shacl.model;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/shacl/#QualifiedValueShapeConstraintComponent
 */
public class QualifiedValueShape implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.w3.shacl.model.QualifiedValueShape");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIED_VALUE_SHAPE = new hydra.core.Name("qualifiedValueShape");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIED_MAX_COUNT = new hydra.core.Name("qualifiedMaxCount");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIED_MIN_COUNT = new hydra.core.Name("qualifiedMinCount");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIED_VALUE_SHAPES_DISJOINT = new hydra.core.Name("qualifiedValueShapesDisjoint");
  
  public final hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.Shape> qualifiedValueShape;
  
  public final java.math.BigInteger qualifiedMaxCount;
  
  public final java.math.BigInteger qualifiedMinCount;
  
  public final hydra.util.Opt<Boolean> qualifiedValueShapesDisjoint;
  
  public QualifiedValueShape (hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.Shape> qualifiedValueShape, java.math.BigInteger qualifiedMaxCount, java.math.BigInteger qualifiedMinCount, hydra.util.Opt<Boolean> qualifiedValueShapesDisjoint) {
    java.util.Objects.requireNonNull((qualifiedValueShape));
    java.util.Objects.requireNonNull((qualifiedMaxCount));
    java.util.Objects.requireNonNull((qualifiedMinCount));
    java.util.Objects.requireNonNull((qualifiedValueShapesDisjoint));
    this.qualifiedValueShape = qualifiedValueShape;
    this.qualifiedMaxCount = qualifiedMaxCount;
    this.qualifiedMinCount = qualifiedMinCount;
    this.qualifiedValueShapesDisjoint = qualifiedValueShapesDisjoint;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QualifiedValueShape)) {
      return false;
    }
    QualifiedValueShape o = (QualifiedValueShape) (other);
    return qualifiedValueShape.equals(o.qualifiedValueShape) && qualifiedMaxCount.equals(o.qualifiedMaxCount) && qualifiedMinCount.equals(o.qualifiedMinCount) && qualifiedValueShapesDisjoint.equals(o.qualifiedValueShapesDisjoint);
  }
  
  @Override
  public int hashCode() {
    return 2 * qualifiedValueShape.hashCode() + 3 * qualifiedMaxCount.hashCode() + 5 * qualifiedMinCount.hashCode() + 7 * qualifiedValueShapesDisjoint.hashCode();
  }
  
  public QualifiedValueShape withQualifiedValueShape(hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.Shape> qualifiedValueShape) {
    java.util.Objects.requireNonNull((qualifiedValueShape));
    return new QualifiedValueShape(qualifiedValueShape, qualifiedMaxCount, qualifiedMinCount, qualifiedValueShapesDisjoint);
  }
  
  public QualifiedValueShape withQualifiedMaxCount(java.math.BigInteger qualifiedMaxCount) {
    java.util.Objects.requireNonNull((qualifiedMaxCount));
    return new QualifiedValueShape(qualifiedValueShape, qualifiedMaxCount, qualifiedMinCount, qualifiedValueShapesDisjoint);
  }
  
  public QualifiedValueShape withQualifiedMinCount(java.math.BigInteger qualifiedMinCount) {
    java.util.Objects.requireNonNull((qualifiedMinCount));
    return new QualifiedValueShape(qualifiedValueShape, qualifiedMaxCount, qualifiedMinCount, qualifiedValueShapesDisjoint);
  }
  
  public QualifiedValueShape withQualifiedValueShapesDisjoint(hydra.util.Opt<Boolean> qualifiedValueShapesDisjoint) {
    java.util.Objects.requireNonNull((qualifiedValueShapesDisjoint));
    return new QualifiedValueShape(qualifiedValueShape, qualifiedMaxCount, qualifiedMinCount, qualifiedValueShapesDisjoint);
  }
}