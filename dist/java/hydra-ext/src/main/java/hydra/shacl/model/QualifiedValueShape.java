// Note: this is an automatically generated file. Do not edit.

package hydra.shacl.model;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/shacl/#QualifiedValueShapeConstraintComponent
 */
public class QualifiedValueShape implements Serializable, Comparable<QualifiedValueShape> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shacl.model.QualifiedValueShape");

  public static final hydra.core.Name QUALIFIED_VALUE_SHAPE = new hydra.core.Name("qualifiedValueShape");

  public static final hydra.core.Name QUALIFIED_MAX_COUNT = new hydra.core.Name("qualifiedMaxCount");

  public static final hydra.core.Name QUALIFIED_MIN_COUNT = new hydra.core.Name("qualifiedMinCount");

  public static final hydra.core.Name QUALIFIED_VALUE_SHAPES_DISJOINT = new hydra.core.Name("qualifiedValueShapesDisjoint");

  public final hydra.shacl.model.Reference<hydra.shacl.model.Shape> qualifiedValueShape;

  public final java.math.BigInteger qualifiedMaxCount;

  public final java.math.BigInteger qualifiedMinCount;

  public final hydra.util.Maybe<Boolean> qualifiedValueShapesDisjoint;

  public QualifiedValueShape (hydra.shacl.model.Reference<hydra.shacl.model.Shape> qualifiedValueShape, java.math.BigInteger qualifiedMaxCount, java.math.BigInteger qualifiedMinCount, hydra.util.Maybe<Boolean> qualifiedValueShapesDisjoint) {
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
    QualifiedValueShape o = (QualifiedValueShape) other;
    return java.util.Objects.equals(
      this.qualifiedValueShape,
      o.qualifiedValueShape) && this.qualifiedMaxCount.compareTo(o.qualifiedMaxCount) == 0 && this.qualifiedMinCount.compareTo(o.qualifiedMinCount) == 0 && java.util.Objects.equals(
      this.qualifiedValueShapesDisjoint,
      o.qualifiedValueShapesDisjoint);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(qualifiedValueShape) + 3 * java.util.Objects.hashCode(qualifiedMaxCount) + 5 * java.util.Objects.hashCode(qualifiedMinCount) + 7 * java.util.Objects.hashCode(qualifiedValueShapesDisjoint);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(QualifiedValueShape other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      qualifiedValueShape,
      other.qualifiedValueShape);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      qualifiedMaxCount,
      other.qualifiedMaxCount);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      qualifiedMinCount,
      other.qualifiedMinCount);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      qualifiedValueShapesDisjoint,
      other.qualifiedValueShapesDisjoint);
  }

  public QualifiedValueShape withQualifiedValueShape(hydra.shacl.model.Reference<hydra.shacl.model.Shape> qualifiedValueShape) {
    return new QualifiedValueShape(qualifiedValueShape, qualifiedMaxCount, qualifiedMinCount, qualifiedValueShapesDisjoint);
  }

  public QualifiedValueShape withQualifiedMaxCount(java.math.BigInteger qualifiedMaxCount) {
    return new QualifiedValueShape(qualifiedValueShape, qualifiedMaxCount, qualifiedMinCount, qualifiedValueShapesDisjoint);
  }

  public QualifiedValueShape withQualifiedMinCount(java.math.BigInteger qualifiedMinCount) {
    return new QualifiedValueShape(qualifiedValueShape, qualifiedMaxCount, qualifiedMinCount, qualifiedValueShapesDisjoint);
  }

  public QualifiedValueShape withQualifiedValueShapesDisjoint(hydra.util.Maybe<Boolean> qualifiedValueShapesDisjoint) {
    return new QualifiedValueShape(qualifiedValueShape, qualifiedMaxCount, qualifiedMinCount, qualifiedValueShapesDisjoint);
  }
}
