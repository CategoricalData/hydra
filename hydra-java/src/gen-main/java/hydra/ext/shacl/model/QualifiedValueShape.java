package hydra.ext.shacl.model;

/**
 * See https://www.w3.org/TR/shacl/#QualifiedValueShapeConstraintComponent
 */
public class QualifiedValueShape {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shacl/model.QualifiedValueShape");
  
  public final hydra.ext.shacl.model.Shape shape;
  
  public final java.math.BigInteger qualifiedManCount;
  
  public final java.math.BigInteger qualifiedMinCount;
  
  public final java.util.Optional<Boolean> qualifiedValueShapesDisjoint;
  
  public QualifiedValueShape (hydra.ext.shacl.model.Shape shape, java.math.BigInteger qualifiedManCount, java.math.BigInteger qualifiedMinCount, java.util.Optional<Boolean> qualifiedValueShapesDisjoint) {
    this.shape = shape;
    this.qualifiedManCount = qualifiedManCount;
    this.qualifiedMinCount = qualifiedMinCount;
    this.qualifiedValueShapesDisjoint = qualifiedValueShapesDisjoint;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QualifiedValueShape)) {
      return false;
    }
    QualifiedValueShape o = (QualifiedValueShape) (other);
    return shape.equals(o.shape) && qualifiedManCount.equals(o.qualifiedManCount) && qualifiedMinCount.equals(o.qualifiedMinCount) && qualifiedValueShapesDisjoint.equals(o.qualifiedValueShapesDisjoint);
  }
  
  @Override
  public int hashCode() {
    return 2 * shape.hashCode() + 3 * qualifiedManCount.hashCode() + 5 * qualifiedMinCount.hashCode() + 7 * qualifiedValueShapesDisjoint.hashCode();
  }
  
  public QualifiedValueShape withShape(hydra.ext.shacl.model.Shape shape) {
    return new QualifiedValueShape(shape, qualifiedManCount, qualifiedMinCount, qualifiedValueShapesDisjoint);
  }
  
  public QualifiedValueShape withQualifiedManCount(java.math.BigInteger qualifiedManCount) {
    return new QualifiedValueShape(shape, qualifiedManCount, qualifiedMinCount, qualifiedValueShapesDisjoint);
  }
  
  public QualifiedValueShape withQualifiedMinCount(java.math.BigInteger qualifiedMinCount) {
    return new QualifiedValueShape(shape, qualifiedManCount, qualifiedMinCount, qualifiedValueShapesDisjoint);
  }
  
  public QualifiedValueShape withQualifiedValueShapesDisjoint(java.util.Optional<Boolean> qualifiedValueShapesDisjoint) {
    return new QualifiedValueShape(shape, qualifiedManCount, qualifiedMinCount, qualifiedValueShapesDisjoint);
  }
}