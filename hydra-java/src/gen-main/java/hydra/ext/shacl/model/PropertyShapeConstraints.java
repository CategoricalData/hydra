package hydra.ext.shacl.model;

/**
 * A number of constraint parameters which are specific to property shapes, and cannot be applied to node shapes
 */
public class PropertyShapeConstraints {
  /**
   * See https://www.w3.org/TR/shacl/#LessThanConstraintComponent
   */
  public final java.util.Set<hydra.ext.rdf.syntax.Property> lessThan;
  
  /**
   * See https://www.w3.org/TR/shacl/#LessThanOrEqualsConstraintComponent
   */
  public final java.util.Set<hydra.ext.rdf.syntax.Property> lessThanOrEquals;
  
  /**
   * The maximum cardinality. Node shapes cannot have any value for sh:maxCount. See https://www.w3.org/TR/shacl/#MaxCountConstraintComponent
   */
  public final java.util.Optional<java.math.BigInteger> maxCount;
  
  /**
   * The minimum cardinality. Node shapes cannot have any value for sh:minCount. See https://www.w3.org/TR/shacl/#MinCountConstraintComponent
   */
  public final java.util.Optional<java.math.BigInteger> minCount;
  
  /**
   * See https://www.w3.org/TR/shacl/#UniqueLangConstraintComponent
   */
  public final java.util.Optional<Boolean> uniqueLang;
  
  /**
   * See https://www.w3.org/TR/shacl/#QualifiedValueShapeConstraintComponent
   */
  public final java.util.Optional<hydra.ext.shacl.model.QualifiedValueShape> qualifiedValueShape;
  
  public PropertyShapeConstraints (java.util.Set<hydra.ext.rdf.syntax.Property> lessThan, java.util.Set<hydra.ext.rdf.syntax.Property> lessThanOrEquals, java.util.Optional<java.math.BigInteger> maxCount, java.util.Optional<java.math.BigInteger> minCount, java.util.Optional<Boolean> uniqueLang, java.util.Optional<hydra.ext.shacl.model.QualifiedValueShape> qualifiedValueShape) {
    this.lessThan = lessThan;
    this.lessThanOrEquals = lessThanOrEquals;
    this.maxCount = maxCount;
    this.minCount = minCount;
    this.uniqueLang = uniqueLang;
    this.qualifiedValueShape = qualifiedValueShape;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyShapeConstraints)) {
      return false;
    }
    PropertyShapeConstraints o = (PropertyShapeConstraints) (other);
    return lessThan.equals(o.lessThan) && lessThanOrEquals.equals(o.lessThanOrEquals) && maxCount.equals(o.maxCount) && minCount.equals(o.minCount) && uniqueLang.equals(o.uniqueLang) && qualifiedValueShape.equals(o.qualifiedValueShape);
  }
  
  @Override
  public int hashCode() {
    return 2 * lessThan.hashCode() + 3 * lessThanOrEquals.hashCode() + 5 * maxCount.hashCode() + 7 * minCount.hashCode() + 11 * uniqueLang.hashCode() + 13 * qualifiedValueShape.hashCode();
  }
  
  public PropertyShapeConstraints withLessThan(java.util.Set<hydra.ext.rdf.syntax.Property> lessThan) {
    return new PropertyShapeConstraints(lessThan, lessThanOrEquals, maxCount, minCount, uniqueLang, qualifiedValueShape);
  }
  
  public PropertyShapeConstraints withLessThanOrEquals(java.util.Set<hydra.ext.rdf.syntax.Property> lessThanOrEquals) {
    return new PropertyShapeConstraints(lessThan, lessThanOrEquals, maxCount, minCount, uniqueLang, qualifiedValueShape);
  }
  
  public PropertyShapeConstraints withMaxCount(java.util.Optional<java.math.BigInteger> maxCount) {
    return new PropertyShapeConstraints(lessThan, lessThanOrEquals, maxCount, minCount, uniqueLang, qualifiedValueShape);
  }
  
  public PropertyShapeConstraints withMinCount(java.util.Optional<java.math.BigInteger> minCount) {
    return new PropertyShapeConstraints(lessThan, lessThanOrEquals, maxCount, minCount, uniqueLang, qualifiedValueShape);
  }
  
  public PropertyShapeConstraints withUniqueLang(java.util.Optional<Boolean> uniqueLang) {
    return new PropertyShapeConstraints(lessThan, lessThanOrEquals, maxCount, minCount, uniqueLang, qualifiedValueShape);
  }
  
  public PropertyShapeConstraints withQualifiedValueShape(java.util.Optional<hydra.ext.shacl.model.QualifiedValueShape> qualifiedValueShape) {
    return new PropertyShapeConstraints(lessThan, lessThanOrEquals, maxCount, minCount, uniqueLang, qualifiedValueShape);
  }
}