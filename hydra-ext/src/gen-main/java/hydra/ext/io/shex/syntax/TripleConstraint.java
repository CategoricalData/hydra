// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class TripleConstraint implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.TripleConstraint");
  
  public static final hydra.core.Name FIELD_NAME_SENSE_FLAGS = new hydra.core.Name("senseFlags");
  
  public static final hydra.core.Name FIELD_NAME_PREDICATE = new hydra.core.Name("predicate");
  
  public static final hydra.core.Name FIELD_NAME_INLINE_SHAPE_EXPRESSION = new hydra.core.Name("inlineShapeExpression");
  
  public static final hydra.core.Name FIELD_NAME_CARDINALITY = new hydra.core.Name("cardinality");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_ANNOTATION = new hydra.core.Name("listOfAnnotation");
  
  public static final hydra.core.Name FIELD_NAME_SEMANTIC_ACTIONS = new hydra.core.Name("semanticActions");
  
  public final hydra.util.Opt<hydra.ext.io.shex.syntax.SenseFlags> senseFlags;
  
  public final hydra.ext.io.shex.syntax.Predicate predicate;
  
  public final hydra.ext.io.shex.syntax.InlineShapeExpression inlineShapeExpression;
  
  public final hydra.util.Opt<hydra.ext.io.shex.syntax.Cardinality> cardinality;
  
  public final java.util.List<hydra.ext.io.shex.syntax.Annotation> listOfAnnotation;
  
  public final hydra.ext.io.shex.syntax.SemanticActions semanticActions;
  
  public TripleConstraint (hydra.util.Opt<hydra.ext.io.shex.syntax.SenseFlags> senseFlags, hydra.ext.io.shex.syntax.Predicate predicate, hydra.ext.io.shex.syntax.InlineShapeExpression inlineShapeExpression, hydra.util.Opt<hydra.ext.io.shex.syntax.Cardinality> cardinality, java.util.List<hydra.ext.io.shex.syntax.Annotation> listOfAnnotation, hydra.ext.io.shex.syntax.SemanticActions semanticActions) {
    java.util.Objects.requireNonNull((senseFlags));
    java.util.Objects.requireNonNull((predicate));
    java.util.Objects.requireNonNull((inlineShapeExpression));
    java.util.Objects.requireNonNull((cardinality));
    java.util.Objects.requireNonNull((listOfAnnotation));
    java.util.Objects.requireNonNull((semanticActions));
    this.senseFlags = senseFlags;
    this.predicate = predicate;
    this.inlineShapeExpression = inlineShapeExpression;
    this.cardinality = cardinality;
    this.listOfAnnotation = listOfAnnotation;
    this.semanticActions = semanticActions;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TripleConstraint)) {
      return false;
    }
    TripleConstraint o = (TripleConstraint) (other);
    return senseFlags.equals(o.senseFlags) && predicate.equals(o.predicate) && inlineShapeExpression.equals(o.inlineShapeExpression) && cardinality.equals(o.cardinality) && listOfAnnotation.equals(o.listOfAnnotation) && semanticActions.equals(o.semanticActions);
  }
  
  @Override
  public int hashCode() {
    return 2 * senseFlags.hashCode() + 3 * predicate.hashCode() + 5 * inlineShapeExpression.hashCode() + 7 * cardinality.hashCode() + 11 * listOfAnnotation.hashCode() + 13 * semanticActions.hashCode();
  }
  
  public TripleConstraint withSenseFlags(hydra.util.Opt<hydra.ext.io.shex.syntax.SenseFlags> senseFlags) {
    java.util.Objects.requireNonNull((senseFlags));
    return new TripleConstraint(senseFlags, predicate, inlineShapeExpression, cardinality, listOfAnnotation, semanticActions);
  }
  
  public TripleConstraint withPredicate(hydra.ext.io.shex.syntax.Predicate predicate) {
    java.util.Objects.requireNonNull((predicate));
    return new TripleConstraint(senseFlags, predicate, inlineShapeExpression, cardinality, listOfAnnotation, semanticActions);
  }
  
  public TripleConstraint withInlineShapeExpression(hydra.ext.io.shex.syntax.InlineShapeExpression inlineShapeExpression) {
    java.util.Objects.requireNonNull((inlineShapeExpression));
    return new TripleConstraint(senseFlags, predicate, inlineShapeExpression, cardinality, listOfAnnotation, semanticActions);
  }
  
  public TripleConstraint withCardinality(hydra.util.Opt<hydra.ext.io.shex.syntax.Cardinality> cardinality) {
    java.util.Objects.requireNonNull((cardinality));
    return new TripleConstraint(senseFlags, predicate, inlineShapeExpression, cardinality, listOfAnnotation, semanticActions);
  }
  
  public TripleConstraint withListOfAnnotation(java.util.List<hydra.ext.io.shex.syntax.Annotation> listOfAnnotation) {
    java.util.Objects.requireNonNull((listOfAnnotation));
    return new TripleConstraint(senseFlags, predicate, inlineShapeExpression, cardinality, listOfAnnotation, semanticActions);
  }
  
  public TripleConstraint withSemanticActions(hydra.ext.io.shex.syntax.SemanticActions semanticActions) {
    java.util.Objects.requireNonNull((semanticActions));
    return new TripleConstraint(senseFlags, predicate, inlineShapeExpression, cardinality, listOfAnnotation, semanticActions);
  }
}