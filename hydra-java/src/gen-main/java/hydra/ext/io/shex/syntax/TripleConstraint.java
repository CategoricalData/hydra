// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class TripleConstraint implements Serializable, Comparable<TripleConstraint> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.TripleConstraint");

  public static final hydra.core.Name SENSE_FLAGS = new hydra.core.Name("SenseFlags");

  public static final hydra.core.Name PREDICATE = new hydra.core.Name("Predicate");

  public static final hydra.core.Name INLINE_SHAPE_EXPRESSION = new hydra.core.Name("InlineShapeExpression");

  public static final hydra.core.Name CARDINALITY = new hydra.core.Name("Cardinality");

  public static final hydra.core.Name LIST_OF_ANNOTATION = new hydra.core.Name("listOfAnnotation");

  public static final hydra.core.Name SEMANTIC_ACTIONS = new hydra.core.Name("SemanticActions");

  public final hydra.util.Maybe<hydra.ext.io.shex.syntax.SenseFlags> SenseFlags;

  public final hydra.ext.io.shex.syntax.Predicate Predicate;

  public final hydra.ext.io.shex.syntax.InlineShapeExpression InlineShapeExpression;

  public final hydra.util.Maybe<hydra.ext.io.shex.syntax.Cardinality> Cardinality;

  public final hydra.util.ConsList<hydra.ext.io.shex.syntax.Annotation> listOfAnnotation;

  public final hydra.ext.io.shex.syntax.SemanticActions SemanticActions;

  public TripleConstraint (hydra.util.Maybe<hydra.ext.io.shex.syntax.SenseFlags> SenseFlags, hydra.ext.io.shex.syntax.Predicate Predicate, hydra.ext.io.shex.syntax.InlineShapeExpression InlineShapeExpression, hydra.util.Maybe<hydra.ext.io.shex.syntax.Cardinality> Cardinality, hydra.util.ConsList<hydra.ext.io.shex.syntax.Annotation> listOfAnnotation, hydra.ext.io.shex.syntax.SemanticActions SemanticActions) {
    this.SenseFlags = SenseFlags;
    this.Predicate = Predicate;
    this.InlineShapeExpression = InlineShapeExpression;
    this.Cardinality = Cardinality;
    this.listOfAnnotation = listOfAnnotation;
    this.SemanticActions = SemanticActions;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TripleConstraint)) {
      return false;
    }
    TripleConstraint o = (TripleConstraint) other;
    return java.util.Objects.equals(
      this.SenseFlags,
      o.SenseFlags) && java.util.Objects.equals(
      this.Predicate,
      o.Predicate) && java.util.Objects.equals(
      this.InlineShapeExpression,
      o.InlineShapeExpression) && java.util.Objects.equals(
      this.Cardinality,
      o.Cardinality) && java.util.Objects.equals(
      this.listOfAnnotation,
      o.listOfAnnotation) && java.util.Objects.equals(
      this.SemanticActions,
      o.SemanticActions);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(SenseFlags) + 3 * java.util.Objects.hashCode(Predicate) + 5 * java.util.Objects.hashCode(InlineShapeExpression) + 7 * java.util.Objects.hashCode(Cardinality) + 11 * java.util.Objects.hashCode(listOfAnnotation) + 13 * java.util.Objects.hashCode(SemanticActions);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TripleConstraint other) {
    int cmp = 0;
    cmp = ((Comparable) SenseFlags).compareTo(other.SenseFlags);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) Predicate).compareTo(other.Predicate);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) InlineShapeExpression).compareTo(other.InlineShapeExpression);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) Cardinality).compareTo(other.Cardinality);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) listOfAnnotation).compareTo(other.listOfAnnotation);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) SemanticActions).compareTo(other.SemanticActions);
  }

  public TripleConstraint withSenseFlags(hydra.util.Maybe<hydra.ext.io.shex.syntax.SenseFlags> SenseFlags) {
    return new TripleConstraint(SenseFlags, Predicate, InlineShapeExpression, Cardinality, listOfAnnotation, SemanticActions);
  }

  public TripleConstraint withPredicate(hydra.ext.io.shex.syntax.Predicate Predicate) {
    return new TripleConstraint(SenseFlags, Predicate, InlineShapeExpression, Cardinality, listOfAnnotation, SemanticActions);
  }

  public TripleConstraint withInlineShapeExpression(hydra.ext.io.shex.syntax.InlineShapeExpression InlineShapeExpression) {
    return new TripleConstraint(SenseFlags, Predicate, InlineShapeExpression, Cardinality, listOfAnnotation, SemanticActions);
  }

  public TripleConstraint withCardinality(hydra.util.Maybe<hydra.ext.io.shex.syntax.Cardinality> Cardinality) {
    return new TripleConstraint(SenseFlags, Predicate, InlineShapeExpression, Cardinality, listOfAnnotation, SemanticActions);
  }

  public TripleConstraint withListOfAnnotation(hydra.util.ConsList<hydra.ext.io.shex.syntax.Annotation> listOfAnnotation) {
    return new TripleConstraint(SenseFlags, Predicate, InlineShapeExpression, Cardinality, listOfAnnotation, SemanticActions);
  }

  public TripleConstraint withSemanticActions(hydra.ext.io.shex.syntax.SemanticActions SemanticActions) {
    return new TripleConstraint(SenseFlags, Predicate, InlineShapeExpression, Cardinality, listOfAnnotation, SemanticActions);
  }
}
