// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class TypedNestedGraphTypeSpecification implements Serializable, Comparable<TypedNestedGraphTypeSpecification> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TypedNestedGraphTypeSpecification");

  public static final hydra.core.Name TYPED = new hydra.core.Name("typed");

  public static final hydra.core.Name GRAPH = new hydra.core.Name("graph");

  public static final hydra.core.Name SPECIFICATION = new hydra.core.Name("specification");

  public final hydra.util.Maybe<java.lang.Void> typed;

  public final Boolean graph;

  public final java.util.List<openGql.grammar.ElementTypeSpecification> specification;

  public TypedNestedGraphTypeSpecification (hydra.util.Maybe<java.lang.Void> typed, Boolean graph, java.util.List<openGql.grammar.ElementTypeSpecification> specification) {
    this.typed = typed;
    this.graph = graph;
    this.specification = specification;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypedNestedGraphTypeSpecification)) {
      return false;
    }
    TypedNestedGraphTypeSpecification o = (TypedNestedGraphTypeSpecification) other;
    return java.util.Objects.equals(
      this.typed,
      o.typed) && java.util.Objects.equals(
      this.graph,
      o.graph) && java.util.Objects.equals(
      this.specification,
      o.specification);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typed) + 3 * java.util.Objects.hashCode(graph) + 5 * java.util.Objects.hashCode(specification);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypedNestedGraphTypeSpecification other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      typed,
      other.typed);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      graph,
      other.graph);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      specification,
      other.specification);
  }

  public TypedNestedGraphTypeSpecification withTyped(hydra.util.Maybe<java.lang.Void> typed) {
    return new TypedNestedGraphTypeSpecification(typed, graph, specification);
  }

  public TypedNestedGraphTypeSpecification withGraph(Boolean graph) {
    return new TypedNestedGraphTypeSpecification(typed, graph, specification);
  }

  public TypedNestedGraphTypeSpecification withSpecification(java.util.List<openGql.grammar.ElementTypeSpecification> specification) {
    return new TypedNestedGraphTypeSpecification(typed, graph, specification);
  }
}
