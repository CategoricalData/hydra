// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class TypedGraphTypeReference implements Serializable, Comparable<TypedGraphTypeReference> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TypedGraphTypeReference");

  public static final hydra.core.Name TYPED = new hydra.core.Name("typed");

  public static final hydra.core.Name REFERENCE = new hydra.core.Name("reference");

  public final hydra.util.Maybe<java.lang.Void> typed;

  public final openGql.grammar.GraphTypeReference reference;

  public TypedGraphTypeReference (hydra.util.Maybe<java.lang.Void> typed, openGql.grammar.GraphTypeReference reference) {
    this.typed = typed;
    this.reference = reference;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypedGraphTypeReference)) {
      return false;
    }
    TypedGraphTypeReference o = (TypedGraphTypeReference) other;
    return java.util.Objects.equals(
      this.typed,
      o.typed) && java.util.Objects.equals(
      this.reference,
      o.reference);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typed) + 3 * java.util.Objects.hashCode(reference);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypedGraphTypeReference other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      typed,
      other.typed);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      reference,
      other.reference);
  }

  public TypedGraphTypeReference withTyped(hydra.util.Maybe<java.lang.Void> typed) {
    return new TypedGraphTypeReference(typed, reference);
  }

  public TypedGraphTypeReference withReference(openGql.grammar.GraphTypeReference reference) {
    return new TypedGraphTypeReference(typed, reference);
  }
}
