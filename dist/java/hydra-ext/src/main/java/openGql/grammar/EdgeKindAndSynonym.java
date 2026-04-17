// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class EdgeKindAndSynonym implements Serializable, Comparable<EdgeKindAndSynonym> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EdgeKindAndSynonym");

  public static final hydra.core.Name KIND = new hydra.core.Name("kind");

  public static final hydra.core.Name SYNONYM = new hydra.core.Name("synonym");

  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("typeName");

  public final hydra.util.Maybe<openGql.grammar.EdgeKind> kind;

  public final openGql.grammar.EdgeSynonym synonym;

  public final hydra.util.Maybe<String> typeName;

  public EdgeKindAndSynonym (hydra.util.Maybe<openGql.grammar.EdgeKind> kind, openGql.grammar.EdgeSynonym synonym, hydra.util.Maybe<String> typeName) {
    this.kind = kind;
    this.synonym = synonym;
    this.typeName = typeName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeKindAndSynonym)) {
      return false;
    }
    EdgeKindAndSynonym o = (EdgeKindAndSynonym) other;
    return java.util.Objects.equals(
      this.kind,
      o.kind) && java.util.Objects.equals(
      this.synonym,
      o.synonym) && java.util.Objects.equals(
      this.typeName,
      o.typeName);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(kind) + 3 * java.util.Objects.hashCode(synonym) + 5 * java.util.Objects.hashCode(typeName);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EdgeKindAndSynonym other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      kind,
      other.kind);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      synonym,
      other.synonym);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      typeName,
      other.typeName);
  }

  public EdgeKindAndSynonym withKind(hydra.util.Maybe<openGql.grammar.EdgeKind> kind) {
    return new EdgeKindAndSynonym(kind, synonym, typeName);
  }

  public EdgeKindAndSynonym withSynonym(openGql.grammar.EdgeSynonym synonym) {
    return new EdgeKindAndSynonym(kind, synonym, typeName);
  }

  public EdgeKindAndSynonym withTypeName(hydra.util.Maybe<String> typeName) {
    return new EdgeKindAndSynonym(kind, synonym, typeName);
  }
}
