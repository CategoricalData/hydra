// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class EdgeTypePhrase implements Serializable, Comparable<EdgeTypePhrase> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EdgeTypePhrase");

  public static final hydra.core.Name KIND = new hydra.core.Name("kind");

  public static final hydra.core.Name SYNONYM = new hydra.core.Name("synonym");

  public static final hydra.core.Name TYPE_NAME_AND_FILLER = new hydra.core.Name("typeNameAndFiller");

  public static final hydra.core.Name ENDPOINT_PAIR = new hydra.core.Name("endpointPair");

  public final openGql.grammar.EdgeKind kind;

  public final openGql.grammar.EdgeSynonym synonym;

  public final openGql.grammar.EdgeTypePhraseFiller typeNameAndFiller;

  public final openGql.grammar.EndpointPair endpointPair;

  public EdgeTypePhrase (openGql.grammar.EdgeKind kind, openGql.grammar.EdgeSynonym synonym, openGql.grammar.EdgeTypePhraseFiller typeNameAndFiller, openGql.grammar.EndpointPair endpointPair) {
    this.kind = kind;
    this.synonym = synonym;
    this.typeNameAndFiller = typeNameAndFiller;
    this.endpointPair = endpointPair;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeTypePhrase)) {
      return false;
    }
    EdgeTypePhrase o = (EdgeTypePhrase) other;
    return java.util.Objects.equals(
      this.kind,
      o.kind) && java.util.Objects.equals(
      this.synonym,
      o.synonym) && java.util.Objects.equals(
      this.typeNameAndFiller,
      o.typeNameAndFiller) && java.util.Objects.equals(
      this.endpointPair,
      o.endpointPair);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(kind) + 3 * java.util.Objects.hashCode(synonym) + 5 * java.util.Objects.hashCode(typeNameAndFiller) + 7 * java.util.Objects.hashCode(endpointPair);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EdgeTypePhrase other) {
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
    cmp = hydra.util.Comparing.compare(
      typeNameAndFiller,
      other.typeNameAndFiller);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      endpointPair,
      other.endpointPair);
  }

  public EdgeTypePhrase withKind(openGql.grammar.EdgeKind kind) {
    return new EdgeTypePhrase(kind, synonym, typeNameAndFiller, endpointPair);
  }

  public EdgeTypePhrase withSynonym(openGql.grammar.EdgeSynonym synonym) {
    return new EdgeTypePhrase(kind, synonym, typeNameAndFiller, endpointPair);
  }

  public EdgeTypePhrase withTypeNameAndFiller(openGql.grammar.EdgeTypePhraseFiller typeNameAndFiller) {
    return new EdgeTypePhrase(kind, synonym, typeNameAndFiller, endpointPair);
  }

  public EdgeTypePhrase withEndpointPair(openGql.grammar.EndpointPair endpointPair) {
    return new EdgeTypePhrase(kind, synonym, typeNameAndFiller, endpointPair);
  }
}
