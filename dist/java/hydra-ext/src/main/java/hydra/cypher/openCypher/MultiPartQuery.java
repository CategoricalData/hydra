// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class MultiPartQuery implements Serializable, Comparable<MultiPartQuery> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.MultiPartQuery");

  public static final hydra.core.Name WITH = new hydra.core.Name("with");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final java.util.List<hydra.cypher.openCypher.WithClause> with;

  public final hydra.cypher.openCypher.SinglePartQuery body;

  public MultiPartQuery (java.util.List<hydra.cypher.openCypher.WithClause> with, hydra.cypher.openCypher.SinglePartQuery body) {
    this.with = with;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiPartQuery)) {
      return false;
    }
    MultiPartQuery o = (MultiPartQuery) other;
    return java.util.Objects.equals(
      this.with,
      o.with) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(with) + 3 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MultiPartQuery other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      with,
      other.with);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public MultiPartQuery withWith(java.util.List<hydra.cypher.openCypher.WithClause> with) {
    return new MultiPartQuery(with, body);
  }

  public MultiPartQuery withBody(hydra.cypher.openCypher.SinglePartQuery body) {
    return new MultiPartQuery(with, body);
  }
}
