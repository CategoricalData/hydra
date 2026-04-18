// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class SinglePartQuery implements Serializable, Comparable<SinglePartQuery> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.SinglePartQuery");

  public static final hydra.core.Name READING = new hydra.core.Name("reading");

  public static final hydra.core.Name UPDATING = new hydra.core.Name("updating");

  public static final hydra.core.Name RETURN = new hydra.core.Name("return");

  public final java.util.List<hydra.cypher.openCypher.ReadingClause> reading;

  public final java.util.List<hydra.cypher.openCypher.UpdatingClause> updating;

  public final hydra.util.Maybe<hydra.cypher.openCypher.Return> return_;

  public SinglePartQuery (java.util.List<hydra.cypher.openCypher.ReadingClause> reading, java.util.List<hydra.cypher.openCypher.UpdatingClause> updating, hydra.util.Maybe<hydra.cypher.openCypher.Return> return_) {
    this.reading = reading;
    this.updating = updating;
    this.return_ = return_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SinglePartQuery)) {
      return false;
    }
    SinglePartQuery o = (SinglePartQuery) other;
    return java.util.Objects.equals(
      this.reading,
      o.reading) && java.util.Objects.equals(
      this.updating,
      o.updating) && java.util.Objects.equals(
      this.return_,
      o.return_);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(reading) + 3 * java.util.Objects.hashCode(updating) + 5 * java.util.Objects.hashCode(return_);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SinglePartQuery other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      reading,
      other.reading);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      updating,
      other.updating);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      return_,
      other.return_);
  }

  public SinglePartQuery withReading(java.util.List<hydra.cypher.openCypher.ReadingClause> reading) {
    return new SinglePartQuery(reading, updating, return_);
  }

  public SinglePartQuery withUpdating(java.util.List<hydra.cypher.openCypher.UpdatingClause> updating) {
    return new SinglePartQuery(reading, updating, return_);
  }

  public SinglePartQuery withReturn(hydra.util.Maybe<hydra.cypher.openCypher.Return> return_) {
    return new SinglePartQuery(reading, updating, return_);
  }
}
