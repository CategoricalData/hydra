// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class SinglePartQuery implements Serializable, Comparable<SinglePartQuery> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.SinglePartQuery");

  public static final hydra.core.Name READING = new hydra.core.Name("reading");

  public static final hydra.core.Name UPDATING = new hydra.core.Name("updating");

  public static final hydra.core.Name RETURN = new hydra.core.Name("return");

  public final hydra.util.ConsList<hydra.ext.cypher.openCypher.ReadingClause> reading;

  public final hydra.util.ConsList<hydra.ext.cypher.openCypher.UpdatingClause> updating;

  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.Return> return_;

  public SinglePartQuery (hydra.util.ConsList<hydra.ext.cypher.openCypher.ReadingClause> reading, hydra.util.ConsList<hydra.ext.cypher.openCypher.UpdatingClause> updating, hydra.util.Maybe<hydra.ext.cypher.openCypher.Return> return_) {
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
    cmp = ((Comparable) reading).compareTo(other.reading);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) updating).compareTo(other.updating);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) return_).compareTo(other.return_);
  }

  public SinglePartQuery withReading(hydra.util.ConsList<hydra.ext.cypher.openCypher.ReadingClause> reading) {
    return new SinglePartQuery(reading, updating, return_);
  }

  public SinglePartQuery withUpdating(hydra.util.ConsList<hydra.ext.cypher.openCypher.UpdatingClause> updating) {
    return new SinglePartQuery(reading, updating, return_);
  }

  public SinglePartQuery withReturn(hydra.util.Maybe<hydra.ext.cypher.openCypher.Return> return_) {
    return new SinglePartQuery(reading, updating, return_);
  }
}
