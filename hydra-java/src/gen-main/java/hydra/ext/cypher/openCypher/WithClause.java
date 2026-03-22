// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class WithClause implements Serializable, Comparable<WithClause> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.WithClause");

  public static final hydra.core.Name READING = new hydra.core.Name("reading");

  public static final hydra.core.Name UPDATING = new hydra.core.Name("updating");

  public static final hydra.core.Name WITH = new hydra.core.Name("with");

  public final hydra.util.ConsList<hydra.ext.cypher.openCypher.ReadingClause> reading;

  public final hydra.util.ConsList<hydra.ext.cypher.openCypher.UpdatingClause> updating;

  public final hydra.ext.cypher.openCypher.With with;

  public WithClause (hydra.util.ConsList<hydra.ext.cypher.openCypher.ReadingClause> reading, hydra.util.ConsList<hydra.ext.cypher.openCypher.UpdatingClause> updating, hydra.ext.cypher.openCypher.With with) {
    this.reading = reading;
    this.updating = updating;
    this.with = with;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WithClause)) {
      return false;
    }
    WithClause o = (WithClause) other;
    return java.util.Objects.equals(
      this.reading,
      o.reading) && java.util.Objects.equals(
      this.updating,
      o.updating) && java.util.Objects.equals(
      this.with,
      o.with);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(reading) + 3 * java.util.Objects.hashCode(updating) + 5 * java.util.Objects.hashCode(with);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(WithClause other) {
    int cmp = 0;
    cmp = ((Comparable) reading).compareTo(other.reading);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) updating).compareTo(other.updating);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) with).compareTo(other.with);
  }

  public WithClause withReading(hydra.util.ConsList<hydra.ext.cypher.openCypher.ReadingClause> reading) {
    return new WithClause(reading, updating, with);
  }

  public WithClause withUpdating(hydra.util.ConsList<hydra.ext.cypher.openCypher.UpdatingClause> updating) {
    return new WithClause(reading, updating, with);
  }

  public WithClause withWith(hydra.ext.cypher.openCypher.With with) {
    return new WithClause(reading, updating, with);
  }
}
