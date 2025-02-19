// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class WithClause implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.WithClause");
  
  public static final hydra.core.Name FIELD_NAME_READING = new hydra.core.Name("reading");
  
  public static final hydra.core.Name FIELD_NAME_UPDATING = new hydra.core.Name("updating");
  
  public static final hydra.core.Name FIELD_NAME_WITH = new hydra.core.Name("with");
  
  public final java.util.List<hydra.ext.cypher.openCypher.ReadingClause> reading;
  
  public final java.util.List<hydra.ext.cypher.openCypher.UpdatingClause> updating;
  
  public final hydra.ext.cypher.openCypher.With with;
  
  public WithClause (java.util.List<hydra.ext.cypher.openCypher.ReadingClause> reading, java.util.List<hydra.ext.cypher.openCypher.UpdatingClause> updating, hydra.ext.cypher.openCypher.With with) {
    java.util.Objects.requireNonNull((reading));
    java.util.Objects.requireNonNull((updating));
    java.util.Objects.requireNonNull((with));
    this.reading = reading;
    this.updating = updating;
    this.with = with;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WithClause)) {
      return false;
    }
    WithClause o = (WithClause) (other);
    return reading.equals(o.reading) && updating.equals(o.updating) && with.equals(o.with);
  }
  
  @Override
  public int hashCode() {
    return 2 * reading.hashCode() + 3 * updating.hashCode() + 5 * with.hashCode();
  }
  
  public WithClause withReading(java.util.List<hydra.ext.cypher.openCypher.ReadingClause> reading) {
    java.util.Objects.requireNonNull((reading));
    return new WithClause(reading, updating, with);
  }
  
  public WithClause withUpdating(java.util.List<hydra.ext.cypher.openCypher.UpdatingClause> updating) {
    java.util.Objects.requireNonNull((updating));
    return new WithClause(reading, updating, with);
  }
  
  public WithClause withWith(hydra.ext.cypher.openCypher.With with) {
    java.util.Objects.requireNonNull((with));
    return new WithClause(reading, updating, with);
  }
}