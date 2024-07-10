// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class WithClause implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.WithClause");
  
  public final java.util.List<hydra.langs.cypher.openCypher.ReadingClause> reading;
  
  public final java.util.List<hydra.langs.cypher.openCypher.UpdatingClause> updating;
  
  public final hydra.langs.cypher.openCypher.With with;
  
  public WithClause (java.util.List<hydra.langs.cypher.openCypher.ReadingClause> reading, java.util.List<hydra.langs.cypher.openCypher.UpdatingClause> updating, hydra.langs.cypher.openCypher.With with) {
    if (reading == null) {
      throw new IllegalArgumentException("null value for 'reading' argument");
    }
    if (updating == null) {
      throw new IllegalArgumentException("null value for 'updating' argument");
    }
    if (with == null) {
      throw new IllegalArgumentException("null value for 'with' argument");
    }
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
  
  public WithClause withReading(java.util.List<hydra.langs.cypher.openCypher.ReadingClause> reading) {
    if (reading == null) {
      throw new IllegalArgumentException("null value for 'reading' argument");
    }
    return new WithClause(reading, updating, with);
  }
  
  public WithClause withUpdating(java.util.List<hydra.langs.cypher.openCypher.UpdatingClause> updating) {
    if (updating == null) {
      throw new IllegalArgumentException("null value for 'updating' argument");
    }
    return new WithClause(reading, updating, with);
  }
  
  public WithClause withWith(hydra.langs.cypher.openCypher.With with) {
    if (with == null) {
      throw new IllegalArgumentException("null value for 'with' argument");
    }
    return new WithClause(reading, updating, with);
  }
}