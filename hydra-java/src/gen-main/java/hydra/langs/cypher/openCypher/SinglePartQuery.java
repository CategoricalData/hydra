// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class SinglePartQuery implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.SinglePartQuery");
  
  public final java.util.List<hydra.langs.cypher.openCypher.ReadingClause> reading;
  
  public final java.util.List<hydra.langs.cypher.openCypher.UpdatingClause> updating;
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.Return> return_;
  
  public SinglePartQuery (java.util.List<hydra.langs.cypher.openCypher.ReadingClause> reading, java.util.List<hydra.langs.cypher.openCypher.UpdatingClause> updating, hydra.util.Opt<hydra.langs.cypher.openCypher.Return> return_) {
    if (reading == null) {
      throw new IllegalArgumentException("null value for 'reading' argument");
    }
    if (updating == null) {
      throw new IllegalArgumentException("null value for 'updating' argument");
    }
    if (return_ == null) {
      throw new IllegalArgumentException("null value for 'return' argument");
    }
    this.reading = reading;
    this.updating = updating;
    this.return_ = return_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SinglePartQuery)) {
      return false;
    }
    SinglePartQuery o = (SinglePartQuery) (other);
    return reading.equals(o.reading) && updating.equals(o.updating) && return_.equals(o.return_);
  }
  
  @Override
  public int hashCode() {
    return 2 * reading.hashCode() + 3 * updating.hashCode() + 5 * return_.hashCode();
  }
  
  public SinglePartQuery withReading(java.util.List<hydra.langs.cypher.openCypher.ReadingClause> reading) {
    if (reading == null) {
      throw new IllegalArgumentException("null value for 'reading' argument");
    }
    return new SinglePartQuery(reading, updating, return_);
  }
  
  public SinglePartQuery withUpdating(java.util.List<hydra.langs.cypher.openCypher.UpdatingClause> updating) {
    if (updating == null) {
      throw new IllegalArgumentException("null value for 'updating' argument");
    }
    return new SinglePartQuery(reading, updating, return_);
  }
  
  public SinglePartQuery withReturn(hydra.util.Opt<hydra.langs.cypher.openCypher.Return> return_) {
    if (return_ == null) {
      throw new IllegalArgumentException("null value for 'return' argument");
    }
    return new SinglePartQuery(reading, updating, return_);
  }
}