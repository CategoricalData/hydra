package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class UpdatingQuery implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.UpdatingQuery");
  
  public final java.util.List<hydra.langs.cypher.openCypher.ReadingClause> reading;
  
  public final java.util.List<hydra.langs.cypher.openCypher.UpdatingClause> updating;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.ProjectionBody> return_;
  
  public UpdatingQuery (java.util.List<hydra.langs.cypher.openCypher.ReadingClause> reading, java.util.List<hydra.langs.cypher.openCypher.UpdatingClause> updating, java.util.Optional<hydra.langs.cypher.openCypher.ProjectionBody> return_) {
    this.reading = reading;
    this.updating = updating;
    this.return_ = return_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UpdatingQuery)) {
      return false;
    }
    UpdatingQuery o = (UpdatingQuery) (other);
    return reading.equals(o.reading) && updating.equals(o.updating) && return_.equals(o.return_);
  }
  
  @Override
  public int hashCode() {
    return 2 * reading.hashCode() + 3 * updating.hashCode() + 5 * return_.hashCode();
  }
  
  public UpdatingQuery withReading(java.util.List<hydra.langs.cypher.openCypher.ReadingClause> reading) {
    return new UpdatingQuery(reading, updating, return_);
  }
  
  public UpdatingQuery withUpdating(java.util.List<hydra.langs.cypher.openCypher.UpdatingClause> updating) {
    return new UpdatingQuery(reading, updating, return_);
  }
  
  public UpdatingQuery withReturn(java.util.Optional<hydra.langs.cypher.openCypher.ProjectionBody> return_) {
    return new UpdatingQuery(reading, updating, return_);
  }
}