package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class SinglePartQuery implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.SinglePartQuery");
  
  public final java.util.List<hydra.langs.cypher.openCypher.ReadingClause> reading;
  
  public final java.util.List<hydra.langs.cypher.openCypher.UpdatingClause> updating;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Return> return_;
  
  public SinglePartQuery (java.util.List<hydra.langs.cypher.openCypher.ReadingClause> reading, java.util.List<hydra.langs.cypher.openCypher.UpdatingClause> updating, java.util.Optional<hydra.langs.cypher.openCypher.Return> return_) {
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
    return new SinglePartQuery(reading, updating, return_);
  }
  
  public SinglePartQuery withUpdating(java.util.List<hydra.langs.cypher.openCypher.UpdatingClause> updating) {
    return new SinglePartQuery(reading, updating, return_);
  }
  
  public SinglePartQuery withReturn(java.util.Optional<hydra.langs.cypher.openCypher.Return> return_) {
    return new SinglePartQuery(reading, updating, return_);
  }
}