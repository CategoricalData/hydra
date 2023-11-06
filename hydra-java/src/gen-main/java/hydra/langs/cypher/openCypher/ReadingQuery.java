package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class ReadingQuery implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ReadingQuery");
  
  public final java.util.List<hydra.langs.cypher.openCypher.ReadingClause> reading;
  
  public final hydra.langs.cypher.openCypher.ProjectionBody return_;
  
  public ReadingQuery (java.util.List<hydra.langs.cypher.openCypher.ReadingClause> reading, hydra.langs.cypher.openCypher.ProjectionBody return_) {
    this.reading = reading;
    this.return_ = return_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ReadingQuery)) {
      return false;
    }
    ReadingQuery o = (ReadingQuery) (other);
    return reading.equals(o.reading) && return_.equals(o.return_);
  }
  
  @Override
  public int hashCode() {
    return 2 * reading.hashCode() + 3 * return_.hashCode();
  }
  
  public ReadingQuery withReading(java.util.List<hydra.langs.cypher.openCypher.ReadingClause> reading) {
    return new ReadingQuery(reading, return_);
  }
  
  public ReadingQuery withReturn(hydra.langs.cypher.openCypher.ProjectionBody return_) {
    return new ReadingQuery(reading, return_);
  }
}