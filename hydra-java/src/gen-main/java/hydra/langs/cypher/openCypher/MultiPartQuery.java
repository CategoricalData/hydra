package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class MultiPartQuery implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.MultiPartQuery");
  
  public final java.util.List<hydra.langs.cypher.openCypher.ReadingClause> reading;
  
  public final java.util.List<hydra.langs.cypher.openCypher.UpdatingClause> updating;
  
  public final hydra.langs.cypher.openCypher.With with;
  
  public final hydra.langs.cypher.openCypher.SinglePartQuery query;
  
  public MultiPartQuery (java.util.List<hydra.langs.cypher.openCypher.ReadingClause> reading, java.util.List<hydra.langs.cypher.openCypher.UpdatingClause> updating, hydra.langs.cypher.openCypher.With with, hydra.langs.cypher.openCypher.SinglePartQuery query) {
    this.reading = reading;
    this.updating = updating;
    this.with = with;
    this.query = query;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiPartQuery)) {
      return false;
    }
    MultiPartQuery o = (MultiPartQuery) (other);
    return reading.equals(o.reading) && updating.equals(o.updating) && with.equals(o.with) && query.equals(o.query);
  }
  
  @Override
  public int hashCode() {
    return 2 * reading.hashCode() + 3 * updating.hashCode() + 5 * with.hashCode() + 7 * query.hashCode();
  }
  
  public MultiPartQuery withReading(java.util.List<hydra.langs.cypher.openCypher.ReadingClause> reading) {
    return new MultiPartQuery(reading, updating, with, query);
  }
  
  public MultiPartQuery withUpdating(java.util.List<hydra.langs.cypher.openCypher.UpdatingClause> updating) {
    return new MultiPartQuery(reading, updating, with, query);
  }
  
  public MultiPartQuery withWith(hydra.langs.cypher.openCypher.With with) {
    return new MultiPartQuery(reading, updating, with, query);
  }
  
  public MultiPartQuery withQuery(hydra.langs.cypher.openCypher.SinglePartQuery query) {
    return new MultiPartQuery(reading, updating, with, query);
  }
}