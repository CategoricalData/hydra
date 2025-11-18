// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import hydra.util.Maybe;

import java.io.Serializable;

public class SinglePartQuery implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.SinglePartQuery");
  
  public static final hydra.core.Name FIELD_NAME_READING = new hydra.core.Name("reading");
  
  public static final hydra.core.Name FIELD_NAME_UPDATING = new hydra.core.Name("updating");
  
  public static final hydra.core.Name FIELD_NAME_RETURN = new hydra.core.Name("return");
  
  public final java.util.List<hydra.ext.cypher.openCypher.ReadingClause> reading;
  
  public final java.util.List<hydra.ext.cypher.openCypher.UpdatingClause> updating;
  
  public final Maybe<Return> return_;
  
  public SinglePartQuery (java.util.List<hydra.ext.cypher.openCypher.ReadingClause> reading, java.util.List<hydra.ext.cypher.openCypher.UpdatingClause> updating, Maybe<Return> return_) {
    java.util.Objects.requireNonNull((reading));
    java.util.Objects.requireNonNull((updating));
    java.util.Objects.requireNonNull((return_));
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
  
  public SinglePartQuery withReading(java.util.List<hydra.ext.cypher.openCypher.ReadingClause> reading) {
    java.util.Objects.requireNonNull((reading));
    return new SinglePartQuery(reading, updating, return_);
  }
  
  public SinglePartQuery withUpdating(java.util.List<hydra.ext.cypher.openCypher.UpdatingClause> updating) {
    java.util.Objects.requireNonNull((updating));
    return new SinglePartQuery(reading, updating, return_);
  }
  
  public SinglePartQuery withReturn(Maybe<Return> return_) {
    java.util.Objects.requireNonNull((return_));
    return new SinglePartQuery(reading, updating, return_);
  }
}
