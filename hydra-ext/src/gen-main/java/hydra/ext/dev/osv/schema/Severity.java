// Note: this is an automatically generated file. Do not edit.

package hydra.ext.dev.osv.schema;

import java.io.Serializable;

public class Severity implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/dev/osv/schema.Severity");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_SCORE = new hydra.core.Name("score");
  
  public final hydra.ext.dev.osv.schema.SeverityType type;
  
  public final hydra.ext.dev.osv.schema.SeverityScore score;
  
  public Severity (hydra.ext.dev.osv.schema.SeverityType type, hydra.ext.dev.osv.schema.SeverityScore score) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((score));
    this.type = type;
    this.score = score;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Severity)) {
      return false;
    }
    Severity o = (Severity) (other);
    return type.equals(o.type) && score.equals(o.score);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * score.hashCode();
  }
  
  public Severity withType(hydra.ext.dev.osv.schema.SeverityType type) {
    java.util.Objects.requireNonNull((type));
    return new Severity(type, score);
  }
  
  public Severity withScore(hydra.ext.dev.osv.schema.SeverityScore score) {
    java.util.Objects.requireNonNull((score));
    return new Severity(type, score);
  }
}