package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class CaseAlternative implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.CaseAlternative");
  
  public final hydra.langs.cypher.openCypher.Expression when;
  
  public final hydra.langs.cypher.openCypher.Expression then;
  
  public CaseAlternative (hydra.langs.cypher.openCypher.Expression when, hydra.langs.cypher.openCypher.Expression then) {
    this.when = when;
    this.then = then;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseAlternative)) {
      return false;
    }
    CaseAlternative o = (CaseAlternative) (other);
    return when.equals(o.when) && then.equals(o.then);
  }
  
  @Override
  public int hashCode() {
    return 2 * when.hashCode() + 3 * then.hashCode();
  }
  
  public CaseAlternative withWhen(hydra.langs.cypher.openCypher.Expression when) {
    return new CaseAlternative(when, then);
  }
  
  public CaseAlternative withThen(hydra.langs.cypher.openCypher.Expression then) {
    return new CaseAlternative(when, then);
  }
}