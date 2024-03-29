package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class StringLiteral implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.StringLiteral");
  
  public final String value;
  
  public StringLiteral (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringLiteral)) {
      return false;
    }
    StringLiteral o = (StringLiteral) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}