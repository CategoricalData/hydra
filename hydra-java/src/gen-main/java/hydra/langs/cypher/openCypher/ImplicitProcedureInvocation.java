// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class ImplicitProcedureInvocation implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ImplicitProcedureInvocation");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.langs.cypher.openCypher.QualifiedName value;
  
  public ImplicitProcedureInvocation (hydra.langs.cypher.openCypher.QualifiedName value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ImplicitProcedureInvocation)) {
      return false;
    }
    ImplicitProcedureInvocation o = (ImplicitProcedureInvocation) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}