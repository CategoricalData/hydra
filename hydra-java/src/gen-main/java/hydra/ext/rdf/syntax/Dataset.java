// Note: this is an automatically generated file. Do not edit.

package hydra.ext.rdf.syntax;

import java.io.Serializable;

public class Dataset implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/rdf/syntax.Dataset");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.Set<hydra.ext.rdf.syntax.Quad> value;
  
  public Dataset (java.util.Set<hydra.ext.rdf.syntax.Quad> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Dataset)) {
      return false;
    }
    Dataset o = (Dataset) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}
