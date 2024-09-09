// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphviz.dot;

import java.io.Serializable;

public class AttrStmt implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/graphviz/dot.AttrStmt");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public final hydra.ext.org.graphviz.dot.AttrType type;
  
  public final hydra.ext.org.graphviz.dot.AttrList attributes;
  
  public AttrStmt (hydra.ext.org.graphviz.dot.AttrType type, hydra.ext.org.graphviz.dot.AttrList attributes) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((attributes));
    this.type = type;
    this.attributes = attributes;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AttrStmt)) {
      return false;
    }
    AttrStmt o = (AttrStmt) (other);
    return type.equals(o.type) && attributes.equals(o.attributes);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * attributes.hashCode();
  }
  
  public AttrStmt withType(hydra.ext.org.graphviz.dot.AttrType type) {
    java.util.Objects.requireNonNull((type));
    return new AttrStmt(type, attributes);
  }
  
  public AttrStmt withAttributes(hydra.ext.org.graphviz.dot.AttrList attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new AttrStmt(type, attributes);
  }
}