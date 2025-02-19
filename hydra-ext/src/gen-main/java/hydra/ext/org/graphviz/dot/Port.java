// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphviz.dot;

import java.io.Serializable;

public class Port implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphviz.dot.Port");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_POSITION = new hydra.core.Name("position");
  
  public final hydra.util.Opt<hydra.ext.org.graphviz.dot.Id> id;
  
  public final hydra.util.Opt<hydra.ext.org.graphviz.dot.CompassPt> position;
  
  public Port (hydra.util.Opt<hydra.ext.org.graphviz.dot.Id> id, hydra.util.Opt<hydra.ext.org.graphviz.dot.CompassPt> position) {
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((position));
    this.id = id;
    this.position = position;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Port)) {
      return false;
    }
    Port o = (Port) (other);
    return id.equals(o.id) && position.equals(o.position);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * position.hashCode();
  }
  
  public Port withId(hydra.util.Opt<hydra.ext.org.graphviz.dot.Id> id) {
    java.util.Objects.requireNonNull((id));
    return new Port(id, position);
  }
  
  public Port withPosition(hydra.util.Opt<hydra.ext.org.graphviz.dot.CompassPt> position) {
    java.util.Objects.requireNonNull((position));
    return new Port(id, position);
  }
}