// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class StructureVertex implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.StructureVertex");
  
  public final Boolean new_;
  
  public final hydra.langs.tinkerpop.gremlin.GenericLiteralArgument id;
  
  public final hydra.langs.tinkerpop.gremlin.StringArgument label;
  
  public StructureVertex (Boolean new_, hydra.langs.tinkerpop.gremlin.GenericLiteralArgument id, hydra.langs.tinkerpop.gremlin.StringArgument label) {
    java.util.Objects.requireNonNull((new_));
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((label));
    this.new_ = new_;
    this.id = id;
    this.label = label;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StructureVertex)) {
      return false;
    }
    StructureVertex o = (StructureVertex) (other);
    return new_.equals(o.new_) && id.equals(o.id) && label.equals(o.label);
  }
  
  @Override
  public int hashCode() {
    return 2 * new_.hashCode() + 3 * id.hashCode() + 5 * label.hashCode();
  }
  
  public StructureVertex withNew(Boolean new_) {
    java.util.Objects.requireNonNull((new_));
    return new StructureVertex(new_, id, label);
  }
  
  public StructureVertex withId(hydra.langs.tinkerpop.gremlin.GenericLiteralArgument id) {
    java.util.Objects.requireNonNull((id));
    return new StructureVertex(new_, id, label);
  }
  
  public StructureVertex withLabel(hydra.langs.tinkerpop.gremlin.StringArgument label) {
    java.util.Objects.requireNonNull((label));
    return new StructureVertex(new_, id, label);
  }
}