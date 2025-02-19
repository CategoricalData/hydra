// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class TraversalStrategy implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalStrategy");
  
  public static final hydra.core.Name FIELD_NAME_NEW = new hydra.core.Name("new");
  
  public static final hydra.core.Name FIELD_NAME_CLASS = new hydra.core.Name("class");
  
  public static final hydra.core.Name FIELD_NAME_CONFIGURATIONS = new hydra.core.Name("configurations");
  
  public final Boolean new_;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.Identifier class_;
  
  public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.Configuration> configurations;
  
  public TraversalStrategy (Boolean new_, hydra.ext.org.apache.tinkerpop.gremlin.Identifier class_, java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.Configuration> configurations) {
    java.util.Objects.requireNonNull((new_));
    java.util.Objects.requireNonNull((class_));
    java.util.Objects.requireNonNull((configurations));
    this.new_ = new_;
    this.class_ = class_;
    this.configurations = configurations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TraversalStrategy)) {
      return false;
    }
    TraversalStrategy o = (TraversalStrategy) (other);
    return new_.equals(o.new_) && class_.equals(o.class_) && configurations.equals(o.configurations);
  }
  
  @Override
  public int hashCode() {
    return 2 * new_.hashCode() + 3 * class_.hashCode() + 5 * configurations.hashCode();
  }
  
  public TraversalStrategy withNew(Boolean new_) {
    java.util.Objects.requireNonNull((new_));
    return new TraversalStrategy(new_, class_, configurations);
  }
  
  public TraversalStrategy withClass(hydra.ext.org.apache.tinkerpop.gremlin.Identifier class_) {
    java.util.Objects.requireNonNull((class_));
    return new TraversalStrategy(new_, class_, configurations);
  }
  
  public TraversalStrategy withConfigurations(java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.Configuration> configurations) {
    java.util.Objects.requireNonNull((configurations));
    return new TraversalStrategy(new_, class_, configurations);
  }
}