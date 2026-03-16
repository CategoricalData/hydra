// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class TraversalStrategy implements Serializable, Comparable<TraversalStrategy> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalStrategy");
  
  public static final hydra.core.Name NEW = new hydra.core.Name("new");
  
  public static final hydra.core.Name CLASS = new hydra.core.Name("class");
  
  public static final hydra.core.Name CONFIGURATIONS = new hydra.core.Name("configurations");
  
  public final Boolean new_;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.Identifier class_;
  
  public final hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.Configuration> configurations;
  
  public TraversalStrategy (Boolean new_, hydra.ext.org.apache.tinkerpop.gremlin.Identifier class_, hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.Configuration> configurations) {
    this.new_ = new_;
    this.class_ = class_;
    this.configurations = configurations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TraversalStrategy)) {
      return false;
    }
    TraversalStrategy o = (TraversalStrategy) other;
    return java.util.Objects.equals(
      this.new_,
      o.new_) && java.util.Objects.equals(
      this.class_,
      o.class_) && java.util.Objects.equals(
      this.configurations,
      o.configurations);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(new_) + 3 * java.util.Objects.hashCode(class_) + 5 * java.util.Objects.hashCode(configurations);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TraversalStrategy other) {
    int cmp = 0;
    cmp = ((Comparable) new_).compareTo(other.new_);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) class_).compareTo(other.class_);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) configurations).compareTo(other.configurations);
  }
  
  public TraversalStrategy withNew(Boolean new_) {
    return new TraversalStrategy(new_, class_, configurations);
  }
  
  public TraversalStrategy withClass(hydra.ext.org.apache.tinkerpop.gremlin.Identifier class_) {
    return new TraversalStrategy(new_, class_, configurations);
  }
  
  public TraversalStrategy withConfigurations(hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.Configuration> configurations) {
    return new TraversalStrategy(new_, class_, configurations);
  }
}
