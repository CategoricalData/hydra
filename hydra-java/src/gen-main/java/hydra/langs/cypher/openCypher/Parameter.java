package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class Parameter implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.Parameter");
  
  public final String name;
  
  public final Integer index;
  
  public Parameter (String name, Integer index) {
    this.name = name;
    this.index = index;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Parameter)) {
      return false;
    }
    Parameter o = (Parameter) (other);
    return name.equals(o.name) && index.equals(o.index);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * index.hashCode();
  }
  
  public Parameter withName(String name) {
    return new Parameter(name, index);
  }
  
  public Parameter withIndex(Integer index) {
    return new Parameter(name, index);
  }
}