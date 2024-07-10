// Note: this is an automatically generated file. Do not edit.

package hydra.phantoms;

import java.io.Serializable;

/**
 * An association with a named term with a phantom type
 */
public class Definition<A> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/phantoms.Definition");
  
  public final hydra.core.Name name;
  
  public final hydra.phantoms.Datum<A> datum;
  
  public Definition (hydra.core.Name name, hydra.phantoms.Datum<A> datum) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (datum == null) {
      throw new IllegalArgumentException("null value for 'datum' argument");
    }
    this.name = name;
    this.datum = datum;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Definition)) {
      return false;
    }
    Definition o = (Definition) (other);
    return name.equals(o.name) && datum.equals(o.datum);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * datum.hashCode();
  }
  
  public Definition withName(hydra.core.Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new Definition(name, datum);
  }
  
  public Definition withDatum(hydra.phantoms.Datum<A> datum) {
    if (datum == null) {
      throw new IllegalArgumentException("null value for 'datum' argument");
    }
    return new Definition(name, datum);
  }
}