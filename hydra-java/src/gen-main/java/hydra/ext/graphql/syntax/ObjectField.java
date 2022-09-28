package hydra.ext.graphql.syntax;

public class ObjectField {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.ObjectField");
  
  public final hydra.ext.graphql.syntax.Name name;
  
  public final hydra.ext.graphql.syntax.Value value;
  
  public ObjectField (hydra.ext.graphql.syntax.Name name, hydra.ext.graphql.syntax.Value value) {
    this.name = name;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectField)) {
      return false;
    }
    ObjectField o = (ObjectField) (other);
    return name.equals(o.name) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * value.hashCode();
  }
  
  public ObjectField withName(hydra.ext.graphql.syntax.Name name) {
    return new ObjectField(name, value);
  }
  
  public ObjectField withValue(hydra.ext.graphql.syntax.Value value) {
    return new ObjectField(name, value);
  }
}