package hydra.graph;

/**
 * A graph element, having a name, data term (value), and schema term (type)
 */
public class Element<A> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/graph.Element");
  
  public final hydra.core.Name name;
  
  public final hydra.core.Term<A> schema;
  
  public final hydra.core.Term<A> data;
  
  public Element (hydra.core.Name name, hydra.core.Term<A> schema, hydra.core.Term<A> data) {
    this.name = name;
    this.schema = schema;
    this.data = data;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Element)) {
      return false;
    }
    Element o = (Element) (other);
    return name.equals(o.name) && schema.equals(o.schema) && data.equals(o.data);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * schema.hashCode() + 5 * data.hashCode();
  }
  
  public Element withName(hydra.core.Name name) {
    return new Element(name, schema, data);
  }
  
  public Element withSchema(hydra.core.Term<A> schema) {
    return new Element(name, schema, data);
  }
  
  public Element withData(hydra.core.Term<A> data) {
    return new Element(name, schema, data);
  }
}