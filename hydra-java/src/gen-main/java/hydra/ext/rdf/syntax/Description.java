package hydra.ext.rdf.syntax;

/**
 * A graph of RDF statements together with a distinguished subject
 */
public class Description {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/rdf/syntax.Description");
  
  public final hydra.ext.rdf.syntax.Node subject;
  
  public final hydra.ext.rdf.syntax.Graph triples;
  
  public Description (hydra.ext.rdf.syntax.Node subject, hydra.ext.rdf.syntax.Graph triples) {
    this.subject = subject;
    this.triples = triples;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Description)) {
      return false;
    }
    Description o = (Description) (other);
    return subject.equals(o.subject) && triples.equals(o.triples);
  }
  
  @Override
  public int hashCode() {
    return 2 * subject.hashCode() + 3 * triples.hashCode();
  }
  
  public Description withSubject(hydra.ext.rdf.syntax.Node subject) {
    return new Description(subject, triples);
  }
  
  public Description withTriples(hydra.ext.rdf.syntax.Graph triples) {
    return new Description(subject, triples);
  }
}