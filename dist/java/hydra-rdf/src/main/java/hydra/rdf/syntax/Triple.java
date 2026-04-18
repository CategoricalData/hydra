// Note: this is an automatically generated file. Do not edit.

package hydra.rdf.syntax;

import java.io.Serializable;

/**
 * An RDF triple defined by a subject, predicate, and object
 */
public class Triple implements Serializable, Comparable<Triple> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.rdf.syntax.Triple");

  public static final hydra.core.Name SUBJECT = new hydra.core.Name("subject");

  public static final hydra.core.Name PREDICATE = new hydra.core.Name("predicate");

  public static final hydra.core.Name OBJECT = new hydra.core.Name("object");

  public final hydra.rdf.syntax.Resource subject;

  public final hydra.rdf.syntax.Iri predicate;

  public final hydra.rdf.syntax.Node object;

  public Triple (hydra.rdf.syntax.Resource subject, hydra.rdf.syntax.Iri predicate, hydra.rdf.syntax.Node object) {
    this.subject = subject;
    this.predicate = predicate;
    this.object = object;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Triple)) {
      return false;
    }
    Triple o = (Triple) other;
    return java.util.Objects.equals(
      this.subject,
      o.subject) && java.util.Objects.equals(
      this.predicate,
      o.predicate) && java.util.Objects.equals(
      this.object,
      o.object);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(subject) + 3 * java.util.Objects.hashCode(predicate) + 5 * java.util.Objects.hashCode(object);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Triple other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      subject,
      other.subject);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      predicate,
      other.predicate);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      object,
      other.object);
  }

  public Triple withSubject(hydra.rdf.syntax.Resource subject) {
    return new Triple(subject, predicate, object);
  }

  public Triple withPredicate(hydra.rdf.syntax.Iri predicate) {
    return new Triple(subject, predicate, object);
  }

  public Triple withObject(hydra.rdf.syntax.Node object) {
    return new Triple(subject, predicate, object);
  }
}
