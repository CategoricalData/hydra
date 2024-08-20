package hydra.ext.rdf;

import hydra.ext.rdf.syntax.Description;
import hydra.ext.rdf.syntax.Graph;
import hydra.ext.rdf.syntax.Node;
import hydra.ext.rdf.syntax.Triple;
import java.io.ByteArrayOutputStream;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import org.eclipse.rdf4j.model.BNode;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.Value;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.eclipse.rdf4j.rio.Rio;

/**
 * RDF4j-based serialization for Hydra RDF graphs.
 */
public interface Serde {
  ValueFactory valueFactory = SimpleValueFactory.getInstance();

  /**
   * Serialize a list of RDF descriptions.
   */
  static String toNtriples(List<Description> descriptions) {
    List<Triple> triples = descriptions.stream().flatMap(
        description -> description.graph.value.stream()).collect(Collectors.toList());
    return Serde.toNtriples(triples);
  }

  /**
   * Serialize an RDF graph.
   */
  static String toNtriples(Graph graph) {
    return toNtriples(graph.value);
  }

  /**
   * Serialize a collection of RDF triples.
   */
  static String toNtriples(Collection<Triple> triples) {
    ByteArrayOutputStream bos = new ByteArrayOutputStream();
    List<Statement> model = triples.stream().map(Serde::tripleToStatement).collect(Collectors.toList());
    Rio.write(model, bos, RDFFormat.TURTLE);

    return bos.toString();
  }

  /**
   * Convert a blank node to its RDF4j equivalent.
   */
  static BNode bnode(hydra.ext.rdf.syntax.BlankNode r) {
    return valueFactory.createBNode(r.value);
  }

  /**
   * Convert an IRI to its RDF4j equivalent.
   */
  static IRI iri(hydra.ext.rdf.syntax.Iri r) {
    return valueFactory.createIRI(r.value);
  }

  /**
   * Convert an RDF literal to its RDF4j equivalent.
   */
  static Literal literal(hydra.ext.rdf.syntax.Literal r) {
    return valueFactory.createLiteral(r.lexicalForm, iri(r.datatypeIri));
  }

  /**
   * Convert an RDF resource to its RDF4j equivalent.
   */
  static Resource resource(hydra.ext.rdf.syntax.Resource r) {
    return r.accept(new hydra.ext.rdf.syntax.Resource.Visitor<Resource>() {
      @Override
      public Resource visit(hydra.ext.rdf.syntax.Resource.Iri instance) {
        return iri(instance.value);
      }

      @Override
      public Resource visit(hydra.ext.rdf.syntax.Resource.Bnode instance) {
        return bnode(instance.value);
      }
    });
  }

  /**
   * Convert an RDF triple to its RDF4j equivalent.
   */
  static Statement tripleToStatement(Triple triple) {
    Resource subj = resource(triple.subject);
    IRI pred = iri(triple.predicate);
    Value obj = value(triple.object);
    return valueFactory.createStatement(subj, pred, obj);
  }

  /**
   * Convert an RDF node to its RDF4j equivalent.
   */
  static Value value(hydra.ext.rdf.syntax.Node r) {
    return r.accept(new Node.Visitor<Value>() {
      @Override
      public Value visit(Node.Iri instance) {
        return iri(instance.value);
      }

      @Override
      public Value visit(Node.Bnode instance) {
        return bnode(instance.value);
      }

      @Override
      public Value visit(Node.Literal instance) {
        return literal(instance.value);
      }
    });
  }
}
