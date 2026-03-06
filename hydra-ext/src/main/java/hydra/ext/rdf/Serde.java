package hydra.ext.rdf;

import hydra.ext.org.w3.rdf.syntax.Description;
import hydra.ext.org.w3.rdf.syntax.Graph;
import hydra.ext.org.w3.rdf.syntax.Node;
import hydra.ext.org.w3.rdf.syntax.Triple;
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
  /** Value factory for creating RDF4j values. */
  ValueFactory valueFactory = SimpleValueFactory.getInstance();

  /**
   * Serialize a list of RDF descriptions to N-Triples format.
   *
   * @param descriptions the list of RDF descriptions to serialize
   * @return the serialized N-Triples string
   */
  static String toNtriples(List<Description> descriptions) {
    List<Triple> triples = descriptions.stream().flatMap(
        description -> description.graph.value.stream()).collect(Collectors.toList());
    return Serde.toNtriples(triples);
  }

  /**
   * Serialize an RDF graph to N-Triples format.
   *
   * @param graph the RDF graph to serialize
   * @return the serialized N-Triples string
   */
  static String toNtriples(Graph graph) {
    return toNtriples(graph.value);
  }

  /**
   * Serialize a collection of RDF triples to N-Triples format.
   *
   * @param triples the collection of RDF triples to serialize
   * @return the serialized N-Triples string
   */
  static String toNtriples(Collection<Triple> triples) {
    ByteArrayOutputStream bos = new ByteArrayOutputStream();
    List<Statement> model = triples.stream().map(Serde::tripleToStatement).collect(Collectors.toList());
    Rio.write(model, bos, RDFFormat.TURTLE);

    return bos.toString();
  }

  /**
   * Convert a Hydra blank node to its RDF4j equivalent.
   *
   * @param r the Hydra blank node
   * @return the RDF4j BNode
   */
  static BNode bnode(hydra.ext.org.w3.rdf.syntax.BlankNode r) {
    return valueFactory.createBNode(r.value);
  }

  /**
   * Convert a Hydra IRI to its RDF4j equivalent.
   *
   * @param r the Hydra IRI
   * @return the RDF4j IRI
   */
  static IRI iri(hydra.ext.org.w3.rdf.syntax.Iri r) {
    return valueFactory.createIRI(r.value);
  }

  /**
   * Convert a Hydra RDF literal to its RDF4j equivalent.
   *
   * @param r the Hydra RDF literal
   * @return the RDF4j Literal
   */
  static Literal literal(hydra.ext.org.w3.rdf.syntax.Literal r) {
    return valueFactory.createLiteral(r.lexicalForm, iri(r.datatypeIri));
  }

  /**
   * Convert a Hydra RDF resource to its RDF4j equivalent.
   *
   * @param r the Hydra RDF resource
   * @return the RDF4j Resource
   */
  static Resource resource(hydra.ext.org.w3.rdf.syntax.Resource r) {
    return r.accept(new hydra.ext.org.w3.rdf.syntax.Resource.Visitor<Resource>() {
      @Override
      public Resource visit(hydra.ext.org.w3.rdf.syntax.Resource.Iri instance) {
        return iri(instance.value);
      }

      @Override
      public Resource visit(hydra.ext.org.w3.rdf.syntax.Resource.Bnode instance) {
        return bnode(instance.value);
      }
    });
  }

  /**
   * Convert a Hydra RDF triple to its RDF4j Statement equivalent.
   *
   * @param triple the Hydra RDF triple
   * @return the RDF4j Statement
   */
  static Statement tripleToStatement(Triple triple) {
    Resource subj = resource(triple.subject);
    IRI pred = iri(triple.predicate);
    Value obj = value(triple.object);
    return valueFactory.createStatement(subj, pred, obj);
  }

  /**
   * Convert a Hydra RDF node to its RDF4j Value equivalent.
   *
   * @param r the Hydra RDF node
   * @return the RDF4j Value
   */
  static Value value(hydra.ext.org.w3.rdf.syntax.Node r) {
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
