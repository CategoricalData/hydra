package hydra.langs.rdf;

import hydra.langs.rdf.syntax.Description;
import hydra.langs.rdf.syntax.Graph;
import hydra.langs.rdf.syntax.Node;
import hydra.langs.rdf.syntax.Triple;
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

public interface Serde {
  ValueFactory valueFactory = SimpleValueFactory.getInstance();

  static String toNtriples(List<Description> descriptions) {
    List<Triple> triples = descriptions.stream().flatMap(
        description -> description.graph.value.stream()).collect(Collectors.toList());
    return Serde.toNtriples(triples);
  }

  static String toNtriples(Graph graph) {
    return toNtriples(graph.value);
  }

  static String toNtriples(Collection<Triple> triples) {
    ByteArrayOutputStream bos = new ByteArrayOutputStream();
    List<Statement> model = triples.stream().map(Serde::tripleToStatement).collect(Collectors.toList());
    Rio.write(model, bos, RDFFormat.TURTLE);

    return bos.toString();
  }

  static BNode bnode(hydra.langs.rdf.syntax.BlankNode r) {
    return valueFactory.createBNode(r.value);
  }

  static IRI iri(hydra.langs.rdf.syntax.Iri r) {
    return valueFactory.createIRI(r.value);
  }

  static Literal literal(hydra.langs.rdf.syntax.Literal r) {
    return valueFactory.createLiteral(r.lexicalForm, iri(r.datatypeIri));
  }

  static Resource resource(hydra.langs.rdf.syntax.Resource r) {
    return r.accept(new hydra.langs.rdf.syntax.Resource.Visitor<Resource>() {
      @Override
      public Resource visit(hydra.langs.rdf.syntax.Resource.Iri instance) {
        return iri(instance.value);
      }

      @Override
      public Resource visit(hydra.langs.rdf.syntax.Resource.Bnode instance) {
        return bnode(instance.value);
      }
    });
  }

  static Statement tripleToStatement(Triple triple) {
    Resource subj = resource(triple.subject);
    IRI pred = iri(triple.predicate);
    Value obj = value(triple.object);
    return valueFactory.createStatement(subj, pred, obj);
  }

  static Value value(hydra.langs.rdf.syntax.Node r) {
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
