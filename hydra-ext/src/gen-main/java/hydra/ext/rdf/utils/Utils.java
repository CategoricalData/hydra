// Note: this is an automatically generated file. Do not edit.

package hydra.ext.rdf.utils;

/**
 * Utility functions for working with RDF graphs and descriptions
 */
public interface Utils {
  static hydra.core.Name key_rdfBlankNodeCounter() {
    return new hydra.core.Name("rdfBlankNodeCounter");
  }

  static hydra.ext.org.w3.rdf.syntax.Graph descriptionsToGraph(hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description> ds) {
    return new hydra.ext.org.w3.rdf.syntax.Graph(hydra.lib.sets.FromList.apply(hydra.ext.rdf.utils.Utils.triplesOf(ds)));
  }

  static hydra.ext.org.w3.rdf.syntax.Description emptyDescription(hydra.ext.org.w3.rdf.syntax.Node node) {
    return new hydra.ext.org.w3.rdf.syntax.Description(node, hydra.ext.rdf.utils.Utils.emptyRdfGraph());
  }

  static hydra.ext.org.w3.rdf.syntax.Graph emptyRdfGraph() {
    return new hydra.ext.org.w3.rdf.syntax.Graph((hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Triple>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.Triple>apply()));
  }

  static hydra.ext.org.w3.rdf.syntax.LangStrings emptyLangStrings() {
    return new hydra.ext.org.w3.rdf.syntax.LangStrings((hydra.util.PersistentMap<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String>) ((hydra.util.PersistentMap<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String>) (hydra.lib.maps.Empty.<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String>apply())));
  }

  static hydra.ext.org.w3.rdf.syntax.Literal encodeLiteral(hydra.core.Literal lit) {
    return (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.ext.org.w3.rdf.syntax.Literal visit(hydra.core.Literal.Binary s) {
        return new hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.BinaryToString.apply((s).value), hydra.ext.rdf.utils.Utils.xmlSchemaDatatypeIri("base64Binary"), (hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>) (hydra.util.Maybe.<hydra.ext.org.w3.rdf.syntax.LanguageTag>nothing()));
      }

      @Override
      public hydra.ext.org.w3.rdf.syntax.Literal visit(hydra.core.Literal.Boolean_ b) {
        return new hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.logic.IfElse.lazy(
          (b).value,
          () -> "true",
          () -> "false"), hydra.ext.rdf.utils.Utils.xmlSchemaDatatypeIri("boolean"), (hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>) (hydra.util.Maybe.<hydra.ext.org.w3.rdf.syntax.LanguageTag>nothing()));
      }

      @Override
      public hydra.ext.org.w3.rdf.syntax.Literal visit(hydra.core.Literal.Float_ f) {
        return (f).value.accept(new hydra.core.FloatValue.PartialVisitor<>() {
          @Override
          public hydra.ext.org.w3.rdf.syntax.Literal visit(hydra.core.FloatValue.Bigfloat v) {
            return new hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.ShowBigfloat.apply((v).value), hydra.ext.rdf.utils.Utils.xmlSchemaDatatypeIri("decimal"), (hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>) (hydra.util.Maybe.<hydra.ext.org.w3.rdf.syntax.LanguageTag>nothing()));
          }

          @Override
          public hydra.ext.org.w3.rdf.syntax.Literal visit(hydra.core.FloatValue.Float32 v) {
            return new hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.ShowFloat32.apply((v).value), hydra.ext.rdf.utils.Utils.xmlSchemaDatatypeIri("float"), (hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>) (hydra.util.Maybe.<hydra.ext.org.w3.rdf.syntax.LanguageTag>nothing()));
          }

          @Override
          public hydra.ext.org.w3.rdf.syntax.Literal visit(hydra.core.FloatValue.Float64 v) {
            return new hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.ShowFloat64.apply((v).value), hydra.ext.rdf.utils.Utils.xmlSchemaDatatypeIri("double"), (hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>) (hydra.util.Maybe.<hydra.ext.org.w3.rdf.syntax.LanguageTag>nothing()));
          }
        });
      }

      @Override
      public hydra.ext.org.w3.rdf.syntax.Literal visit(hydra.core.Literal.Integer_ i) {
        return (i).value.accept(new hydra.core.IntegerValue.PartialVisitor<>() {
          @Override
          public hydra.ext.org.w3.rdf.syntax.Literal visit(hydra.core.IntegerValue.Bigint v) {
            return new hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.ShowBigint.apply((v).value), hydra.ext.rdf.utils.Utils.xmlSchemaDatatypeIri("integer"), (hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>) (hydra.util.Maybe.<hydra.ext.org.w3.rdf.syntax.LanguageTag>nothing()));
          }

          @Override
          public hydra.ext.org.w3.rdf.syntax.Literal visit(hydra.core.IntegerValue.Int8 v) {
            return new hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.ShowInt8.apply((v).value), hydra.ext.rdf.utils.Utils.xmlSchemaDatatypeIri("byte"), (hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>) (hydra.util.Maybe.<hydra.ext.org.w3.rdf.syntax.LanguageTag>nothing()));
          }

          @Override
          public hydra.ext.org.w3.rdf.syntax.Literal visit(hydra.core.IntegerValue.Int16 v) {
            return new hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.ShowInt16.apply((v).value), hydra.ext.rdf.utils.Utils.xmlSchemaDatatypeIri("short"), (hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>) (hydra.util.Maybe.<hydra.ext.org.w3.rdf.syntax.LanguageTag>nothing()));
          }

          @Override
          public hydra.ext.org.w3.rdf.syntax.Literal visit(hydra.core.IntegerValue.Int32 v) {
            return new hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.ShowInt32.apply((v).value), hydra.ext.rdf.utils.Utils.xmlSchemaDatatypeIri("int"), (hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>) (hydra.util.Maybe.<hydra.ext.org.w3.rdf.syntax.LanguageTag>nothing()));
          }

          @Override
          public hydra.ext.org.w3.rdf.syntax.Literal visit(hydra.core.IntegerValue.Int64 v) {
            return new hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.ShowInt64.apply((v).value), hydra.ext.rdf.utils.Utils.xmlSchemaDatatypeIri("long"), (hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>) (hydra.util.Maybe.<hydra.ext.org.w3.rdf.syntax.LanguageTag>nothing()));
          }

          @Override
          public hydra.ext.org.w3.rdf.syntax.Literal visit(hydra.core.IntegerValue.Uint8 v) {
            return new hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.ShowUint8.apply((v).value), hydra.ext.rdf.utils.Utils.xmlSchemaDatatypeIri("unsignedByte"), (hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>) (hydra.util.Maybe.<hydra.ext.org.w3.rdf.syntax.LanguageTag>nothing()));
          }

          @Override
          public hydra.ext.org.w3.rdf.syntax.Literal visit(hydra.core.IntegerValue.Uint16 v) {
            return new hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.ShowUint16.apply((v).value), hydra.ext.rdf.utils.Utils.xmlSchemaDatatypeIri("unsignedShort"), (hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>) (hydra.util.Maybe.<hydra.ext.org.w3.rdf.syntax.LanguageTag>nothing()));
          }

          @Override
          public hydra.ext.org.w3.rdf.syntax.Literal visit(hydra.core.IntegerValue.Uint32 v) {
            return new hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.ShowUint32.apply((v).value), hydra.ext.rdf.utils.Utils.xmlSchemaDatatypeIri("unsignedInt"), (hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>) (hydra.util.Maybe.<hydra.ext.org.w3.rdf.syntax.LanguageTag>nothing()));
          }

          @Override
          public hydra.ext.org.w3.rdf.syntax.Literal visit(hydra.core.IntegerValue.Uint64 v) {
            return new hydra.ext.org.w3.rdf.syntax.Literal(hydra.lib.literals.ShowUint64.apply((v).value), hydra.ext.rdf.utils.Utils.xmlSchemaDatatypeIri("unsignedLong"), (hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>) (hydra.util.Maybe.<hydra.ext.org.w3.rdf.syntax.LanguageTag>nothing()));
          }
        });
      }

      @Override
      public hydra.ext.org.w3.rdf.syntax.Literal visit(hydra.core.Literal.String_ s) {
        return new hydra.ext.org.w3.rdf.syntax.Literal((s).value, hydra.ext.rdf.utils.Utils.xmlSchemaDatatypeIri("string"), (hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>) (hydra.util.Maybe.<hydra.ext.org.w3.rdf.syntax.LanguageTag>nothing()));
      }
    });
  }

  static hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple> forObjects(hydra.ext.org.w3.rdf.syntax.Resource subj, hydra.ext.org.w3.rdf.syntax.Iri pred, hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Node> objs) {
    return hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.org.w3.rdf.syntax.Node, hydra.ext.org.w3.rdf.syntax.Triple>) (obj -> new hydra.ext.org.w3.rdf.syntax.Triple(subj, pred, obj)),
      objs);
  }

  static hydra.ext.org.w3.rdf.syntax.Iri iri(String ns, String local) {
    return new hydra.ext.org.w3.rdf.syntax.Iri(hydra.lib.strings.Cat2.apply(
      ns,
      local));
  }

  static hydra.ext.org.w3.rdf.syntax.Iri keyIri(String local) {
    return hydra.ext.rdf.utils.Utils.iri(
      "urn:key:",
      local);
  }

  static hydra.ext.org.w3.rdf.syntax.Graph mergeGraphs(hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Graph> graphs) {
    return new hydra.ext.org.w3.rdf.syntax.Graph(hydra.lib.sets.Unions.apply(hydra.lib.lists.Map.apply(
      wrapped -> (wrapped).value,
      graphs)));
  }

  static hydra.ext.org.w3.rdf.syntax.Iri nameToIri(hydra.core.Name name) {
    return new hydra.ext.org.w3.rdf.syntax.Iri(hydra.lib.strings.Cat2.apply(
      "urn:",
      (name).value));
  }

  static hydra.util.Pair<hydra.ext.org.w3.rdf.syntax.Resource, hydra.context.Context> nextBlankNode(hydra.context.Context cx) {
    hydra.util.Pair<Integer, hydra.context.Context> result = hydra.annotations.Annotations.nextCount(
      hydra.ext.rdf.utils.Utils.key_rdfBlankNodeCounter(),
      cx);
    hydra.util.Lazy<Integer> count = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(result));
    hydra.util.Lazy<hydra.context.Context> cx_ = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(result));
    return (hydra.util.Pair<hydra.ext.org.w3.rdf.syntax.Resource, hydra.context.Context>) ((hydra.util.Pair<hydra.ext.org.w3.rdf.syntax.Resource, hydra.context.Context>) (new hydra.util.Pair<hydra.ext.org.w3.rdf.syntax.Resource, hydra.context.Context>(new hydra.ext.org.w3.rdf.syntax.Resource.Bnode(new hydra.ext.org.w3.rdf.syntax.BlankNode(hydra.lib.strings.Cat2.apply(
      "b",
      hydra.lib.literals.ShowInt32.apply(count.get())))), cx_.get())));
  }

  static hydra.ext.org.w3.rdf.syntax.Iri propertyIri(hydra.core.Name rname, hydra.core.Name fname) {
    hydra.module.QualifiedName qualName = hydra.names.Names.qualifyName(rname);
    hydra.util.Maybe<hydra.module.Namespace> gname = (qualName).namespace;
    String local_ = (qualName).local;
    return new hydra.ext.org.w3.rdf.syntax.Iri(hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
      "urn:",
      hydra.lib.maybes.Maybe.applyLazy(
        () -> "",
        wrapped -> (wrapped).value,
        gname),
      "#",
      hydra.formatting.Formatting.decapitalize(local_),
      hydra.formatting.Formatting.capitalize((fname).value))));
  }

  static hydra.ext.org.w3.rdf.syntax.Iri rdfIri(String local) {
    return hydra.ext.rdf.utils.Utils.iri(
      "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      local);
  }

  static hydra.ext.org.w3.rdf.syntax.Node resourceToNode(hydra.ext.org.w3.rdf.syntax.Resource r) {
    return (r).accept(new hydra.ext.org.w3.rdf.syntax.Resource.PartialVisitor<>() {
      @Override
      public hydra.ext.org.w3.rdf.syntax.Node visit(hydra.ext.org.w3.rdf.syntax.Resource.Iri i) {
        return new hydra.ext.org.w3.rdf.syntax.Node.Iri((i).value);
      }

      @Override
      public hydra.ext.org.w3.rdf.syntax.Node visit(hydra.ext.org.w3.rdf.syntax.Resource.Bnode b) {
        return new hydra.ext.org.w3.rdf.syntax.Node.Bnode((b).value);
      }
    });
  }

  static hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Node> subjectsOf(hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description> descs) {
    return hydra.lib.lists.Map.apply(
      projected -> projected.subject,
      descs);
  }

  static hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple> triplesOf(hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description> descs) {
    return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.ext.org.w3.rdf.syntax.Description, hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>>) (d -> hydra.lib.sets.ToList.apply((d).graph.value)),
      descs));
  }

  static hydra.ext.org.w3.rdf.syntax.Iri xmlSchemaDatatypeIri(String local) {
    return hydra.ext.rdf.utils.Utils.iri(
      "http://www.w3.org/2001/XMLSchema#",
      local);
  }
}
