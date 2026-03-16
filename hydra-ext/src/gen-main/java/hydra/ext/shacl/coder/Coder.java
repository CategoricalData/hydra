// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shacl.coder;

/**
 * SHACL coder: converts Hydra types and terms to SHACL shapes and RDF descriptions
 */
public interface Coder {
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0> err(hydra.context.Context cx, String msg) {
    return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T0>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(msg)), cx)));
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0> unexpectedE(hydra.context.Context cx, String expected, String found) {
    return hydra.ext.shacl.coder.Coder.<T0>err(
      cx,
      hydra.lib.strings.Cat.apply(hydra.util.ConsList.of(
        "Expected ",
        expected,
        ", found: ",
        found)));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.ext.org.w3.shacl.model.ShapesGraph, hydra.context.Context>> shaclCoder(hydra.module.Module mod, hydra.context.Context cx, hydra.graph.Graph g) {
    java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.Shape>>> toShape = (java.util.function.Function<hydra.core.Binding, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.Shape>>>) (el -> hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<hydra.error.DecodingError, hydra.context.InContext<hydra.error.Error_>>) (_de -> (hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError((_de).value)), cx))),
        (java.util.function.Function<hydra.core.Type, hydra.core.Type>) (_t -> _t),
        hydra.decode.core.Core.type(
          g,
          (el).term)),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.Shape>>>) (_typ -> hydra.lib.eithers.Map.apply(
        (java.util.function.Function<hydra.ext.org.w3.shacl.model.CommonProperties, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.Shape>>) (_cp -> (hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.Shape>) (new hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.Shape>(hydra.ext.shacl.coder.Coder.elementIri(el), new hydra.ext.org.w3.shacl.model.Shape.Node(new hydra.ext.org.w3.shacl.model.NodeShape(_cp))))),
        hydra.ext.shacl.coder.Coder.encodeType(
          (el).name,
          _typ,
          cx)))));
    hydra.util.Lazy<hydra.util.ConsList<hydra.core.Binding>> typeEls = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      hydra.annotations.Annotations::isNativeType,
      (mod).elements));
    return hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.util.ConsList<hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.Shape>>, hydra.util.Pair<hydra.ext.org.w3.shacl.model.ShapesGraph, hydra.context.Context>>) (_shapes -> (hydra.util.Pair<hydra.ext.org.w3.shacl.model.ShapesGraph, hydra.context.Context>) ((hydra.util.Pair<hydra.ext.org.w3.shacl.model.ShapesGraph, hydra.context.Context>) (new hydra.util.Pair<hydra.ext.org.w3.shacl.model.ShapesGraph, hydra.context.Context>(new hydra.ext.org.w3.shacl.model.ShapesGraph(hydra.lib.sets.FromList.apply(_shapes)), cx)))),
      hydra.lib.eithers.MapList.apply(
        toShape,
        typeEls.get()));
  }
  
  static hydra.ext.org.w3.shacl.model.CommonProperties common(hydra.util.ConsList<hydra.ext.org.w3.shacl.model.CommonConstraint> constraints) {
    return new hydra.ext.org.w3.shacl.model.CommonProperties(hydra.lib.sets.FromList.apply(constraints), (hydra.util.Maybe<Boolean>) (hydra.util.Maybe.<Boolean>nothing()), new hydra.ext.org.w3.rdf.syntax.LangStrings((hydra.util.PersistentMap<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String>) ((hydra.util.PersistentMap<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String>) (hydra.lib.maps.Empty.<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String>apply()))), new hydra.ext.org.w3.shacl.model.Severity.Info(), (hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.RdfsClass>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.RdfsClass>apply()), (hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.IriOrLiteral>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.IriOrLiteral>apply()), (hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Property>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.Property>apply()), (hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Property>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.Property>apply()));
  }
  
  static hydra.ext.org.w3.shacl.model.CommonProperties defaultCommonProperties() {
    return hydra.ext.shacl.coder.Coder.common((hydra.util.ConsList<hydra.ext.org.w3.shacl.model.CommonConstraint>) (hydra.util.ConsList.<hydra.ext.org.w3.shacl.model.CommonConstraint>empty()));
  }
  
  static hydra.ext.org.w3.rdf.syntax.Iri elementIri(hydra.core.Binding el) {
    return hydra.ext.rdf.utils.Utils.nameToIri((el).name);
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>, hydra.context.Context>> encodeField(hydra.core.Name rname, hydra.ext.org.w3.rdf.syntax.Resource subject, hydra.core.Field field, hydra.context.Context cx, hydra.graph.Graph g) {
    hydra.util.Pair<hydra.ext.org.w3.rdf.syntax.Resource, hydra.context.Context> pair1 = hydra.ext.rdf.utils.Utils.nextBlankNode(cx);
    hydra.util.Lazy<hydra.context.Context> cx1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair1));
    hydra.util.Lazy<hydra.ext.org.w3.rdf.syntax.Resource> node = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair1));
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.shacl.coder.Coder.encodeTerm(
        node.get(),
        (field).term,
        cx1.get(),
        g),
      (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>, hydra.context.Context>>>) (_r1 -> {
        hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(_r1));
        hydra.util.Lazy<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>> descs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(_r1));
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>, hydra.context.Context>(hydra.lib.lists.Concat2.apply(
          hydra.ext.rdf.utils.Utils.triplesOf(descs.get()),
          hydra.ext.rdf.utils.Utils.forObjects(
            subject,
            hydra.ext.rdf.utils.Utils.propertyIri(
              rname,
              (field).name),
            hydra.ext.rdf.utils.Utils.subjectsOf(descs.get()))), cx2.get()))));
      }));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>> encodeFieldType(hydra.core.Name rname, hydra.util.Maybe<java.math.BigInteger> order, hydra.core.FieldType ft, hydra.context.Context cx) {
    hydra.core.Name fname = (ft).name;
    hydra.ext.org.w3.rdf.syntax.Iri iri = hydra.ext.rdf.utils.Utils.propertyIri(
      rname,
      fname);
    java.util.function.Function<hydra.util.Maybe<java.math.BigInteger>, java.util.function.Function<hydra.util.Maybe<java.math.BigInteger>, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>>>>> forTypeDefault = (java.util.function.Function<hydra.util.Maybe<java.math.BigInteger>, java.util.function.Function<hydra.util.Maybe<java.math.BigInteger>, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>>>>>) (mn -> (java.util.function.Function<hydra.util.Maybe<java.math.BigInteger>, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>>>>) (mx -> (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>>>) (t -> hydra.lib.eithers.Map.apply(
      (java.util.function.Function<hydra.ext.org.w3.shacl.model.CommonProperties, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>>) (_cp -> {
        hydra.ext.org.w3.shacl.model.PropertyShape baseProp = hydra.ext.shacl.coder.Coder.property(iri);
        hydra.util.Lazy<hydra.util.Maybe<hydra.ext.org.w3.shacl.model.PropertyShapeConstraint>> maxC = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
          (java.util.function.Function<java.math.BigInteger, hydra.ext.org.w3.shacl.model.PropertyShapeConstraint>) (_n -> new hydra.ext.org.w3.shacl.model.PropertyShapeConstraint.MaxCount(_n)),
          mx));
        hydra.util.Lazy<hydra.util.Maybe<hydra.ext.org.w3.shacl.model.PropertyShapeConstraint>> minC = new hydra.util.Lazy<>(() -> hydra.lib.maybes.Map.apply(
          (java.util.function.Function<java.math.BigInteger, hydra.ext.org.w3.shacl.model.PropertyShapeConstraint>) (_n -> new hydra.ext.org.w3.shacl.model.PropertyShapeConstraint.MinCount(_n)),
          mn));
        return (hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>) (new hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>(iri, new hydra.ext.org.w3.shacl.model.PropertyShape(_cp, hydra.lib.sets.FromList.apply(hydra.lib.maybes.Cat.apply(hydra.util.ConsList.of(
          minC.get(),
          maxC.get()))), (hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.Node>) (hydra.util.Maybe.<hydra.ext.org.w3.rdf.syntax.Node>nothing()), new hydra.ext.org.w3.rdf.syntax.LangStrings((hydra.util.PersistentMap<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String>) ((hydra.util.PersistentMap<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String>) (hydra.lib.maps.Empty.<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String>apply()))), new hydra.ext.org.w3.rdf.syntax.LangStrings((hydra.util.PersistentMap<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String>) ((hydra.util.PersistentMap<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String>) (hydra.lib.maps.Empty.<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String>apply()))), order, iri)));
      }),
      hydra.ext.shacl.coder.Coder.encodeType(
        rname,
        t,
        cx)))));
    java.util.concurrent.atomic.AtomicReference<java.util.function.Function<hydra.util.Maybe<java.math.BigInteger>, java.util.function.Function<hydra.util.Maybe<java.math.BigInteger>, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>>>>>> forType = new java.util.concurrent.atomic.AtomicReference<>();
    forType.set((java.util.function.Function<hydra.util.Maybe<java.math.BigInteger>, java.util.function.Function<hydra.util.Maybe<java.math.BigInteger>, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>>>>>) (mn -> (java.util.function.Function<hydra.util.Maybe<java.math.BigInteger>, java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>>>>) (mx -> (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>>>) (t -> hydra.rewriting.Rewriting.deannotateType(t).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>> otherwise(hydra.core.Type instance) {
        return (forTypeDefault).apply(mn).apply(mx).apply(t);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>> visit(hydra.core.Type.Maybe ot) {
        return forType.get().apply(hydra.util.Maybe.just(new java.math.BigInteger("0"))).apply(mx).apply((ot).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>> visit(hydra.core.Type.Set st) {
        return forType.get().apply(mn).apply((hydra.util.Maybe<java.math.BigInteger>) (hydra.util.Maybe.<java.math.BigInteger>nothing())).apply((st).value);
      }
    })))));
    hydra.core.Type ftype = (ft).type;
    return forType.get().apply(hydra.util.Maybe.just(new java.math.BigInteger("1"))).apply(hydra.util.Maybe.just(new java.math.BigInteger("1"))).apply(ftype);
  }
  
  static hydra.ext.org.w3.shacl.model.CommonProperties encodeLiteralType(hydra.core.LiteralType lt) {
    java.util.function.Function<String, hydra.ext.org.w3.shacl.model.CommonProperties> xsd = (java.util.function.Function<String, hydra.ext.org.w3.shacl.model.CommonProperties>) (local -> hydra.ext.shacl.coder.Coder.common(hydra.util.ConsList.of(new hydra.ext.org.w3.shacl.model.CommonConstraint.Datatype(hydra.ext.rdf.utils.Utils.xmlSchemaDatatypeIri(local)))));
    return (lt).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public hydra.ext.org.w3.shacl.model.CommonProperties visit(hydra.core.LiteralType.Binary ignored) {
        return (xsd).apply("base64Binary");
      }
      
      @Override
      public hydra.ext.org.w3.shacl.model.CommonProperties visit(hydra.core.LiteralType.Boolean_ ignored) {
        return (xsd).apply("boolean");
      }
      
      @Override
      public hydra.ext.org.w3.shacl.model.CommonProperties visit(hydra.core.LiteralType.Float_ ft) {
        return (ft).value.accept(new hydra.core.FloatType.PartialVisitor<>() {
          @Override
          public hydra.ext.org.w3.shacl.model.CommonProperties visit(hydra.core.FloatType.Bigfloat ignored) {
            return (xsd).apply("decimal");
          }
          
          @Override
          public hydra.ext.org.w3.shacl.model.CommonProperties visit(hydra.core.FloatType.Float32 ignored) {
            return (xsd).apply("float");
          }
          
          @Override
          public hydra.ext.org.w3.shacl.model.CommonProperties visit(hydra.core.FloatType.Float64 ignored) {
            return (xsd).apply("double");
          }
        });
      }
      
      @Override
      public hydra.ext.org.w3.shacl.model.CommonProperties visit(hydra.core.LiteralType.Integer_ it) {
        return (it).value.accept(new hydra.core.IntegerType.PartialVisitor<>() {
          @Override
          public hydra.ext.org.w3.shacl.model.CommonProperties visit(hydra.core.IntegerType.Bigint ignored) {
            return (xsd).apply("integer");
          }
          
          @Override
          public hydra.ext.org.w3.shacl.model.CommonProperties visit(hydra.core.IntegerType.Int8 ignored) {
            return (xsd).apply("byte");
          }
          
          @Override
          public hydra.ext.org.w3.shacl.model.CommonProperties visit(hydra.core.IntegerType.Int16 ignored) {
            return (xsd).apply("short");
          }
          
          @Override
          public hydra.ext.org.w3.shacl.model.CommonProperties visit(hydra.core.IntegerType.Int32 ignored) {
            return (xsd).apply("int");
          }
          
          @Override
          public hydra.ext.org.w3.shacl.model.CommonProperties visit(hydra.core.IntegerType.Int64 ignored) {
            return (xsd).apply("long");
          }
          
          @Override
          public hydra.ext.org.w3.shacl.model.CommonProperties visit(hydra.core.IntegerType.Uint8 ignored) {
            return (xsd).apply("unsignedByte");
          }
          
          @Override
          public hydra.ext.org.w3.shacl.model.CommonProperties visit(hydra.core.IntegerType.Uint16 ignored) {
            return (xsd).apply("unsignedShort");
          }
          
          @Override
          public hydra.ext.org.w3.shacl.model.CommonProperties visit(hydra.core.IntegerType.Uint32 ignored) {
            return (xsd).apply("unsignedInt");
          }
          
          @Override
          public hydra.ext.org.w3.shacl.model.CommonProperties visit(hydra.core.IntegerType.Uint64 ignored) {
            return (xsd).apply("unsignedLong");
          }
        });
      }
      
      @Override
      public hydra.ext.org.w3.shacl.model.CommonProperties visit(hydra.core.LiteralType.String_ ignored) {
        return (xsd).apply("string");
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>> encodeTerm(hydra.ext.org.w3.rdf.syntax.Resource subject, hydra.core.Term term, hydra.context.Context cx, hydra.graph.Graph g) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>> otherwise(hydra.core.Term instance) {
        return hydra.ext.shacl.coder.Coder.unexpectedE(
          cx,
          "RDF-compatible term",
          "unsupported term variant");
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>> visit(hydra.core.Term.Annotated at) {
        return hydra.ext.shacl.coder.Coder.encodeTerm(
          subject,
          (at).value.body,
          cx,
          g);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>> visit(hydra.core.Term.List terms) {
        return hydra.ext.shacl.coder.Coder.encodeList(
          subject,
          (terms).value,
          cx,
          g);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>> visit(hydra.core.Term.Literal lit) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>(hydra.util.ConsList.of(new hydra.ext.org.w3.rdf.syntax.Description(new hydra.ext.org.w3.rdf.syntax.Node.Literal(hydra.ext.rdf.utils.Utils.encodeLiteral((lit).value)), new hydra.ext.org.w3.rdf.syntax.Graph((hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Triple>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.Triple>apply())))), cx))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>> visit(hydra.core.Term.Map m) {
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>>, hydra.context.Context>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>>) (_r -> (hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>(hydra.util.ConsList.of(new hydra.ext.org.w3.rdf.syntax.Description(hydra.ext.rdf.utils.Utils.resourceToNode(subject), new hydra.ext.org.w3.rdf.syntax.Graph(hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(hydra.lib.pairs.First.apply(_r)))))), hydra.lib.pairs.Second.apply(_r))))),
          hydra.ext.shacl.coder.Coder.foldAccumResult(
            (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>, hydra.context.Context>>>>) (_cx0 -> (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>, hydra.context.Context>>>) (kv -> hydra.lib.eithers.Bind.apply(
              hydra.extract.core.Core.string(
                _cx0,
                g,
                hydra.rewriting.Rewriting.deannotateTerm(hydra.lib.pairs.First.apply(kv))),
              (java.util.function.Function<String, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>, hydra.context.Context>>>) (_ks -> {
                hydra.util.Pair<hydra.ext.org.w3.rdf.syntax.Resource, hydra.context.Context> pair2 = hydra.ext.rdf.utils.Utils.nextBlankNode(_cx0);
                hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair2));
                hydra.util.Lazy<hydra.ext.org.w3.rdf.syntax.Resource> node2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair2));
                return hydra.lib.eithers.Map.apply(
                  (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>, hydra.context.Context>>) (_dr -> (hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>, hydra.context.Context>(hydra.lib.lists.Concat2.apply(
                    hydra.ext.rdf.utils.Utils.forObjects(
                      subject,
                      hydra.ext.rdf.utils.Utils.keyIri(_ks),
                      hydra.ext.rdf.utils.Utils.subjectsOf(hydra.lib.pairs.First.apply(_dr))),
                    hydra.ext.rdf.utils.Utils.triplesOf(hydra.lib.pairs.First.apply(_dr))), hydra.lib.pairs.Second.apply(_dr))))),
                  hydra.ext.shacl.coder.Coder.encodeTerm(
                    node2.get(),
                    hydra.lib.pairs.Second.apply(kv),
                    cx2.get(),
                    g));
              })))),
            cx,
            hydra.lib.maps.ToList.apply((m).value)));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>> visit(hydra.core.Term.Wrap wt) {
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>>) (_dr -> {
            hydra.util.Lazy<hydra.context.Context> cx1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(_dr));
            hydra.util.Lazy<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>> descs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(_dr));
            return (hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>(hydra.lib.lists.Cons.apply(
              hydra.ext.shacl.coder.Coder.withType(
                (wt).value.typeName,
                hydra.lib.lists.Head.apply(descs.get())),
              hydra.lib.lists.Tail.apply(descs.get())), cx1.get())));
          }),
          hydra.ext.shacl.coder.Coder.encodeTerm(
            subject,
            (wt).value.body,
            cx,
            g));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>> visit(hydra.core.Term.Maybe mterm) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>((hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>) (hydra.util.ConsList.<hydra.ext.org.w3.rdf.syntax.Description>empty()), cx)))),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>>>) (_inner -> hydra.ext.shacl.coder.Coder.encodeTerm(
            subject,
            _inner,
            cx,
            g)),
          (mterm).value);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>> visit(hydra.core.Term.Record rec) {
        hydra.util.ConsList<hydra.core.Field> fields = (rec).value.fields;
        hydra.core.Name rname = (rec).value.typeName;
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>>, hydra.context.Context>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>>) (_r -> (hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>(hydra.util.ConsList.of(hydra.ext.shacl.coder.Coder.withType(
            rname,
            new hydra.ext.org.w3.rdf.syntax.Description(hydra.ext.rdf.utils.Utils.resourceToNode(subject), new hydra.ext.org.w3.rdf.syntax.Graph(hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat.apply(hydra.lib.pairs.First.apply(_r))))))), hydra.lib.pairs.Second.apply(_r))))),
          hydra.ext.shacl.coder.Coder.foldAccumResult(
            (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>, hydra.context.Context>>>>) (_cx0 -> (java.util.function.Function<hydra.core.Field, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>, hydra.context.Context>>>) (field -> hydra.ext.shacl.coder.Coder.encodeField(
              rname,
              subject,
              field,
              _cx0,
              g))),
            cx,
            fields));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>> visit(hydra.core.Term.Set terms) {
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>>, hydra.context.Context>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>>) (_r -> (hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>(hydra.lib.lists.Concat.apply(hydra.lib.pairs.First.apply(_r)), hydra.lib.pairs.Second.apply(_r))))),
          hydra.ext.shacl.coder.Coder.foldAccumResult(
            (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>>>>) (_cx0 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>>>) (t -> {
              hydra.util.Pair<hydra.ext.org.w3.rdf.syntax.Resource, hydra.context.Context> pair3 = hydra.ext.rdf.utils.Utils.nextBlankNode(_cx0);
              hydra.util.Lazy<hydra.context.Context> cx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair3));
              hydra.util.Lazy<hydra.ext.org.w3.rdf.syntax.Resource> node3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair3));
              return hydra.ext.shacl.coder.Coder.encodeTerm(
                node3.get(),
                t,
                cx3.get(),
                g);
            })),
            cx,
            hydra.lib.sets.ToList.apply((terms).value)));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>> visit(hydra.core.Term.Union inj) {
        hydra.core.Field field = (inj).value.field;
        hydra.core.Name rname = (inj).value.typeName;
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>, hydra.context.Context>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>>) (_r -> (hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>(hydra.util.ConsList.of(hydra.ext.shacl.coder.Coder.withType(
            rname,
            new hydra.ext.org.w3.rdf.syntax.Description(hydra.ext.rdf.utils.Utils.resourceToNode(subject), new hydra.ext.org.w3.rdf.syntax.Graph(hydra.lib.sets.FromList.apply(hydra.lib.pairs.First.apply(_r)))))), hydra.lib.pairs.Second.apply(_r))))),
          hydra.ext.shacl.coder.Coder.encodeField(
            rname,
            subject,
            field,
            cx,
            g));
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>> encodeList(hydra.ext.org.w3.rdf.syntax.Resource subj, hydra.util.ConsList<hydra.core.Term> terms, hydra.context.Context cx0, hydra.graph.Graph g) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(terms),
      () -> hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>>right((hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>(hydra.util.ConsList.of(new hydra.ext.org.w3.rdf.syntax.Description(new hydra.ext.org.w3.rdf.syntax.Node.Iri(new hydra.ext.org.w3.rdf.syntax.Iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")), new hydra.ext.org.w3.rdf.syntax.Graph((hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Triple>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.Triple>apply())))), cx0)))),
      () -> ((java.util.function.Supplier<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>>>) (() -> {
        hydra.util.Pair<hydra.ext.org.w3.rdf.syntax.Resource, hydra.context.Context> pair1 = hydra.ext.rdf.utils.Utils.nextBlankNode(cx0);
        hydra.util.Lazy<hydra.context.Context> cx1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair1));
        hydra.util.Lazy<hydra.ext.org.w3.rdf.syntax.Resource> node1 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair1));
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.shacl.coder.Coder.encodeTerm(
            node1.get(),
            hydra.lib.lists.Head.apply(terms),
            cx1.get(),
            g),
          (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>>>) (_r1 -> {
            hydra.util.Lazy<hydra.context.Context> cx2 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(_r1));
            hydra.util.Pair<hydra.ext.org.w3.rdf.syntax.Resource, hydra.context.Context> pair2 = hydra.ext.rdf.utils.Utils.nextBlankNode(cx2.get());
            hydra.util.Lazy<hydra.context.Context> cx3 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(pair2));
            hydra.util.Lazy<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>> fdescs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(_r1));
            hydra.util.Lazy<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>> firstTriples = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
              hydra.ext.rdf.utils.Utils.triplesOf(fdescs.get()),
              hydra.ext.rdf.utils.Utils.forObjects(
                subj,
                hydra.ext.rdf.utils.Utils.rdfIri("first"),
                hydra.ext.rdf.utils.Utils.subjectsOf(fdescs.get()))));
            hydra.util.Lazy<hydra.ext.org.w3.rdf.syntax.Resource> next = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(pair2));
            return hydra.lib.eithers.Map.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>, hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>>) (_r2 -> {
                hydra.util.Lazy<hydra.context.Context> cx4 = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(_r2));
                hydra.util.Lazy<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>> rdescs = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(_r2));
                hydra.util.Lazy<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Triple>> restTriples = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat2.apply(
                  hydra.ext.rdf.utils.Utils.triplesOf(rdescs.get()),
                  hydra.ext.rdf.utils.Utils.forObjects(
                    subj,
                    hydra.ext.rdf.utils.Utils.rdfIri("rest"),
                    hydra.ext.rdf.utils.Utils.subjectsOf(rdescs.get()))));
                return (hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>) ((hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>) (new hydra.util.Pair<hydra.util.ConsList<hydra.ext.org.w3.rdf.syntax.Description>, hydra.context.Context>(hydra.util.ConsList.of(new hydra.ext.org.w3.rdf.syntax.Description(hydra.ext.rdf.utils.Utils.resourceToNode(subj), new hydra.ext.org.w3.rdf.syntax.Graph(hydra.lib.sets.FromList.apply(hydra.lib.lists.Concat2.apply(
                  firstTriples.get(),
                  restTriples.get()))))), cx4.get())));
              }),
              hydra.ext.shacl.coder.Coder.encodeList(
                next.get(),
                hydra.lib.lists.Tail.apply(terms),
                cx3.get(),
                g));
          }));
      })).get());
  }
  
  static <T0, T1, T2, T3> hydra.util.Either<T2, hydra.util.Pair<hydra.util.ConsList<T3>, T0>> foldAccumResult(java.util.function.Function<T0, java.util.function.Function<T1, hydra.util.Either<T2, hydra.util.Pair<T3, T0>>>> f, T0 cx, hydra.util.ConsList<T1> xs) {
    return hydra.lib.logic.IfElse.lazy(
      hydra.lib.lists.Null.apply(xs),
      () -> hydra.util.Either.<T2, hydra.util.Pair<hydra.util.ConsList<T3>, T0>>right((hydra.util.Pair<hydra.util.ConsList<T3>, T0>) ((hydra.util.Pair<hydra.util.ConsList<T3>, T0>) (new hydra.util.Pair<hydra.util.ConsList<T3>, T0>((hydra.util.ConsList<T3>) (hydra.util.ConsList.<T3>empty()), cx)))),
      () -> hydra.lib.eithers.Bind.apply(
        (f).apply(cx).apply(hydra.lib.lists.Head.apply(xs)),
        (java.util.function.Function<hydra.util.Pair<T3, T0>, hydra.util.Either<T2, hydra.util.Pair<hydra.util.ConsList<T3>, T0>>>) (_r -> hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.util.Pair<hydra.util.ConsList<T3>, T0>, hydra.util.Pair<hydra.util.ConsList<T3>, T0>>) (_rest -> (hydra.util.Pair<hydra.util.ConsList<T3>, T0>) ((hydra.util.Pair<hydra.util.ConsList<T3>, T0>) (new hydra.util.Pair<hydra.util.ConsList<T3>, T0>(hydra.lib.lists.Cons.apply(
            hydra.lib.pairs.First.apply(_r),
            hydra.lib.pairs.First.apply(_rest)), hydra.lib.pairs.Second.apply(_rest))))),
          hydra.ext.shacl.coder.Coder.<T0, T1, T2, T3>foldAccumResult(
            f,
            hydra.lib.pairs.Second.apply(_r),
            hydra.lib.lists.Tail.apply(xs))))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.CommonProperties> encodeType(hydra.core.Name tname, hydra.core.Type typ, hydra.context.Context cx) {
    return hydra.rewriting.Rewriting.deannotateType(typ).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.CommonProperties> otherwise(hydra.core.Type instance) {
        return hydra.ext.shacl.coder.Coder.unexpectedE(
          cx,
          "type",
          "unsupported type variant");
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.CommonProperties> visit(hydra.core.Type.List ignored) {
        return hydra.ext.shacl.coder.Coder.encodeType_any(hydra.ext.shacl.coder.Coder::common);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.CommonProperties> visit(hydra.core.Type.Literal lt) {
        return hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.CommonProperties>right(hydra.ext.shacl.coder.Coder.encodeLiteralType((lt).value));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.CommonProperties> visit(hydra.core.Type.Map ignored) {
        return hydra.ext.shacl.coder.Coder.encodeType_any(hydra.ext.shacl.coder.Coder::common);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.CommonProperties> visit(hydra.core.Type.Wrap ignored) {
        return hydra.ext.shacl.coder.Coder.encodeType_any(hydra.ext.shacl.coder.Coder::common);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.CommonProperties> visit(hydra.core.Type.Record fts) {
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>>, hydra.ext.org.w3.shacl.model.CommonProperties>) (_props -> hydra.ext.shacl.coder.Coder.common(hydra.util.ConsList.of(new hydra.ext.org.w3.shacl.model.CommonConstraint.Property(hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>, hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.PropertyShape>>) (_p -> (hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.PropertyShape>) (new hydra.ext.org.w3.shacl.model.Reference.Definition(_p))),
            _props)))))),
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.util.Pair<java.math.BigInteger, hydra.core.FieldType>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>>>) (_pair -> hydra.ext.shacl.coder.Coder.encodeFieldType(
              tname,
              hydra.util.Maybe.just(hydra.lib.pairs.First.apply(_pair)),
              hydra.lib.pairs.Second.apply(_pair),
              cx)),
            hydra.lib.lists.Zip.apply(
              hydra.lib.lists.Map.apply(
                (java.util.function.Function<Integer, java.math.BigInteger>) (_i -> hydra.lib.literals.Int32ToBigint.apply(_i)),
                hydra.lib.math.Range.apply(
                  0,
                  hydra.lib.lists.Length.apply((fts).value))),
              (fts).value)));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.CommonProperties> visit(hydra.core.Type.Set ignored) {
        return hydra.ext.shacl.coder.Coder.encodeType_any(hydra.ext.shacl.coder.Coder::common);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.CommonProperties> visit(hydra.core.Type.Union fts) {
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.util.ConsList<hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>>, hydra.ext.org.w3.shacl.model.CommonProperties>) (_props -> hydra.ext.shacl.coder.Coder.common(hydra.util.ConsList.of(new hydra.ext.org.w3.shacl.model.CommonConstraint.Xone(hydra.lib.sets.FromList.apply(hydra.lib.lists.Map.apply(
            (java.util.function.Function<hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>, hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.Shape>>) (_p -> (hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.Shape>) (new hydra.ext.org.w3.shacl.model.Reference.Anonymous(hydra.ext.shacl.coder.Coder.node(hydra.util.ConsList.of(new hydra.ext.org.w3.shacl.model.CommonConstraint.Property(hydra.lib.sets.FromList.apply(hydra.util.ConsList.of((hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.PropertyShape>) (new hydra.ext.org.w3.shacl.model.Reference.Definition(_p)))))))))),
            _props)))))),
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.PropertyShape>>>) (_ft -> hydra.ext.shacl.coder.Coder.encodeFieldType(
              tname,
              (hydra.util.Maybe<java.math.BigInteger>) (hydra.util.Maybe.<java.math.BigInteger>nothing()),
              _ft,
              cx)),
            (fts).value));
      }
    });
  }
  
  static <T0> hydra.util.Either<T0, hydra.ext.org.w3.shacl.model.CommonProperties> encodeType_any(java.util.function.Function<hydra.util.ConsList<hydra.ext.org.w3.shacl.model.CommonConstraint>, hydra.ext.org.w3.shacl.model.CommonProperties> hydra_ext_shacl_coder_common2) {
    return hydra.util.Either.<T0, hydra.ext.org.w3.shacl.model.CommonProperties>right((hydra_ext_shacl_coder_common2).apply((hydra.util.ConsList<hydra.ext.org.w3.shacl.model.CommonConstraint>) (hydra.util.ConsList.<hydra.ext.org.w3.shacl.model.CommonConstraint>empty())));
  }
  
  static hydra.ext.org.w3.shacl.model.Shape node(hydra.util.ConsList<hydra.ext.org.w3.shacl.model.CommonConstraint> constraints) {
    return new hydra.ext.org.w3.shacl.model.Shape.Node(new hydra.ext.org.w3.shacl.model.NodeShape(hydra.ext.shacl.coder.Coder.common(constraints)));
  }
  
  static hydra.ext.org.w3.shacl.model.PropertyShape property(hydra.ext.org.w3.rdf.syntax.Iri iri) {
    return new hydra.ext.org.w3.shacl.model.PropertyShape(hydra.ext.shacl.coder.Coder.defaultCommonProperties(), (hydra.util.PersistentSet<hydra.ext.org.w3.shacl.model.PropertyShapeConstraint>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.shacl.model.PropertyShapeConstraint>apply()), (hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.Node>) (hydra.util.Maybe.<hydra.ext.org.w3.rdf.syntax.Node>nothing()), new hydra.ext.org.w3.rdf.syntax.LangStrings((hydra.util.PersistentMap<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String>) ((hydra.util.PersistentMap<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String>) (hydra.lib.maps.Empty.<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String>apply()))), new hydra.ext.org.w3.rdf.syntax.LangStrings((hydra.util.PersistentMap<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String>) ((hydra.util.PersistentMap<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String>) (hydra.lib.maps.Empty.<hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag>, String>apply()))), (hydra.util.Maybe<java.math.BigInteger>) (hydra.util.Maybe.<java.math.BigInteger>nothing()), iri);
  }
  
  static hydra.ext.org.w3.rdf.syntax.Description withType(hydra.core.Name name, hydra.ext.org.w3.rdf.syntax.Description desc) {
    hydra.ext.org.w3.rdf.syntax.Node subj = (desc).subject;
    hydra.ext.org.w3.rdf.syntax.Resource subjRes = (subj).accept(new hydra.ext.org.w3.rdf.syntax.Node.PartialVisitor<>() {
      @Override
      public hydra.ext.org.w3.rdf.syntax.Resource visit(hydra.ext.org.w3.rdf.syntax.Node.Iri iri) {
        return new hydra.ext.org.w3.rdf.syntax.Resource.Iri((iri).value);
      }
      
      @Override
      public hydra.ext.org.w3.rdf.syntax.Resource visit(hydra.ext.org.w3.rdf.syntax.Node.Bnode bnode) {
        return new hydra.ext.org.w3.rdf.syntax.Resource.Bnode((bnode).value);
      }
    });
    hydra.ext.org.w3.rdf.syntax.Triple triple = new hydra.ext.org.w3.rdf.syntax.Triple(subjRes, hydra.ext.rdf.utils.Utils.rdfIri("type"), new hydra.ext.org.w3.rdf.syntax.Node.Iri(hydra.ext.rdf.utils.Utils.nameToIri(name)));
    hydra.util.PersistentSet<hydra.ext.org.w3.rdf.syntax.Triple> triples = (desc).graph.value;
    return new hydra.ext.org.w3.rdf.syntax.Description(subj, new hydra.ext.org.w3.rdf.syntax.Graph(hydra.lib.sets.Insert.apply(
      triple,
      triples)));
  }
}
