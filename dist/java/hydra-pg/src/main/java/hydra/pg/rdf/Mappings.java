// Note: this is an automatically generated file. Do not edit.

package hydra.pg.rdf;

/**
 * Mappings from property graph schemas to SHACL shapes graphs, and from property graph data to RDF graphs
 */
public interface Mappings {
  static <T0, T1> java.util.List<hydra.ext.org.w3.shacl.model.CommonConstraint> edgeTypesToPropertyShapes(T0 encodeVertexLabel, java.util.function.Function<hydra.pg.model.EdgeLabel, hydra.ext.org.w3.rdf.syntax.Iri> encodeEdgeLabel, hydra.pg.model.VertexLabel vertexLabel, java.util.List<hydra.pg.model.EdgeType<T1>> edgeTypes) {
    return hydra.lib.lists.Concat.apply(hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.pg.model.EdgeType<T1>, java.util.List<hydra.ext.org.w3.shacl.model.CommonConstraint>>) (et -> {
        hydra.util.Lazy<hydra.ext.org.w3.shacl.model.CommonConstraint> edgeShape = new hydra.util.Lazy<>(() -> new hydra.ext.org.w3.shacl.model.CommonConstraint.Property(hydra.lib.sets.Singleton.apply((hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.PropertyShape>) (new hydra.ext.org.w3.shacl.model.Reference.Anonymous(new hydra.ext.org.w3.shacl.model.PropertyShape(new hydra.ext.org.w3.shacl.model.CommonProperties(hydra.lib.sets.Singleton.apply(new hydra.ext.org.w3.shacl.model.CommonConstraint.Class_(hydra.lib.sets.Singleton.apply(new hydra.ext.org.w3.rdf.syntax.RdfsClass(null)))), (hydra.util.Maybe<Boolean>) (hydra.util.Maybe.<Boolean>nothing()), hydra.ext.rdf.Utils.emptyLangStrings(), new hydra.ext.org.w3.shacl.model.Severity.Violation(), (java.util.Set<hydra.ext.org.w3.rdf.syntax.RdfsClass>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.RdfsClass>apply()), (java.util.Set<hydra.ext.org.w3.rdf.syntax.IriOrLiteral>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.IriOrLiteral>apply()), (java.util.Set<hydra.ext.org.w3.rdf.syntax.Property>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.Property>apply()), (java.util.Set<hydra.ext.org.w3.rdf.syntax.Property>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.Property>apply())), (java.util.Set<hydra.ext.org.w3.shacl.model.PropertyShapeConstraint>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.shacl.model.PropertyShapeConstraint>apply()), (hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.Node>) (hydra.util.Maybe.<hydra.ext.org.w3.rdf.syntax.Node>nothing()), hydra.ext.rdf.Utils.emptyLangStrings(), hydra.ext.rdf.Utils.emptyLangStrings(), (hydra.util.Maybe<java.math.BigInteger>) (hydra.util.Maybe.<java.math.BigInteger>nothing()), (encodeEdgeLabel).apply(((java.util.function.Function<hydra.pg.model.EdgeType<T1>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(et))))))));
        hydra.util.Lazy<hydra.pg.model.VertexLabel> outLabel = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.EdgeType<T1>, hydra.pg.model.VertexLabel>) (projected -> projected.out)).apply(et));
        hydra.util.Lazy<Boolean> matchesVertex = new hydra.util.Lazy<>(() -> hydra.lib.equality.Equal.apply(
          outLabel.get().value,
          (vertexLabel).value));
        return hydra.lib.logic.IfElse.lazy(
          matchesVertex.get(),
          () -> java.util.Arrays.asList(edgeShape.get()),
          () -> (java.util.List<hydra.ext.org.w3.shacl.model.CommonConstraint>) (java.util.Collections.<hydra.ext.org.w3.shacl.model.CommonConstraint>emptyList()));
      }),
      edgeTypes));
  }

  static <T0> hydra.ext.org.w3.rdf.syntax.Description encodeEdge(hydra.pg.rdf.environment.PgRdfEnvironment<T0> env, hydra.pg.model.Edge<T0> edge) {
    hydra.util.Lazy<hydra.pg.model.EdgeLabel> elab = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Edge<T0>, hydra.pg.model.EdgeLabel>) (projected -> projected.label)).apply(edge));
    hydra.util.Lazy<hydra.ext.org.w3.rdf.syntax.Node> obj = new hydra.util.Lazy<>(() -> new hydra.ext.org.w3.rdf.syntax.Node.Iri(((java.util.function.Function<hydra.pg.rdf.environment.PgRdfEnvironment<T0>, java.util.function.Function<T0, hydra.ext.org.w3.rdf.syntax.Iri>>) (projected -> projected.encodeVertexId)).apply(env).apply(hydra.pg.rdf.Mappings.<T0>encodeEdge_ein(edge))));
    hydra.util.Lazy<hydra.ext.org.w3.rdf.syntax.Iri> pred = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.rdf.environment.PgRdfEnvironment<T0>, java.util.function.Function<hydra.pg.model.EdgeLabel, hydra.ext.org.w3.rdf.syntax.Iri>>) (projected -> projected.encodeEdgeLabel)).apply(env).apply(elab.get()));
    hydra.util.Lazy<hydra.ext.org.w3.rdf.syntax.Resource> subj = new hydra.util.Lazy<>(() -> new hydra.ext.org.w3.rdf.syntax.Resource.Iri(((java.util.function.Function<hydra.pg.rdf.environment.PgRdfEnvironment<T0>, java.util.function.Function<T0, hydra.ext.org.w3.rdf.syntax.Iri>>) (projected -> projected.encodeVertexId)).apply(env).apply(hydra.pg.rdf.Mappings.<T0>encodeEdge_eout(edge))));
    return new hydra.ext.org.w3.rdf.syntax.Description(hydra.ext.rdf.Utils.resourceToNode(subj.get()), new hydra.ext.org.w3.rdf.syntax.Graph(hydra.lib.sets.Singleton.apply(new hydra.ext.org.w3.rdf.syntax.Triple(subj.get(), pred.get(), obj.get()))));
  }

  static <T0> T0 encodeEdge_ein(hydra.pg.model.Edge<T0> edge) {
    return ((java.util.function.Function<hydra.pg.model.Edge<T0>, T0>) (projected -> projected.in)).apply(edge);
  }

  static <T0> T0 encodeEdge_eout(hydra.pg.model.Edge<T0> edge) {
    return ((java.util.function.Function<hydra.pg.model.Edge<T0>, T0>) (projected -> projected.out)).apply(edge);
  }

  static <T0> hydra.ext.org.w3.rdf.syntax.Graph encodeLazyGraph(hydra.pg.rdf.environment.PgRdfEnvironment<T0> env, hydra.pg.model.LazyGraph<T0> lg) {
    hydra.util.Lazy<java.util.List<hydra.ext.org.w3.rdf.syntax.Description>> edgeDescs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.pg.model.Edge<T0>, hydra.ext.org.w3.rdf.syntax.Description>) (v1 -> hydra.pg.rdf.Mappings.<T0>encodeEdge(
        env,
        v1)),
      ((java.util.function.Function<hydra.pg.model.LazyGraph<T0>, java.util.List<hydra.pg.model.Edge<T0>>>) (projected -> projected.edges)).apply(lg)));
    hydra.util.Lazy<java.util.List<hydra.ext.org.w3.rdf.syntax.Description>> vertexDescs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.pg.model.Vertex<T0>, hydra.ext.org.w3.rdf.syntax.Description>) (v1 -> hydra.pg.rdf.Mappings.<T0>encodeVertex(
        env,
        v1)),
      ((java.util.function.Function<hydra.pg.model.LazyGraph<T0>, java.util.List<hydra.pg.model.Vertex<T0>>>) (projected -> projected.vertices)).apply(lg)));
    hydra.util.Lazy<java.util.List<hydra.ext.org.w3.rdf.syntax.Description>> allDescs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Concat.apply(java.util.Arrays.asList(
      vertexDescs.get(),
      edgeDescs.get())));
    return hydra.ext.rdf.Utils.descriptionsToGraph(allDescs.get());
  }

  static <T0> hydra.ext.org.w3.rdf.syntax.Description encodeVertex(hydra.pg.rdf.environment.PgRdfEnvironment<T0> env, hydra.pg.model.Vertex<T0> vertex) {
    hydra.util.Lazy<hydra.ext.org.w3.rdf.syntax.Resource> subj = new hydra.util.Lazy<>(() -> new hydra.ext.org.w3.rdf.syntax.Resource.Iri(((java.util.function.Function<hydra.pg.rdf.environment.PgRdfEnvironment<T0>, java.util.function.Function<T0, hydra.ext.org.w3.rdf.syntax.Iri>>) (projected -> projected.encodeVertexId)).apply(env).apply(hydra.pg.rdf.Mappings.<T0>encodeVertex_vid(vertex))));
    hydra.util.Lazy<java.util.List<hydra.ext.org.w3.rdf.syntax.Triple>> propTriples = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.util.Pair<hydra.pg.model.PropertyKey, T0>, hydra.ext.org.w3.rdf.syntax.Triple>) (kv -> {
        hydra.util.Lazy<hydra.pg.model.PropertyKey> key = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(kv));
        hydra.util.Lazy<hydra.ext.org.w3.rdf.syntax.Node> obj = new hydra.util.Lazy<>(() -> new hydra.ext.org.w3.rdf.syntax.Node.Literal(((java.util.function.Function<hydra.pg.rdf.environment.PgRdfEnvironment<T0>, java.util.function.Function<T0, hydra.ext.org.w3.rdf.syntax.Literal>>) (projected -> projected.encodePropertyValue)).apply(env).apply(hydra.pg.rdf.Mappings.<T0>encodeVertex_val(kv))));
        hydra.util.Lazy<hydra.ext.org.w3.rdf.syntax.Iri> pred = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.rdf.environment.PgRdfEnvironment<T0>, java.util.function.Function<hydra.pg.model.PropertyKey, hydra.ext.org.w3.rdf.syntax.Iri>>) (projected -> projected.encodePropertyKey)).apply(env).apply(key.get()));
        return new hydra.ext.org.w3.rdf.syntax.Triple(subj.get(), pred.get(), obj.get());
      }),
      hydra.lib.maps.ToList.apply(hydra.pg.rdf.Mappings.<T0>encodeVertex_vprops(vertex))));
    hydra.util.Lazy<hydra.pg.model.VertexLabel> vlab = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.Vertex<T0>, hydra.pg.model.VertexLabel>) (projected -> projected.label)).apply(vertex));
    hydra.util.Lazy<hydra.ext.org.w3.rdf.syntax.Node> rtype = new hydra.util.Lazy<>(() -> new hydra.ext.org.w3.rdf.syntax.Node.Iri(((java.util.function.Function<hydra.pg.rdf.environment.PgRdfEnvironment<T0>, java.util.function.Function<hydra.pg.model.VertexLabel, hydra.ext.org.w3.rdf.syntax.Iri>>) (projected -> projected.encodeVertexLabel)).apply(env).apply(vlab.get())));
    hydra.ext.org.w3.rdf.syntax.Triple typeTriple = new hydra.ext.org.w3.rdf.syntax.Triple(subj.get(), hydra.ext.rdf.Utils.rdfIri("type"), rtype.get());
    hydra.util.Lazy<java.util.List<hydra.ext.org.w3.rdf.syntax.Triple>> allTriples = new hydra.util.Lazy<>(() -> hydra.lib.lists.Cons.apply(
      typeTriple,
      propTriples.get()));
    return new hydra.ext.org.w3.rdf.syntax.Description(hydra.ext.rdf.Utils.resourceToNode(subj.get()), new hydra.ext.org.w3.rdf.syntax.Graph(hydra.lib.sets.FromList.apply(allTriples.get())));
  }

  static <T0> T0 encodeVertex_val(hydra.util.Pair<hydra.pg.model.PropertyKey, T0> kv) {
    return hydra.lib.pairs.Second.apply(kv);
  }

  static <T0> T0 encodeVertex_vid(hydra.pg.model.Vertex<T0> vertex) {
    return ((java.util.function.Function<hydra.pg.model.Vertex<T0>, T0>) (projected -> projected.id)).apply(vertex);
  }

  static <T0> java.util.Map<hydra.pg.model.PropertyKey, T0> encodeVertex_vprops(hydra.pg.model.Vertex<T0> vertex) {
    return ((java.util.function.Function<hydra.pg.model.Vertex<T0>, java.util.Map<hydra.pg.model.PropertyKey, T0>>) (projected -> projected.properties)).apply(vertex);
  }

  static <T0> hydra.ext.org.w3.shacl.model.ShapesGraph graphSchemaToShapesGraph(java.util.function.Function<T0, hydra.ext.org.w3.rdf.syntax.Iri> encodeType, java.util.function.Function<hydra.pg.model.VertexLabel, hydra.ext.org.w3.rdf.syntax.Iri> encodeVertexLabel, java.util.function.Function<hydra.pg.model.EdgeLabel, hydra.ext.org.w3.rdf.syntax.Iri> encodeEdgeLabel, java.util.function.Function<hydra.pg.model.PropertyKey, hydra.ext.org.w3.rdf.syntax.Iri> encodeKey, hydra.pg.model.GraphSchema<T0> schema) {
    hydra.util.Lazy<java.util.List<hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.Shape>>> defs = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.pg.model.VertexType<T0>, hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.Shape>>) (vt -> {
        hydra.util.Lazy<hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.Shape>> baseDef = new hydra.util.Lazy<>(() -> hydra.pg.rdf.Mappings.<T0>vertexTypeToNodeShape(
          encodeType,
          encodeVertexLabel,
          encodeKey,
          vt));
        hydra.util.Lazy<hydra.ext.org.w3.shacl.model.Shape> baseShape = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.Shape>, hydra.ext.org.w3.shacl.model.Shape>) (projected -> projected.target)).apply(baseDef.get()));
        hydra.util.Lazy<hydra.ext.org.w3.shacl.model.NodeShape> baseNode = new hydra.util.Lazy<>(() -> baseShape.get().accept(new hydra.ext.org.w3.shacl.model.Shape.PartialVisitor<>() {
          @Override
          public hydra.ext.org.w3.shacl.model.NodeShape visit(hydra.ext.org.w3.shacl.model.Shape.Node ns) {
            return (ns).value;
          }

          @Override
          public hydra.ext.org.w3.shacl.model.NodeShape visit(hydra.ext.org.w3.shacl.model.Shape.Property ignored) {
            return new hydra.ext.org.w3.shacl.model.NodeShape(new hydra.ext.org.w3.shacl.model.CommonProperties((java.util.Set<hydra.ext.org.w3.shacl.model.CommonConstraint>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.shacl.model.CommonConstraint>apply()), (hydra.util.Maybe<Boolean>) (hydra.util.Maybe.<Boolean>nothing()), hydra.ext.rdf.Utils.emptyLangStrings(), new hydra.ext.org.w3.shacl.model.Severity.Violation(), (java.util.Set<hydra.ext.org.w3.rdf.syntax.RdfsClass>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.RdfsClass>apply()), (java.util.Set<hydra.ext.org.w3.rdf.syntax.IriOrLiteral>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.IriOrLiteral>apply()), (java.util.Set<hydra.ext.org.w3.rdf.syntax.Property>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.Property>apply()), (java.util.Set<hydra.ext.org.w3.rdf.syntax.Property>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.Property>apply())));
          }
        }));
        hydra.ext.org.w3.shacl.model.CommonProperties baseCommon = baseNode.get().common;
        hydra.util.Lazy<java.util.List<hydra.ext.org.w3.shacl.model.CommonConstraint>> edgeShapes = new hydra.util.Lazy<>(() -> hydra.pg.rdf.Mappings.edgeTypesToPropertyShapes(
          encodeVertexLabel,
          encodeEdgeLabel,
          ((java.util.function.Function<hydra.pg.model.VertexType<T0>, hydra.pg.model.VertexLabel>) (projected -> projected.label)).apply(vt),
          hydra.pg.rdf.Mappings.<T0>graphSchemaToShapesGraph_edgeTypes(schema)));
        hydra.util.Lazy<java.util.Set<hydra.ext.org.w3.shacl.model.CommonConstraint>> mergedConstraints = new hydra.util.Lazy<>(() -> hydra.lib.sets.Union.apply(
          (baseCommon).constraints,
          hydra.lib.sets.FromList.apply(edgeShapes.get())));
        hydra.ext.org.w3.shacl.model.CommonProperties updatedCommon = new hydra.ext.org.w3.shacl.model.CommonProperties(mergedConstraints.get(), (baseCommon).deactivated, (baseCommon).message, (baseCommon).severity, (baseCommon).targetClass, (baseCommon).targetNode, (baseCommon).targetObjectsOf, (baseCommon).targetSubjectsOf);
        hydra.ext.org.w3.shacl.model.Shape updatedShape = new hydra.ext.org.w3.shacl.model.Shape.Node(new hydra.ext.org.w3.shacl.model.NodeShape(updatedCommon));
        return (hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.Shape>) (new hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.Shape>(((java.util.function.Function<hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.Shape>, hydra.ext.org.w3.rdf.syntax.Iri>) (projected -> projected.iri)).apply(baseDef.get()), updatedShape));
      }),
      hydra.pg.rdf.Mappings.<T0>graphSchemaToShapesGraph_vertexTypes(schema)));
    return new hydra.ext.org.w3.shacl.model.ShapesGraph(hydra.lib.sets.FromList.apply(defs.get()));
  }

  static <T0> java.util.List<hydra.pg.model.EdgeType<T0>> graphSchemaToShapesGraph_edgeTypes(hydra.pg.model.GraphSchema<T0> schema) {
    return hydra.lib.maps.Elems.apply(((java.util.function.Function<hydra.pg.model.GraphSchema<T0>, java.util.Map<hydra.pg.model.EdgeLabel, hydra.pg.model.EdgeType<T0>>>) (projected -> projected.edges)).apply(schema));
  }

  static <T0> java.util.List<hydra.pg.model.VertexType<T0>> graphSchemaToShapesGraph_vertexTypes(hydra.pg.model.GraphSchema<T0> schema) {
    return hydra.lib.maps.Elems.apply(((java.util.function.Function<hydra.pg.model.GraphSchema<T0>, java.util.Map<hydra.pg.model.VertexLabel, hydra.pg.model.VertexType<T0>>>) (projected -> projected.vertices)).apply(schema));
  }

  static <T0> hydra.ext.org.w3.shacl.model.PropertyShape propertyTypeToPropertyShape(java.util.function.Function<T0, hydra.ext.org.w3.rdf.syntax.Iri> encodeType, java.util.function.Function<hydra.pg.model.PropertyKey, hydra.ext.org.w3.rdf.syntax.Iri> encodeKey, hydra.pg.model.PropertyType<T0> pt) {
    hydra.util.Lazy<hydra.ext.org.w3.rdf.syntax.Iri> dtIri = new hydra.util.Lazy<>(() -> (encodeType).apply(((java.util.function.Function<hydra.pg.model.PropertyType<T0>, T0>) (projected -> projected.value)).apply(pt)));
    hydra.util.Lazy<java.util.Set<hydra.ext.org.w3.shacl.model.CommonConstraint>> constraints = new hydra.util.Lazy<>(() -> hydra.lib.sets.Singleton.apply(new hydra.ext.org.w3.shacl.model.CommonConstraint.Datatype(dtIri.get())));
    hydra.util.Lazy<hydra.pg.model.PropertyKey> key = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.PropertyType<T0>, hydra.pg.model.PropertyKey>) (projected -> projected.key)).apply(pt));
    hydra.ext.org.w3.rdf.syntax.Iri path = (encodeKey).apply(key.get());
    hydra.util.Lazy<Boolean> required_ = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.PropertyType<T0>, Boolean>) (projected -> projected.required)).apply(pt));
    hydra.util.Lazy<java.util.Set<hydra.ext.org.w3.shacl.model.PropertyShapeConstraint>> propConstraints = new hydra.util.Lazy<>(() -> hydra.lib.logic.IfElse.lazy(
      required_.get(),
      () -> hydra.lib.sets.Singleton.apply(new hydra.ext.org.w3.shacl.model.PropertyShapeConstraint.MinCount(new java.math.BigInteger("1"))),
      () -> (java.util.Set<hydra.ext.org.w3.shacl.model.PropertyShapeConstraint>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.shacl.model.PropertyShapeConstraint>apply())));
    return new hydra.ext.org.w3.shacl.model.PropertyShape(new hydra.ext.org.w3.shacl.model.CommonProperties(constraints.get(), (hydra.util.Maybe<Boolean>) (hydra.util.Maybe.<Boolean>nothing()), hydra.ext.rdf.Utils.emptyLangStrings(), new hydra.ext.org.w3.shacl.model.Severity.Violation(), (java.util.Set<hydra.ext.org.w3.rdf.syntax.RdfsClass>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.RdfsClass>apply()), (java.util.Set<hydra.ext.org.w3.rdf.syntax.IriOrLiteral>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.IriOrLiteral>apply()), (java.util.Set<hydra.ext.org.w3.rdf.syntax.Property>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.Property>apply()), (java.util.Set<hydra.ext.org.w3.rdf.syntax.Property>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.Property>apply())), propConstraints.get(), (hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.Node>) (hydra.util.Maybe.<hydra.ext.org.w3.rdf.syntax.Node>nothing()), hydra.ext.rdf.Utils.emptyLangStrings(), hydra.ext.rdf.Utils.emptyLangStrings(), (hydra.util.Maybe<java.math.BigInteger>) (hydra.util.Maybe.<java.math.BigInteger>nothing()), path);
  }

  static <T0> hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.Shape> vertexTypeToNodeShape(java.util.function.Function<T0, hydra.ext.org.w3.rdf.syntax.Iri> encodeType, java.util.function.Function<hydra.pg.model.VertexLabel, hydra.ext.org.w3.rdf.syntax.Iri> encodeLabel, java.util.function.Function<hydra.pg.model.PropertyKey, hydra.ext.org.w3.rdf.syntax.Iri> encodeKey, hydra.pg.model.VertexType<T0> vt) {
    hydra.util.Lazy<java.util.List<hydra.ext.org.w3.shacl.model.CommonConstraint>> propShapes = new hydra.util.Lazy<>(() -> hydra.lib.lists.Map.apply(
      (java.util.function.Function<hydra.pg.model.PropertyType<T0>, hydra.ext.org.w3.shacl.model.CommonConstraint>) (pt -> new hydra.ext.org.w3.shacl.model.CommonConstraint.Property(hydra.lib.sets.Singleton.apply((hydra.ext.org.w3.shacl.model.Reference<hydra.ext.org.w3.shacl.model.PropertyShape>) (new hydra.ext.org.w3.shacl.model.Reference.Anonymous(hydra.pg.rdf.Mappings.<T0>propertyTypeToPropertyShape(
        encodeType,
        encodeKey,
        pt)))))),
      hydra.pg.rdf.Mappings.<T0>vertexTypeToNodeShape_propTypes(vt)));
    hydra.util.Lazy<hydra.ext.org.w3.shacl.model.CommonProperties> common = new hydra.util.Lazy<>(() -> new hydra.ext.org.w3.shacl.model.CommonProperties(hydra.lib.sets.FromList.apply(propShapes.get()), (hydra.util.Maybe<Boolean>) (hydra.util.Maybe.<Boolean>nothing()), hydra.ext.rdf.Utils.emptyLangStrings(), new hydra.ext.org.w3.shacl.model.Severity.Violation(), hydra.lib.sets.Singleton.apply(new hydra.ext.org.w3.rdf.syntax.RdfsClass(null)), (java.util.Set<hydra.ext.org.w3.rdf.syntax.IriOrLiteral>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.IriOrLiteral>apply()), (java.util.Set<hydra.ext.org.w3.rdf.syntax.Property>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.Property>apply()), (java.util.Set<hydra.ext.org.w3.rdf.syntax.Property>) (hydra.lib.sets.Empty.<hydra.ext.org.w3.rdf.syntax.Property>apply())));
    hydra.util.Lazy<hydra.pg.model.VertexLabel> label = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.pg.model.VertexType<T0>, hydra.pg.model.VertexLabel>) (projected -> projected.label)).apply(vt));
    hydra.ext.org.w3.rdf.syntax.Iri labelIri = (encodeLabel).apply(label.get());
    return (hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.Shape>) (new hydra.ext.org.w3.shacl.model.Definition<hydra.ext.org.w3.shacl.model.Shape>(labelIri, new hydra.ext.org.w3.shacl.model.Shape.Node(new hydra.ext.org.w3.shacl.model.NodeShape(common.get()))));
  }

  static <T0> java.util.List<hydra.pg.model.PropertyType<T0>> vertexTypeToNodeShape_propTypes(hydra.pg.model.VertexType<T0> vt) {
    return ((java.util.function.Function<hydra.pg.model.VertexType<T0>, java.util.List<hydra.pg.model.PropertyType<T0>>>) (projected -> projected.properties)).apply(vt);
  }
}
