// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.validate;

import hydra.langs.tinkerpop.propertyGraph.EdgeLabel;
import hydra.langs.tinkerpop.propertyGraph.VertexLabel;
import hydra.util.Opt;
import hydra.util.Tuple;

import java.util.function.Function;

/**
 * Utilities for validating property graphs against property graph schemas
 */
public interface Validate {
  static <T, V> java.util.function.Function<java.util.function.Function<V, String>, java.util.function.Function<Opt<Function<V, hydra.util.Opt<VertexLabel>>>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.EdgeType<T>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Edge<V>, Opt<String>>>>> validateEdge(java.util.function.Function<T, java.util.function.Function<V, Opt<String>>> checkValue) {
    return (java.util.function.Function<java.util.function.Function<V, String>, java.util.function.Function<Opt<Function<V, hydra.util.Opt<VertexLabel>>>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.EdgeType<T>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Edge<V>, Opt<String>>>>>) (showValue -> (java.util.function.Function<Opt<Function<V, hydra.util.Opt<VertexLabel>>>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.EdgeType<T>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Edge<V>, Opt<String>>>>) (labelForVertexId -> (java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.EdgeType<T>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Edge<V>, Opt<String>>>) (typ -> (java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Edge<V>, Opt<String>>) (el -> {
      java.util.function.Function<String, String> failWith = (hydra.langs.tinkerpop.validate.Validate.edgeError((showValue))).apply((el));
      Opt<String> checkProperties = hydra.lib.optionals.Map.apply(
        (java.util.function.Function<String, String>) (s2 -> ((failWith)).apply((hydra.langs.tinkerpop.validate.Validate.prepend("Invalid property")).apply((s2)))),
        ((hydra.langs.tinkerpop.validate.Validate.validateProperties((checkValue))).apply(((typ)).properties)).apply(((el)).properties));
      Opt<String> checkOut = (((labelForVertexId)).map((java.util.function.Function<java.util.function.Function<V, Opt<VertexLabel>>, Opt<String>>) (s0 -> ((((s0)).apply(((el)).out)).map((java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.VertexLabel, Opt<String>>) (s1 -> (hydra.langs.tinkerpop.validate.Validate.verify(hydra.lib.equality.EqualString.apply(
        ((s1)).value,
        (((typ)).out).value))).apply(((failWith)).apply((hydra.langs.tinkerpop.validate.Validate.prepend("Wrong out-vertex label")).apply((hydra.langs.tinkerpop.validate.Validate.vertexLabelMismatch(((typ)).out)).apply((s1)))))))).orElse(Opt.of(((failWith)).apply((hydra.langs.tinkerpop.validate.Validate.prepend("Out-vertex does not exist")).apply(((showValue)).apply(((el)).out)))))))).orElse(Opt.empty());
      hydra.langs.tinkerpop.propertyGraph.EdgeLabel checkLabel_expected = ((typ)).label;
      hydra.langs.tinkerpop.propertyGraph.EdgeLabel checkLabel_actual = ((el)).label;
      Opt<String> checkLabel = (hydra.langs.tinkerpop.validate.Validate.verify(hydra.lib.equality.EqualString.apply(
        ((checkLabel_actual)).value,
        ((checkLabel_expected)).value))).apply(((failWith)).apply((hydra.langs.tinkerpop.validate.Validate.prepend("Wrong label")).apply((hydra.langs.tinkerpop.validate.Validate.edgeLabelMismatch((checkLabel_expected))).apply((checkLabel_actual)))));
      Opt<String> checkIn = (((labelForVertexId)).map((java.util.function.Function<java.util.function.Function<V, Opt<VertexLabel>>, Opt<String>>) (f -> ((((f)).apply(((el)).in)).map((java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.VertexLabel, Opt<String>>) (label -> (hydra.langs.tinkerpop.validate.Validate.verify(hydra.lib.equality.EqualString.apply(
        ((label)).value,
        (((typ)).in).value))).apply(((failWith)).apply((hydra.langs.tinkerpop.validate.Validate.prepend("Wrong in-vertex label")).apply((hydra.langs.tinkerpop.validate.Validate.vertexLabelMismatch(((typ)).in)).apply((label)))))))).orElse(Opt.of(((failWith)).apply((hydra.langs.tinkerpop.validate.Validate.prepend("In-vertex does not exist")).apply(((showValue)).apply(((el)).in)))))))).orElse(Opt.empty());
      Opt<String> checkId = hydra.lib.optionals.Map.apply(
        (java.util.function.Function<String, String>) (x -> ((failWith)).apply((hydra.langs.tinkerpop.validate.Validate.prepend("Invalid id")).apply((x)))),
        (((checkValue)).apply(((typ)).id)).apply(((el)).id));
      return hydra.langs.tinkerpop.validate.Validate.checkAll(java.util.Arrays.asList(
        (checkLabel),
        (checkId),
        (checkProperties),
        (checkOut),
        (checkIn)));
    }))));
  }
  
  static <T, V> java.util.function.Function<java.util.function.Function<V, String>, java.util.function.Function<Opt<Function<V, hydra.util.Opt<VertexLabel>>>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.ElementType<T>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Element<V>, Opt<String>>>>> validateElement(java.util.function.Function<T, java.util.function.Function<V, Opt<String>>> checkValue) {
    return (java.util.function.Function<java.util.function.Function<V, String>, java.util.function.Function<Opt<Function<V, hydra.util.Opt<VertexLabel>>>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.ElementType<T>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Element<V>, Opt<String>>>>>) (showValue -> (java.util.function.Function<Opt<Function<V, hydra.util.Opt<VertexLabel>>>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.ElementType<T>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Element<V>, Opt<String>>>>) (labelForVertexId -> (java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.ElementType<T>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Element<V>, Opt<String>>>) (typ -> (java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Element<V>, Opt<String>>) (el -> ((typ)).accept(new hydra.langs.tinkerpop.propertyGraph.ElementType.Visitor<T, Opt<String>>() {
      @Override
      public Opt<String> visit(hydra.langs.tinkerpop.propertyGraph.ElementType.Vertex<T> instance) {
        return ((el)).accept(new hydra.langs.tinkerpop.propertyGraph.Element.Visitor<V, Opt<String>>() {
          @Override
          public Opt<String> visit(hydra.langs.tinkerpop.propertyGraph.Element.Edge<V> inst) {
            return Opt.of((hydra.langs.tinkerpop.validate.Validate.prepend("Edge instead of vertex")).apply(((showValue)).apply(((inst.value)).id)));
          }
          
          @Override
          public Opt<String> visit(hydra.langs.tinkerpop.propertyGraph.Element.Vertex<V> inst) {
            return (((hydra.langs.tinkerpop.validate.Validate.validateVertex((checkValue))).apply((showValue))).apply((instance.value))).apply((inst.value));
          }
        });
      }
      
      @Override
      public Opt<String> visit(hydra.langs.tinkerpop.propertyGraph.ElementType.Edge<T> instance) {
        return ((el)).accept(new hydra.langs.tinkerpop.propertyGraph.Element.Visitor<V, Opt<String>>() {
          @Override
          public Opt<String> visit(hydra.langs.tinkerpop.propertyGraph.Element.Vertex<V> inst) {
            return Opt.of((hydra.langs.tinkerpop.validate.Validate.prepend("Vertex instead of edge")).apply(((showValue)).apply(((inst.value)).id)));
          }
          
          @Override
          public Opt<String> visit(hydra.langs.tinkerpop.propertyGraph.Element.Edge<V> inst) {
            return ((((hydra.langs.tinkerpop.validate.Validate.validateEdge((checkValue))).apply((showValue))).apply((labelForVertexId))).apply((instance.value))).apply((inst.value));
          }
        });
      }
    })))));
  }
  
  static <T, V> java.util.function.Function<java.util.function.Function<V, String>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.GraphSchema<T>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Graph<V>, Opt<String>>>> validateGraph(java.util.function.Function<T, java.util.function.Function<V, Opt<String>>> checkValue) {
    return (java.util.function.Function<java.util.function.Function<V, String>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.GraphSchema<T>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Graph<V>, Opt<String>>>>) (showValue -> (java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.GraphSchema<T>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Graph<V>, Opt<String>>>) (schema -> (java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Graph<V>, Opt<String>>) (graph -> {
      java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Vertex<V>, Opt<String>> checkVertices_checkVertex = (java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Vertex<V>, Opt<String>>) (s0 -> ((hydra.lib.maps.Lookup.apply(
        ((s0)).label,
        ((schema)).vertices)).map((java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.VertexType<T>, Opt<String>>) (s1 -> (((hydra.langs.tinkerpop.validate.Validate.validateVertex((checkValue))).apply((showValue))).apply((s1))).apply((s0))))).orElse(Opt.of(((hydra.langs.tinkerpop.validate.Validate.vertexError((showValue))).apply((s0))).apply((hydra.langs.tinkerpop.validate.Validate.prepend("Unexpected label")).apply((((s0)).label).value)))));
      Opt<String> checkVertices = hydra.langs.tinkerpop.validate.Validate.checkAll(hydra.lib.lists.Map.apply(
        (checkVertices_checkVertex),
        hydra.lib.maps.Values.apply(((graph)).vertices)));
      Opt<Function<V, hydra.util.Opt<VertexLabel>>> checkEdges_labelForVertexId = Opt.of((java.util.function.Function<V, Opt<VertexLabel>>) (i -> hydra.lib.optionals.Map.apply(
        (java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Vertex<V>, hydra.langs.tinkerpop.propertyGraph.VertexLabel>) (v1 -> ((v1)).label),
        hydra.lib.maps.Lookup.apply(
          (i),
          ((graph)).vertices))));
      java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Edge<V>, Opt<String>> checkEdges_checkEdge = (java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Edge<V>, Opt<String>>) (el -> ((hydra.lib.maps.Lookup.apply(
        ((el)).label,
        ((schema)).edges)).map((java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.EdgeType<T>, Opt<String>>) (t -> ((((hydra.langs.tinkerpop.validate.Validate.validateEdge((checkValue))).apply((showValue))).apply((checkEdges_labelForVertexId))).apply((t))).apply((el))))).orElse(Opt.of(((hydra.langs.tinkerpop.validate.Validate.edgeError((showValue))).apply((el))).apply((hydra.langs.tinkerpop.validate.Validate.prepend("Unexpected label")).apply((((el)).label).value)))));
      Opt<String> checkEdges = hydra.langs.tinkerpop.validate.Validate.checkAll(hydra.lib.lists.Map.apply(
        (checkEdges_checkEdge),
        hydra.lib.maps.Values.apply(((graph)).edges)));
      return hydra.langs.tinkerpop.validate.Validate.checkAll(java.util.Arrays.asList(
        (checkVertices),
        (checkEdges)));
    })));
  }
  
  static <T, V> java.util.function.Function<java.util.List<hydra.langs.tinkerpop.propertyGraph.PropertyType<T>>, java.util.function.Function<java.util.Map<hydra.langs.tinkerpop.propertyGraph.PropertyKey, V>, Opt<String>>> validateProperties(java.util.function.Function<T, java.util.function.Function<V, Opt<String>>> checkValue) {
    return (java.util.function.Function<java.util.List<hydra.langs.tinkerpop.propertyGraph.PropertyType<T>>, java.util.function.Function<java.util.Map<hydra.langs.tinkerpop.propertyGraph.PropertyKey, V>, Opt<String>>>) (types -> (java.util.function.Function<java.util.Map<hydra.langs.tinkerpop.propertyGraph.PropertyKey, V>, Opt<String>>) (props -> {
      java.util.Map<hydra.langs.tinkerpop.propertyGraph.PropertyKey, T> checkValues_m = hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.PropertyType<T>, Tuple.Tuple2<hydra.langs.tinkerpop.propertyGraph.PropertyKey, T>>) (p -> new Tuple.Tuple2(((p)).key, ((p)).value)),
        (types)));
      java.util.function.Function<Tuple.Tuple2<hydra.langs.tinkerpop.propertyGraph.PropertyKey, V>, Opt<String>> checkValues_checkPair = (java.util.function.Function<Tuple.Tuple2<hydra.langs.tinkerpop.propertyGraph.PropertyKey, V>, Opt<String>>) (pair -> {
        V val = ((pair)).object2;
        hydra.langs.tinkerpop.propertyGraph.PropertyKey key = ((pair)).object1;
        return ((hydra.lib.maps.Lookup.apply(
          (key),
          (checkValues_m))).map((java.util.function.Function<T, Opt<String>>) (typ -> hydra.lib.optionals.Map.apply(
          hydra.langs.tinkerpop.validate.Validate.prepend("Invalid value"),
          (((checkValue)).apply((typ))).apply((val)))))).orElse(Opt.of((hydra.langs.tinkerpop.validate.Validate.prepend("Unexpected key")).apply(((key)).value)));
      });
      Opt<String> checkValues = hydra.langs.tinkerpop.validate.Validate.checkAll(hydra.lib.lists.Map.apply(
        (checkValues_checkPair),
        hydra.lib.maps.ToList.apply((props))));
      java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.PropertyType<T>, Opt<String>> checkType = (java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.PropertyType<T>, Opt<String>>) (t -> hydra.lib.logic.IfElse.apply(
        ((hydra.lib.maps.Lookup.apply(
          ((t)).key,
          (props))).map((java.util.function.Function<V, Opt<String>>) (ignored -> Opt.empty()))).orElse(Opt.of((hydra.langs.tinkerpop.validate.Validate.prepend("Missing value for ")).apply((((t)).key).value))),
        Opt.empty(),
        ((t)).required));
      Opt<String> checkTypes = hydra.langs.tinkerpop.validate.Validate.checkAll(hydra.lib.lists.Map.apply(
        (checkType),
        (types)));
      return hydra.langs.tinkerpop.validate.Validate.checkAll(java.util.Arrays.asList(
        (checkTypes),
        (checkValues)));
    }));
  }
  
  static <T, V> java.util.function.Function<java.util.function.Function<V, String>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.VertexType<T>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Vertex<V>, Opt<String>>>> validateVertex(java.util.function.Function<T, java.util.function.Function<V, Opt<String>>> checkValue) {
    return (java.util.function.Function<java.util.function.Function<V, String>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.VertexType<T>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Vertex<V>, Opt<String>>>>) (showValue -> (java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.VertexType<T>, java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Vertex<V>, Opt<String>>>) (typ -> (java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Vertex<V>, Opt<String>>) (el -> {
      java.util.function.Function<String, String> failWith = (hydra.langs.tinkerpop.validate.Validate.vertexError((showValue))).apply((el));
      Opt<String> checkProperties = hydra.lib.optionals.Map.apply(
        (java.util.function.Function<String, String>) (s0 -> ((failWith)).apply((hydra.langs.tinkerpop.validate.Validate.prepend("Invalid property")).apply((s0)))),
        ((hydra.langs.tinkerpop.validate.Validate.validateProperties((checkValue))).apply(((typ)).properties)).apply(((el)).properties));
      hydra.langs.tinkerpop.propertyGraph.VertexLabel checkLabel_expected = ((typ)).label;
      hydra.langs.tinkerpop.propertyGraph.VertexLabel checkLabel_actual = ((el)).label;
      Opt<String> checkLabel = (hydra.langs.tinkerpop.validate.Validate.verify(hydra.lib.equality.EqualString.apply(
        ((checkLabel_actual)).value,
        ((checkLabel_expected)).value))).apply(((failWith)).apply((hydra.langs.tinkerpop.validate.Validate.prepend("Wrong label")).apply((hydra.langs.tinkerpop.validate.Validate.vertexLabelMismatch((checkLabel_expected))).apply((checkLabel_actual)))));
      Opt<String> checkId = hydra.lib.optionals.Map.apply(
        (java.util.function.Function<String, String>) (x -> ((failWith)).apply((hydra.langs.tinkerpop.validate.Validate.prepend("Invalid id")).apply((x)))),
        (((checkValue)).apply(((typ)).id)).apply(((el)).id));
      return hydra.langs.tinkerpop.validate.Validate.checkAll(java.util.Arrays.asList(
        (checkLabel),
        (checkId),
        (checkProperties)));
    })));
  }
  
  static <A> Opt<A> checkAll(java.util.List<Opt<A>> checks) {
    java.util.List<A> errors = hydra.lib.optionals.Cat.apply((checks));
    return hydra.lib.lists.SafeHead.apply(errors);
  }
  
  static <V> java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Edge<V>, java.util.function.Function<String, String>> edgeError(java.util.function.Function<V, String> a1) {
    return (java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Edge<V>, java.util.function.Function<String, String>>) (e -> hydra.langs.tinkerpop.validate.Validate.prepend(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      "Invalid edge with id ",
      ((a1)).apply(((e)).id)))));
  }
  
  static java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.EdgeLabel, String> edgeLabelMismatch(EdgeLabel expected) {
    return (java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.EdgeLabel, String>) (actual -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
        hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "expected ",
          ((expected)).value)),
        ", found ")),
      ((actual)).value)));
  }
  
  static java.util.function.Function<String, String> prepend(String prefix) {
    return (java.util.function.Function<String, String>) (msg -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
        (prefix),
        ": ")),
      (msg))));
  }
  
  static java.util.function.Function<String, Opt<String>> verify(Boolean b) {
    return (java.util.function.Function<String, Opt<String>>) (err -> hydra.lib.logic.IfElse.apply(
      Opt.empty(),
      Opt.of((err)),
      (b)));
  }
  
  static <V> java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Vertex<V>, java.util.function.Function<String, String>> vertexError(java.util.function.Function<V, String> a1) {
    return (java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.Vertex<V>, java.util.function.Function<String, String>>) (v -> hydra.langs.tinkerpop.validate.Validate.prepend(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      "Invalid vertex with id ",
      ((a1)).apply(((v)).id)))));
  }
  
  static java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.VertexLabel, String> vertexLabelMismatch(VertexLabel expected) {
    return (java.util.function.Function<hydra.langs.tinkerpop.propertyGraph.VertexLabel, String>) (actual -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
        hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "expected ",
          ((expected)).value)),
        ", found ")),
      ((actual)).value)));
  }
}
