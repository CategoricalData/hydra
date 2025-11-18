// Note: this is an automatically generated file. Do not edit.

package hydra.pg.validation;

import hydra.lib.maps.Elems;
import hydra.pg.model.EdgeLabel;
import hydra.pg.model.VertexLabel;
import hydra.util.Maybe;
import hydra.util.Tuple;

import java.util.function.Function;

/**
 * Utilities for validating property graphs against property graph schemas
 */
public interface Validation {
  static <T, V> java.util.function.Function<java.util.function.Function<V, String>, java.util.function.Function<Maybe<Function<V, Maybe<VertexLabel>>>, java.util.function.Function<hydra.pg.model.EdgeType<T>, java.util.function.Function<hydra.pg.model.Edge<V>, Maybe<String>>>>> validateEdge(java.util.function.Function<T, java.util.function.Function<V, Maybe<String>>> checkValue) {
    return (java.util.function.Function<java.util.function.Function<V, String>, java.util.function.Function<Maybe<Function<V, Maybe<VertexLabel>>>, java.util.function.Function<hydra.pg.model.EdgeType<T>, java.util.function.Function<hydra.pg.model.Edge<V>, Maybe<String>>>>>) (showValue -> (java.util.function.Function<Maybe<Function<V, Maybe<VertexLabel>>>, java.util.function.Function<hydra.pg.model.EdgeType<T>, java.util.function.Function<hydra.pg.model.Edge<V>, Maybe<String>>>>) (labelForVertexId -> (java.util.function.Function<hydra.pg.model.EdgeType<T>, java.util.function.Function<hydra.pg.model.Edge<V>, Maybe<String>>>) (typ -> (java.util.function.Function<hydra.pg.model.Edge<V>, Maybe<String>>) (el -> {
      java.util.function.Function<String, String> failWith = (hydra.pg.validation.Validation.edgeError((showValue))).apply((el));
      Maybe<String> checkProperties = hydra.lib.maybes.Map.apply(
        (java.util.function.Function<String, String>) (s2 -> ((failWith)).apply((hydra.pg.validation.Validation.prepend("Invalid property")).apply((s2)))),
        ((hydra.pg.validation.Validation.validateProperties((checkValue))).apply(((typ)).properties)).apply(((el)).properties));
      Maybe<String> checkOut = (((labelForVertexId)).map((java.util.function.Function<java.util.function.Function<V, Maybe<VertexLabel>>, Maybe<String>>) (s0 -> ((((s0)).apply(((el)).out)).map((java.util.function.Function<hydra.pg.model.VertexLabel, Maybe<String>>) (s1 -> (hydra.pg.validation.Validation.verify(hydra.lib.equality.Equal.apply(
        ((s1)).value,
        (((typ)).out).value))).apply(((failWith)).apply((hydra.pg.validation.Validation.prepend("Wrong out-vertex label")).apply((hydra.pg.validation.Validation.vertexLabelMismatch(((typ)).out)).apply((s1)))))))).orElse(Maybe.just(((failWith)).apply((hydra.pg.validation.Validation.prepend("Out-vertex does not exist")).apply(((showValue)).apply(((el)).out)))))))).orElse(Maybe.nothing());
      hydra.pg.model.EdgeLabel checkLabel_expected = ((typ)).label;
      hydra.pg.model.EdgeLabel checkLabel_actual = ((el)).label;
      Maybe<String> checkLabel = (hydra.pg.validation.Validation.verify(hydra.lib.equality.Equal.apply(
        ((checkLabel_actual)).value,
        ((checkLabel_expected)).value))).apply(((failWith)).apply((hydra.pg.validation.Validation.prepend("Wrong label")).apply((hydra.pg.validation.Validation.edgeLabelMismatch((checkLabel_expected))).apply((checkLabel_actual)))));
      Maybe<String> checkIn = (((labelForVertexId)).map((java.util.function.Function<java.util.function.Function<V, Maybe<VertexLabel>>, Maybe<String>>) (f -> ((((f)).apply(((el)).in)).map((java.util.function.Function<hydra.pg.model.VertexLabel, Maybe<String>>) (label -> (hydra.pg.validation.Validation.verify(hydra.lib.equality.Equal.apply(
        ((label)).value,
        (((typ)).in).value))).apply(((failWith)).apply((hydra.pg.validation.Validation.prepend("Wrong in-vertex label")).apply((hydra.pg.validation.Validation.vertexLabelMismatch(((typ)).in)).apply((label)))))))).orElse(Maybe.just(((failWith)).apply((hydra.pg.validation.Validation.prepend("In-vertex does not exist")).apply(((showValue)).apply(((el)).in)))))))).orElse(Maybe.nothing());
      Maybe<String> checkId = hydra.lib.maybes.Map.apply(
        (java.util.function.Function<String, String>) (x -> ((failWith)).apply((hydra.pg.validation.Validation.prepend("Invalid id")).apply((x)))),
        (((checkValue)).apply(((typ)).id)).apply(((el)).id));
      return hydra.pg.validation.Validation.checkAll(java.util.Arrays.asList(
        (checkLabel),
        (checkId),
        (checkProperties),
        (checkOut),
        (checkIn)));
    }))));
  }
  
  static <T, V> java.util.function.Function<java.util.function.Function<V, String>, java.util.function.Function<Maybe<Function<V, Maybe<VertexLabel>>>, java.util.function.Function<hydra.pg.model.ElementType<T>, java.util.function.Function<hydra.pg.model.Element<V>, Maybe<String>>>>> validateElement(java.util.function.Function<T, java.util.function.Function<V, Maybe<String>>> checkValue) {
    return (java.util.function.Function<java.util.function.Function<V, String>, java.util.function.Function<Maybe<Function<V, Maybe<VertexLabel>>>, java.util.function.Function<hydra.pg.model.ElementType<T>, java.util.function.Function<hydra.pg.model.Element<V>, Maybe<String>>>>>) (showValue -> (java.util.function.Function<Maybe<Function<V, Maybe<VertexLabel>>>, java.util.function.Function<hydra.pg.model.ElementType<T>, java.util.function.Function<hydra.pg.model.Element<V>, Maybe<String>>>>) (labelForVertexId -> (java.util.function.Function<hydra.pg.model.ElementType<T>, java.util.function.Function<hydra.pg.model.Element<V>, Maybe<String>>>) (typ -> (java.util.function.Function<hydra.pg.model.Element<V>, Maybe<String>>) (el -> ((typ)).accept(new hydra.pg.model.ElementType.Visitor<T, Maybe<String>>() {
      @Override
      public Maybe<String> visit(hydra.pg.model.ElementType.Vertex<T> instance) {
        return ((el)).accept(new hydra.pg.model.Element.Visitor<V, Maybe<String>>() {
          @Override
          public Maybe<String> visit(hydra.pg.model.Element.Edge<V> inst) {
            return Maybe.just((hydra.pg.validation.Validation.prepend("Edge instead of vertex")).apply(((showValue)).apply(((inst.value)).id)));
          }
          
          @Override
          public Maybe<String> visit(hydra.pg.model.Element.Vertex<V> inst) {
            return (((hydra.pg.validation.Validation.validateVertex((checkValue))).apply((showValue))).apply((instance.value))).apply((inst.value));
          }
        });
      }
      
      @Override
      public Maybe<String> visit(hydra.pg.model.ElementType.Edge<T> instance) {
        return ((el)).accept(new hydra.pg.model.Element.Visitor<V, Maybe<String>>() {
          @Override
          public Maybe<String> visit(hydra.pg.model.Element.Vertex<V> inst) {
            return Maybe.just((hydra.pg.validation.Validation.prepend("Vertex instead of edge")).apply(((showValue)).apply(((inst.value)).id)));
          }
          
          @Override
          public Maybe<String> visit(hydra.pg.model.Element.Edge<V> inst) {
            return ((((hydra.pg.validation.Validation.validateEdge((checkValue))).apply((showValue))).apply((labelForVertexId))).apply((instance.value))).apply((inst.value));
          }
        });
      }
    })))));
  }
  
  static <T, V> java.util.function.Function<java.util.function.Function<V, String>, java.util.function.Function<hydra.pg.model.GraphSchema<T>, java.util.function.Function<hydra.pg.model.Graph<V>, Maybe<String>>>> validateGraph(java.util.function.Function<T, java.util.function.Function<V, Maybe<String>>> checkValue) {
    return (java.util.function.Function<java.util.function.Function<V, String>, java.util.function.Function<hydra.pg.model.GraphSchema<T>, java.util.function.Function<hydra.pg.model.Graph<V>, Maybe<String>>>>) (showValue -> (java.util.function.Function<hydra.pg.model.GraphSchema<T>, java.util.function.Function<hydra.pg.model.Graph<V>, Maybe<String>>>) (schema -> (java.util.function.Function<hydra.pg.model.Graph<V>, Maybe<String>>) (graph -> {
      java.util.function.Function<hydra.pg.model.Vertex<V>, Maybe<String>> checkVertices_checkVertex = (java.util.function.Function<hydra.pg.model.Vertex<V>, Maybe<String>>) (s0 -> ((hydra.lib.maps.Lookup.apply(
        ((s0)).label,
        ((schema)).vertices)).map((java.util.function.Function<hydra.pg.model.VertexType<T>, Maybe<String>>) (s1 -> (((hydra.pg.validation.Validation.validateVertex((checkValue))).apply((showValue))).apply((s1))).apply((s0))))).orElse(Maybe.just(((hydra.pg.validation.Validation.vertexError((showValue))).apply((s0))).apply((hydra.pg.validation.Validation.prepend("Unexpected label")).apply((((s0)).label).value)))));
      Maybe<String> checkVertices = hydra.pg.validation.Validation.checkAll(hydra.lib.lists.Map.apply(
        (checkVertices_checkVertex),
        Elems.apply(((graph)).vertices)));
      Maybe<Function<V, Maybe<VertexLabel>>> checkEdges_labelForVertexId = Maybe.just((java.util.function.Function<V, Maybe<VertexLabel>>) (i -> hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.pg.model.Vertex<V>, hydra.pg.model.VertexLabel>) (v1 -> ((v1)).label),
        hydra.lib.maps.Lookup.apply(
          (i),
          ((graph)).vertices))));
      java.util.function.Function<hydra.pg.model.Edge<V>, Maybe<String>> checkEdges_checkEdge = (java.util.function.Function<hydra.pg.model.Edge<V>, Maybe<String>>) (el -> ((hydra.lib.maps.Lookup.apply(
        ((el)).label,
        ((schema)).edges)).map((java.util.function.Function<hydra.pg.model.EdgeType<T>, Maybe<String>>) (t -> ((((hydra.pg.validation.Validation.validateEdge((checkValue))).apply((showValue))).apply((checkEdges_labelForVertexId))).apply((t))).apply((el))))).orElse(Maybe.just(((hydra.pg.validation.Validation.edgeError((showValue))).apply((el))).apply((hydra.pg.validation.Validation.prepend("Unexpected label")).apply((((el)).label).value)))));
      Maybe<String> checkEdges = hydra.pg.validation.Validation.checkAll(hydra.lib.lists.Map.apply(
        (checkEdges_checkEdge),
        Elems.apply(((graph)).edges)));
      return hydra.pg.validation.Validation.checkAll(java.util.Arrays.asList(
        (checkVertices),
        (checkEdges)));
    })));
  }
  
  static <T, V> java.util.function.Function<java.util.List<hydra.pg.model.PropertyType<T>>, java.util.function.Function<java.util.Map<hydra.pg.model.PropertyKey, V>, Maybe<String>>> validateProperties(java.util.function.Function<T, java.util.function.Function<V, Maybe<String>>> checkValue) {
    return (java.util.function.Function<java.util.List<hydra.pg.model.PropertyType<T>>, java.util.function.Function<java.util.Map<hydra.pg.model.PropertyKey, V>, Maybe<String>>>) (types -> (java.util.function.Function<java.util.Map<hydra.pg.model.PropertyKey, V>, Maybe<String>>) (props -> {
      java.util.Map<hydra.pg.model.PropertyKey, T> checkValues_m = hydra.lib.maps.FromList.apply(hydra.lib.lists.Map.apply(
        (java.util.function.Function<hydra.pg.model.PropertyType<T>, Tuple.Tuple2<hydra.pg.model.PropertyKey, T>>) (p -> new Tuple.Tuple2(((p)).key, ((p)).value)),
        (types)));
      java.util.function.Function<Tuple.Tuple2<hydra.pg.model.PropertyKey, V>, Maybe<String>> checkValues_checkPair = (java.util.function.Function<Tuple.Tuple2<hydra.pg.model.PropertyKey, V>, Maybe<String>>) (pair -> {
        V val = ((pair)).object2;
        hydra.pg.model.PropertyKey key = ((pair)).object1;
        return ((hydra.lib.maps.Lookup.apply(
          (key),
          (checkValues_m))).map((java.util.function.Function<T, Maybe<String>>) (typ -> hydra.lib.maybes.Map.apply(
          hydra.pg.validation.Validation.prepend("Invalid value"),
          (((checkValue)).apply((typ))).apply((val)))))).orElse(Maybe.just((hydra.pg.validation.Validation.prepend("Unexpected key")).apply(((key)).value)));
      });
      Maybe<String> checkValues = hydra.pg.validation.Validation.checkAll(hydra.lib.lists.Map.apply(
        (checkValues_checkPair),
        hydra.lib.maps.ToList.apply((props))));
      java.util.function.Function<hydra.pg.model.PropertyType<T>, Maybe<String>> checkType = (java.util.function.Function<hydra.pg.model.PropertyType<T>, Maybe<String>>) (t -> hydra.lib.logic.IfElse.apply(
              ((t)).required,
        ((hydra.lib.maps.Lookup.apply(
          ((t)).key,
          (props))).map((java.util.function.Function<V, Maybe<String>>) (ignored -> Maybe.nothing()))).orElse(Maybe.just((hydra.pg.validation.Validation.prepend("Missing value for ")).apply((((t)).key).value))),
        Maybe.nothing()
        ));
      Maybe<String> checkTypes = hydra.pg.validation.Validation.checkAll(hydra.lib.lists.Map.apply(
        (checkType),
        (types)));
      return hydra.pg.validation.Validation.checkAll(java.util.Arrays.asList(
        (checkTypes),
        (checkValues)));
    }));
  }
  
  static <T, V> java.util.function.Function<java.util.function.Function<V, String>, java.util.function.Function<hydra.pg.model.VertexType<T>, java.util.function.Function<hydra.pg.model.Vertex<V>, Maybe<String>>>> validateVertex(java.util.function.Function<T, java.util.function.Function<V, Maybe<String>>> checkValue) {
    return (java.util.function.Function<java.util.function.Function<V, String>, java.util.function.Function<hydra.pg.model.VertexType<T>, java.util.function.Function<hydra.pg.model.Vertex<V>, Maybe<String>>>>) (showValue -> (java.util.function.Function<hydra.pg.model.VertexType<T>, java.util.function.Function<hydra.pg.model.Vertex<V>, Maybe<String>>>) (typ -> (java.util.function.Function<hydra.pg.model.Vertex<V>, Maybe<String>>) (el -> {
      java.util.function.Function<String, String> failWith = (hydra.pg.validation.Validation.vertexError((showValue))).apply((el));
      Maybe<String> checkProperties = hydra.lib.maybes.Map.apply(
        (java.util.function.Function<String, String>) (s0 -> ((failWith)).apply((hydra.pg.validation.Validation.prepend("Invalid property")).apply((s0)))),
        ((hydra.pg.validation.Validation.validateProperties((checkValue))).apply(((typ)).properties)).apply(((el)).properties));
      hydra.pg.model.VertexLabel checkLabel_expected = ((typ)).label;
      hydra.pg.model.VertexLabel checkLabel_actual = ((el)).label;
      Maybe<String> checkLabel = (hydra.pg.validation.Validation.verify(hydra.lib.equality.Equal.apply(
        ((checkLabel_actual)).value,
        ((checkLabel_expected)).value))).apply(((failWith)).apply((hydra.pg.validation.Validation.prepend("Wrong label")).apply((hydra.pg.validation.Validation.vertexLabelMismatch((checkLabel_expected))).apply((checkLabel_actual)))));
      Maybe<String> checkId = hydra.lib.maybes.Map.apply(
        (java.util.function.Function<String, String>) (x -> ((failWith)).apply((hydra.pg.validation.Validation.prepend("Invalid id")).apply((x)))),
        (((checkValue)).apply(((typ)).id)).apply(((el)).id));
      return hydra.pg.validation.Validation.checkAll(java.util.Arrays.asList(
        (checkLabel),
        (checkId),
        (checkProperties)));
    })));
  }
  
  static <A> Maybe<A> checkAll(java.util.List<Maybe<A>> checks) {
    java.util.List<A> errors = hydra.lib.maybes.Cat.apply((checks));
    return hydra.lib.lists.SafeHead.apply(errors);
  }
  
  static <V> java.util.function.Function<hydra.pg.model.Edge<V>, java.util.function.Function<String, String>> edgeError(java.util.function.Function<V, String> a1) {
    return (java.util.function.Function<hydra.pg.model.Edge<V>, java.util.function.Function<String, String>>) (e -> hydra.pg.validation.Validation.prepend(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      "Invalid edge with id ",
      ((a1)).apply(((e)).id)))));
  }
  
  static java.util.function.Function<hydra.pg.model.EdgeLabel, String> edgeLabelMismatch(EdgeLabel expected) {
    return (java.util.function.Function<hydra.pg.model.EdgeLabel, String>) (actual -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
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
  
  static java.util.function.Function<String, Maybe<String>> verify(Boolean b) {
    return (java.util.function.Function<String, Maybe<String>>) (err -> hydra.lib.logic.IfElse.apply(
            b,
      Maybe.nothing(),
      Maybe.just((err))));
  }
  
  static <V> java.util.function.Function<hydra.pg.model.Vertex<V>, java.util.function.Function<String, String>> vertexError(java.util.function.Function<V, String> a1) {
    return (java.util.function.Function<hydra.pg.model.Vertex<V>, java.util.function.Function<String, String>>) (v -> hydra.pg.validation.Validation.prepend(hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      "Invalid vertex with id ",
      ((a1)).apply(((v)).id)))));
  }
  
  static java.util.function.Function<hydra.pg.model.VertexLabel, String> vertexLabelMismatch(VertexLabel expected) {
    return (java.util.function.Function<hydra.pg.model.VertexLabel, String>) (actual -> hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
      hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
        hydra.lib.strings.Cat.apply(java.util.Arrays.asList(
          "expected ",
          ((expected)).value)),
        ", found ")),
      ((actual)).value)));
  }
}
