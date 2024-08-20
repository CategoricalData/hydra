// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * List functions
 */
public class ListFunctionFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/features.ListFunctionFeatures");
  
  public static final hydra.core.Name FIELD_NAME_KEYS = new hydra.core.Name("keys");
  
  public static final hydra.core.Name FIELD_NAME_LABELS = new hydra.core.Name("labels");
  
  public static final hydra.core.Name FIELD_NAME_NODES = new hydra.core.Name("nodes");
  
  public static final hydra.core.Name FIELD_NAME_RANGE = new hydra.core.Name("range");
  
  public static final hydra.core.Name FIELD_NAME_REDUCE = new hydra.core.Name("reduce");
  
  public static final hydra.core.Name FIELD_NAME_RELATIONSHIPS = new hydra.core.Name("relationships");
  
  public static final hydra.core.Name FIELD_NAME_REVERSE = new hydra.core.Name("reverse");
  
  public static final hydra.core.Name FIELD_NAME_TAIL = new hydra.core.Name("tail");
  
  public static final hydra.core.Name FIELD_NAME_TO_BOOLEAN_LIST = new hydra.core.Name("toBooleanList");
  
  public static final hydra.core.Name FIELD_NAME_TO_FLOAT_LIST = new hydra.core.Name("toFloatList");
  
  public static final hydra.core.Name FIELD_NAME_TO_INTEGER_LIST = new hydra.core.Name("toIntegerList");
  
  public static final hydra.core.Name FIELD_NAME_TO_STRING_LIST = new hydra.core.Name("toStringList");
  
  /**
   * The keys() function. Returns a LIST&lt;STRING&gt; containing the STRING representations for all the property names of a MAP.; Returns a LIST&lt;STRING&gt; containing the STRING representations for all the property names of a NODE.; Returns a LIST&lt;STRING&gt; containing the STRING representations for all the property names of a RELATIONSHIP.
   */
  public final Boolean keys;
  
  /**
   * The labels() function. Returns a LIST&lt;STRING&gt; containing the STRING representations for all the labels of a NODE.
   */
  public final Boolean labels;
  
  /**
   * The nodes() function. Returns a LIST&lt;NODE&gt; containing all the NODE values in a PATH.
   */
  public final Boolean nodes;
  
  /**
   * The range() function. Returns a LIST&lt;INTEGER&gt; comprising all INTEGER values within a specified range.; Returns a LIST&lt;INTEGER&gt; comprising all INTEGER values within a specified range created with step length.
   */
  public final Boolean range;
  
  /**
   * The reduce() function. Runs an expression against individual elements of a LIST&lt;ANY&gt;, storing the result of the expression in an accumulator.
   */
  public final Boolean reduce;
  
  /**
   * The relationships() function. Returns a LIST&lt;RELATIONSHIP&gt; containing all the RELATIONSHIP values in a PATH.
   */
  public final Boolean relationships;
  
  /**
   * The reverse() function. Returns a LIST&lt;ANY&gt; in which the order of all elements in the given LIST&lt;ANY&gt; have been reversed.
   */
  public final Boolean reverse;
  
  /**
   * The tail() function. Returns all but the first element in a LIST&lt;ANY&gt;.
   */
  public final Boolean tail;
  
  /**
   * The toBooleanList() function. Converts a LIST&lt;ANY&gt; of values to a LIST&lt;BOOLEAN&gt; values. If any values are not convertible to BOOLEAN they will be null in the LIST&lt;BOOLEAN&gt; returned.
   */
  public final Boolean toBooleanList;
  
  /**
   * The toFloatList() function. Converts a LIST&lt;ANY&gt; to a LIST&lt;FLOAT&gt; values. If any values are not convertible to FLOAT they will be null in the LIST&lt;FLOAT&gt; returned.
   */
  public final Boolean toFloatList;
  
  /**
   * The toIntegerList() function. Converts a LIST&lt;ANY&gt; to a LIST&lt;INTEGER&gt; values. If any values are not convertible to INTEGER they will be null in the LIST&lt;INTEGER&gt; returned.
   */
  public final Boolean toIntegerList;
  
  /**
   * The toStringList() function. Converts a LIST&lt;ANY&gt; to a LIST&lt;STRING&gt; values. If any values are not convertible to STRING they will be null in the LIST&lt;STRING&gt; returned.
   */
  public final Boolean toStringList;
  
  public ListFunctionFeatures (Boolean keys, Boolean labels, Boolean nodes, Boolean range, Boolean reduce, Boolean relationships, Boolean reverse, Boolean tail, Boolean toBooleanList, Boolean toFloatList, Boolean toIntegerList, Boolean toStringList) {
    java.util.Objects.requireNonNull((keys));
    java.util.Objects.requireNonNull((labels));
    java.util.Objects.requireNonNull((nodes));
    java.util.Objects.requireNonNull((range));
    java.util.Objects.requireNonNull((reduce));
    java.util.Objects.requireNonNull((relationships));
    java.util.Objects.requireNonNull((reverse));
    java.util.Objects.requireNonNull((tail));
    java.util.Objects.requireNonNull((toBooleanList));
    java.util.Objects.requireNonNull((toFloatList));
    java.util.Objects.requireNonNull((toIntegerList));
    java.util.Objects.requireNonNull((toStringList));
    this.keys = keys;
    this.labels = labels;
    this.nodes = nodes;
    this.range = range;
    this.reduce = reduce;
    this.relationships = relationships;
    this.reverse = reverse;
    this.tail = tail;
    this.toBooleanList = toBooleanList;
    this.toFloatList = toFloatList;
    this.toIntegerList = toIntegerList;
    this.toStringList = toStringList;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ListFunctionFeatures)) {
      return false;
    }
    ListFunctionFeatures o = (ListFunctionFeatures) (other);
    return keys.equals(o.keys) && labels.equals(o.labels) && nodes.equals(o.nodes) && range.equals(o.range) && reduce.equals(o.reduce) && relationships.equals(o.relationships) && reverse.equals(o.reverse) && tail.equals(o.tail) && toBooleanList.equals(o.toBooleanList) && toFloatList.equals(o.toFloatList) && toIntegerList.equals(o.toIntegerList) && toStringList.equals(o.toStringList);
  }
  
  @Override
  public int hashCode() {
    return 2 * keys.hashCode() + 3 * labels.hashCode() + 5 * nodes.hashCode() + 7 * range.hashCode() + 11 * reduce.hashCode() + 13 * relationships.hashCode() + 17 * reverse.hashCode() + 19 * tail.hashCode() + 23 * toBooleanList.hashCode() + 29 * toFloatList.hashCode() + 31 * toIntegerList.hashCode() + 37 * toStringList.hashCode();
  }
  
  public ListFunctionFeatures withKeys(Boolean keys) {
    java.util.Objects.requireNonNull((keys));
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withLabels(Boolean labels) {
    java.util.Objects.requireNonNull((labels));
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withNodes(Boolean nodes) {
    java.util.Objects.requireNonNull((nodes));
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withRange(Boolean range) {
    java.util.Objects.requireNonNull((range));
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withReduce(Boolean reduce) {
    java.util.Objects.requireNonNull((reduce));
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withRelationships(Boolean relationships) {
    java.util.Objects.requireNonNull((relationships));
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withReverse(Boolean reverse) {
    java.util.Objects.requireNonNull((reverse));
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withTail(Boolean tail) {
    java.util.Objects.requireNonNull((tail));
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withToBooleanList(Boolean toBooleanList) {
    java.util.Objects.requireNonNull((toBooleanList));
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withToFloatList(Boolean toFloatList) {
    java.util.Objects.requireNonNull((toFloatList));
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withToIntegerList(Boolean toIntegerList) {
    java.util.Objects.requireNonNull((toIntegerList));
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withToStringList(Boolean toStringList) {
    java.util.Objects.requireNonNull((toStringList));
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
}