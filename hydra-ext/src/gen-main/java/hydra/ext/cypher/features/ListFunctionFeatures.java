// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * List functions
 */
public class ListFunctionFeatures implements Serializable, Comparable<ListFunctionFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.features.ListFunctionFeatures");
  
  public static final hydra.core.Name KEYS = new hydra.core.Name("keys");
  
  public static final hydra.core.Name LABELS = new hydra.core.Name("labels");
  
  public static final hydra.core.Name NODES = new hydra.core.Name("nodes");
  
  public static final hydra.core.Name RANGE = new hydra.core.Name("range");
  
  public static final hydra.core.Name REDUCE = new hydra.core.Name("reduce");
  
  public static final hydra.core.Name RELATIONSHIPS = new hydra.core.Name("relationships");
  
  public static final hydra.core.Name REVERSE = new hydra.core.Name("reverse");
  
  public static final hydra.core.Name TAIL = new hydra.core.Name("tail");
  
  public static final hydra.core.Name TO_BOOLEAN_LIST = new hydra.core.Name("toBooleanList");
  
  public static final hydra.core.Name TO_FLOAT_LIST = new hydra.core.Name("toFloatList");
  
  public static final hydra.core.Name TO_INTEGER_LIST = new hydra.core.Name("toIntegerList");
  
  public static final hydra.core.Name TO_STRING_LIST = new hydra.core.Name("toStringList");
  
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
    ListFunctionFeatures o = (ListFunctionFeatures) other;
    return java.util.Objects.equals(
      this.keys,
      o.keys) && java.util.Objects.equals(
      this.labels,
      o.labels) && java.util.Objects.equals(
      this.nodes,
      o.nodes) && java.util.Objects.equals(
      this.range,
      o.range) && java.util.Objects.equals(
      this.reduce,
      o.reduce) && java.util.Objects.equals(
      this.relationships,
      o.relationships) && java.util.Objects.equals(
      this.reverse,
      o.reverse) && java.util.Objects.equals(
      this.tail,
      o.tail) && java.util.Objects.equals(
      this.toBooleanList,
      o.toBooleanList) && java.util.Objects.equals(
      this.toFloatList,
      o.toFloatList) && java.util.Objects.equals(
      this.toIntegerList,
      o.toIntegerList) && java.util.Objects.equals(
      this.toStringList,
      o.toStringList);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(keys) + 3 * java.util.Objects.hashCode(labels) + 5 * java.util.Objects.hashCode(nodes) + 7 * java.util.Objects.hashCode(range) + 11 * java.util.Objects.hashCode(reduce) + 13 * java.util.Objects.hashCode(relationships) + 17 * java.util.Objects.hashCode(reverse) + 19 * java.util.Objects.hashCode(tail) + 23 * java.util.Objects.hashCode(toBooleanList) + 29 * java.util.Objects.hashCode(toFloatList) + 31 * java.util.Objects.hashCode(toIntegerList) + 37 * java.util.Objects.hashCode(toStringList);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ListFunctionFeatures other) {
    int cmp = 0;
    cmp = ((Comparable) keys).compareTo(other.keys);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) labels).compareTo(other.labels);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) nodes).compareTo(other.nodes);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) range).compareTo(other.range);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) reduce).compareTo(other.reduce);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) relationships).compareTo(other.relationships);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) reverse).compareTo(other.reverse);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) tail).compareTo(other.tail);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) toBooleanList).compareTo(other.toBooleanList);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) toFloatList).compareTo(other.toFloatList);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) toIntegerList).compareTo(other.toIntegerList);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) toStringList).compareTo(other.toStringList);
  }
  
  public ListFunctionFeatures withKeys(Boolean keys) {
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withLabels(Boolean labels) {
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withNodes(Boolean nodes) {
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withRange(Boolean range) {
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withReduce(Boolean reduce) {
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withRelationships(Boolean relationships) {
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withReverse(Boolean reverse) {
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withTail(Boolean tail) {
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withToBooleanList(Boolean toBooleanList) {
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withToFloatList(Boolean toFloatList) {
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withToIntegerList(Boolean toIntegerList) {
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFunctionFeatures withToStringList(Boolean toStringList) {
    return new ListFunctionFeatures(keys, labels, nodes, range, reduce, relationships, reverse, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
}
