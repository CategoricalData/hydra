// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.features;

import java.io.Serializable;

/**
 * A set of features for list functionality.
 */
public class ListFeatures implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/features.ListFeatures");
  
  /**
   * Whether to expect the all() function.
   */
  public final Boolean all;
  
  /**
   * Whether to expect the any() function.
   */
  public final Boolean any;
  
  /**
   * Whether to expect the coalesce() function.
   */
  public final Boolean coalesce;
  
  /**
   * Whether to expect the isEmpty() function.
   */
  public final Boolean isEmpty;
  
  /**
   * Whether to expect the head() function.
   */
  public final Boolean head;
  
  /**
   * Whether to expect the last() function.
   */
  public final Boolean last;
  
  /**
   * Whether to expect basic list comprehensions.
   */
  public final Boolean listComprehension;
  
  /**
   * Whether to expect list range comprehensions (e.g. [1..10]).
   */
  public final Boolean listRange;
  
  /**
   * Whether to expect the none() function.
   */
  public final Boolean none;
  
  /**
   * Whether to expect the reduce() function.
   */
  public final Boolean reduce;
  
  /**
   * Whether to expect the reverse() function.
   */
  public final Boolean reverse;
  
  /**
   * Whether to expect the single() function.
   */
  public final Boolean single;
  
  /**
   * Whether to expect the size() function.
   */
  public final Boolean size;
  
  /**
   * Whether to expect the tail() function.
   */
  public final Boolean tail;
  
  /**
   * Whether to expect the toBooleanList() function.
   */
  public final Boolean toBooleanList;
  
  /**
   * Whether to expect the toFloatList() function.
   */
  public final Boolean toFloatList;
  
  /**
   * Whether to expect the toIntegerList() function.
   */
  public final Boolean toIntegerList;
  
  /**
   * Whether to expect the toStringList() function.
   */
  public final Boolean toStringList;
  
  public ListFeatures (Boolean all, Boolean any, Boolean coalesce, Boolean isEmpty, Boolean head, Boolean last, Boolean listComprehension, Boolean listRange, Boolean none, Boolean reduce, Boolean reverse, Boolean single, Boolean size, Boolean tail, Boolean toBooleanList, Boolean toFloatList, Boolean toIntegerList, Boolean toStringList) {
    if (all == null) {
      throw new IllegalArgumentException("null value for 'all' argument");
    }
    if (any == null) {
      throw new IllegalArgumentException("null value for 'any' argument");
    }
    if (coalesce == null) {
      throw new IllegalArgumentException("null value for 'coalesce' argument");
    }
    if (isEmpty == null) {
      throw new IllegalArgumentException("null value for 'isEmpty' argument");
    }
    if (head == null) {
      throw new IllegalArgumentException("null value for 'head' argument");
    }
    if (last == null) {
      throw new IllegalArgumentException("null value for 'last' argument");
    }
    if (listComprehension == null) {
      throw new IllegalArgumentException("null value for 'listComprehension' argument");
    }
    if (listRange == null) {
      throw new IllegalArgumentException("null value for 'listRange' argument");
    }
    if (none == null) {
      throw new IllegalArgumentException("null value for 'none' argument");
    }
    if (reduce == null) {
      throw new IllegalArgumentException("null value for 'reduce' argument");
    }
    if (reverse == null) {
      throw new IllegalArgumentException("null value for 'reverse' argument");
    }
    if (single == null) {
      throw new IllegalArgumentException("null value for 'single' argument");
    }
    if (size == null) {
      throw new IllegalArgumentException("null value for 'size' argument");
    }
    if (tail == null) {
      throw new IllegalArgumentException("null value for 'tail' argument");
    }
    if (toBooleanList == null) {
      throw new IllegalArgumentException("null value for 'toBooleanList' argument");
    }
    if (toFloatList == null) {
      throw new IllegalArgumentException("null value for 'toFloatList' argument");
    }
    if (toIntegerList == null) {
      throw new IllegalArgumentException("null value for 'toIntegerList' argument");
    }
    if (toStringList == null) {
      throw new IllegalArgumentException("null value for 'toStringList' argument");
    }
    this.all = all;
    this.any = any;
    this.coalesce = coalesce;
    this.isEmpty = isEmpty;
    this.head = head;
    this.last = last;
    this.listComprehension = listComprehension;
    this.listRange = listRange;
    this.none = none;
    this.reduce = reduce;
    this.reverse = reverse;
    this.single = single;
    this.size = size;
    this.tail = tail;
    this.toBooleanList = toBooleanList;
    this.toFloatList = toFloatList;
    this.toIntegerList = toIntegerList;
    this.toStringList = toStringList;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ListFeatures)) {
      return false;
    }
    ListFeatures o = (ListFeatures) (other);
    return all.equals(o.all) && any.equals(o.any) && coalesce.equals(o.coalesce) && isEmpty.equals(o.isEmpty) && head.equals(o.head) && last.equals(o.last) && listComprehension.equals(o.listComprehension) && listRange.equals(o.listRange) && none.equals(o.none) && reduce.equals(o.reduce) && reverse.equals(o.reverse) && single.equals(o.single) && size.equals(o.size) && tail.equals(o.tail) && toBooleanList.equals(o.toBooleanList) && toFloatList.equals(o.toFloatList) && toIntegerList.equals(o.toIntegerList) && toStringList.equals(o.toStringList);
  }
  
  @Override
  public int hashCode() {
    return 2 * all.hashCode() + 3 * any.hashCode() + 5 * coalesce.hashCode() + 7 * isEmpty.hashCode() + 11 * head.hashCode() + 13 * last.hashCode() + 17 * listComprehension.hashCode() + 19 * listRange.hashCode() + 23 * none.hashCode() + 29 * reduce.hashCode() + 31 * reverse.hashCode() + 37 * single.hashCode() + 41 * size.hashCode() + 43 * tail.hashCode() + 47 * toBooleanList.hashCode() + 53 * toFloatList.hashCode() + 59 * toIntegerList.hashCode() + 61 * toStringList.hashCode();
  }
  
  public ListFeatures withAll(Boolean all) {
    if (all == null) {
      throw new IllegalArgumentException("null value for 'all' argument");
    }
    return new ListFeatures(all, any, coalesce, isEmpty, head, last, listComprehension, listRange, none, reduce, reverse, single, size, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFeatures withAny(Boolean any) {
    if (any == null) {
      throw new IllegalArgumentException("null value for 'any' argument");
    }
    return new ListFeatures(all, any, coalesce, isEmpty, head, last, listComprehension, listRange, none, reduce, reverse, single, size, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFeatures withCoalesce(Boolean coalesce) {
    if (coalesce == null) {
      throw new IllegalArgumentException("null value for 'coalesce' argument");
    }
    return new ListFeatures(all, any, coalesce, isEmpty, head, last, listComprehension, listRange, none, reduce, reverse, single, size, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFeatures withIsEmpty(Boolean isEmpty) {
    if (isEmpty == null) {
      throw new IllegalArgumentException("null value for 'isEmpty' argument");
    }
    return new ListFeatures(all, any, coalesce, isEmpty, head, last, listComprehension, listRange, none, reduce, reverse, single, size, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFeatures withHead(Boolean head) {
    if (head == null) {
      throw new IllegalArgumentException("null value for 'head' argument");
    }
    return new ListFeatures(all, any, coalesce, isEmpty, head, last, listComprehension, listRange, none, reduce, reverse, single, size, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFeatures withLast(Boolean last) {
    if (last == null) {
      throw new IllegalArgumentException("null value for 'last' argument");
    }
    return new ListFeatures(all, any, coalesce, isEmpty, head, last, listComprehension, listRange, none, reduce, reverse, single, size, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFeatures withListComprehension(Boolean listComprehension) {
    if (listComprehension == null) {
      throw new IllegalArgumentException("null value for 'listComprehension' argument");
    }
    return new ListFeatures(all, any, coalesce, isEmpty, head, last, listComprehension, listRange, none, reduce, reverse, single, size, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFeatures withListRange(Boolean listRange) {
    if (listRange == null) {
      throw new IllegalArgumentException("null value for 'listRange' argument");
    }
    return new ListFeatures(all, any, coalesce, isEmpty, head, last, listComprehension, listRange, none, reduce, reverse, single, size, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFeatures withNone(Boolean none) {
    if (none == null) {
      throw new IllegalArgumentException("null value for 'none' argument");
    }
    return new ListFeatures(all, any, coalesce, isEmpty, head, last, listComprehension, listRange, none, reduce, reverse, single, size, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFeatures withReduce(Boolean reduce) {
    if (reduce == null) {
      throw new IllegalArgumentException("null value for 'reduce' argument");
    }
    return new ListFeatures(all, any, coalesce, isEmpty, head, last, listComprehension, listRange, none, reduce, reverse, single, size, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFeatures withReverse(Boolean reverse) {
    if (reverse == null) {
      throw new IllegalArgumentException("null value for 'reverse' argument");
    }
    return new ListFeatures(all, any, coalesce, isEmpty, head, last, listComprehension, listRange, none, reduce, reverse, single, size, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFeatures withSingle(Boolean single) {
    if (single == null) {
      throw new IllegalArgumentException("null value for 'single' argument");
    }
    return new ListFeatures(all, any, coalesce, isEmpty, head, last, listComprehension, listRange, none, reduce, reverse, single, size, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFeatures withSize(Boolean size) {
    if (size == null) {
      throw new IllegalArgumentException("null value for 'size' argument");
    }
    return new ListFeatures(all, any, coalesce, isEmpty, head, last, listComprehension, listRange, none, reduce, reverse, single, size, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFeatures withTail(Boolean tail) {
    if (tail == null) {
      throw new IllegalArgumentException("null value for 'tail' argument");
    }
    return new ListFeatures(all, any, coalesce, isEmpty, head, last, listComprehension, listRange, none, reduce, reverse, single, size, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFeatures withToBooleanList(Boolean toBooleanList) {
    if (toBooleanList == null) {
      throw new IllegalArgumentException("null value for 'toBooleanList' argument");
    }
    return new ListFeatures(all, any, coalesce, isEmpty, head, last, listComprehension, listRange, none, reduce, reverse, single, size, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFeatures withToFloatList(Boolean toFloatList) {
    if (toFloatList == null) {
      throw new IllegalArgumentException("null value for 'toFloatList' argument");
    }
    return new ListFeatures(all, any, coalesce, isEmpty, head, last, listComprehension, listRange, none, reduce, reverse, single, size, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFeatures withToIntegerList(Boolean toIntegerList) {
    if (toIntegerList == null) {
      throw new IllegalArgumentException("null value for 'toIntegerList' argument");
    }
    return new ListFeatures(all, any, coalesce, isEmpty, head, last, listComprehension, listRange, none, reduce, reverse, single, size, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
  
  public ListFeatures withToStringList(Boolean toStringList) {
    if (toStringList == null) {
      throw new IllegalArgumentException("null value for 'toStringList' argument");
    }
    return new ListFeatures(all, any, coalesce, isEmpty, head, last, listComprehension, listRange, none, reduce, reverse, single, size, tail, toBooleanList, toFloatList, toIntegerList, toStringList);
  }
}