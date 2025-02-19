// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * The unlabeled equivalent of an Injection term
 */
public class Sum implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.Sum");
  
  public static final hydra.core.Name FIELD_NAME_INDEX = new hydra.core.Name("index");
  
  public static final hydra.core.Name FIELD_NAME_SIZE = new hydra.core.Name("size");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public final Integer index;
  
  public final Integer size;
  
  public final hydra.core.Term term;
  
  public Sum (Integer index, Integer size, hydra.core.Term term) {
    java.util.Objects.requireNonNull((index));
    java.util.Objects.requireNonNull((size));
    java.util.Objects.requireNonNull((term));
    this.index = index;
    this.size = size;
    this.term = term;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Sum)) {
      return false;
    }
    Sum o = (Sum) (other);
    return index.equals(o.index) && size.equals(o.size) && term.equals(o.term);
  }
  
  @Override
  public int hashCode() {
    return 2 * index.hashCode() + 3 * size.hashCode() + 5 * term.hashCode();
  }
  
  public Sum withIndex(Integer index) {
    java.util.Objects.requireNonNull((index));
    return new Sum(index, size, term);
  }
  
  public Sum withSize(Integer size) {
    java.util.Objects.requireNonNull((size));
    return new Sum(index, size, term);
  }
  
  public Sum withTerm(hydra.core.Term term) {
    java.util.Objects.requireNonNull((term));
    return new Sum(index, size, term);
  }
}