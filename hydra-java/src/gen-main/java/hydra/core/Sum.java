// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * The unlabeled equivalent of an Injection term
 */
public class Sum<A> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Sum");
  
  public final Integer index;
  
  public final Integer size;
  
  public final hydra.core.Term<A> term;
  
  public Sum (Integer index, Integer size, hydra.core.Term<A> term) {
    if (index == null) {
      throw new IllegalArgumentException("null value for 'index' argument");
    }
    if (size == null) {
      throw new IllegalArgumentException("null value for 'size' argument");
    }
    if (term == null) {
      throw new IllegalArgumentException("null value for 'term' argument");
    }
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
    if (index == null) {
      throw new IllegalArgumentException("null value for 'index' argument");
    }
    return new Sum(index, size, term);
  }
  
  public Sum withSize(Integer size) {
    if (size == null) {
      throw new IllegalArgumentException("null value for 'size' argument");
    }
    return new Sum(index, size, term);
  }
  
  public Sum withTerm(hydra.core.Term<A> term) {
    if (term == null) {
      throw new IllegalArgumentException("null value for 'term' argument");
    }
    return new Sum(index, size, term);
  }
}