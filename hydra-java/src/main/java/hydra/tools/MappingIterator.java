package hydra.tools;

import java.util.Iterator;
import java.util.function.Function;


public class MappingIterator<A, B> implements Iterator<B> {
  private final Iterator<A> inner;
  private final Function<A, B> mapping;

  public MappingIterator(Iterator<A> inner, Function<A, B> mapping) {
    this.inner = inner;
    this.mapping = mapping;
  }

  @Override
  public boolean hasNext() {
    return inner.hasNext();
  }

  @Override
  public B next() {
    return mapping.apply(inner.next());
  }
}

