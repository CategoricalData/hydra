// Note: this is an automatically generated file. Do not edit.

package hydra.context;

import java.io.Serializable;

/**
 * A particular domain object (such as an error) together with an execution context
 */
public class InContext<E> implements Serializable, Comparable<InContext<E>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.context.InContext");

  public static final hydra.core.Name OBJECT = new hydra.core.Name("object");

  public static final hydra.core.Name CONTEXT = new hydra.core.Name("context");

  /**
   * A domain object; typically an error
   */
  public final E object;

  /**
   * The execution context at the point of capture
   */
  public final hydra.context.Context context;

  public InContext (E object, hydra.context.Context context) {
    this.object = object;
    this.context = context;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InContext)) {
      return false;
    }
    InContext o = (InContext) other;
    return java.util.Objects.equals(
      this.object,
      o.object) && java.util.Objects.equals(
      this.context,
      o.context);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(object) + 3 * java.util.Objects.hashCode(context);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InContext other) {
    int cmp = 0;
    cmp = ((Comparable) object).compareTo(other.object);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) context).compareTo(other.context);
  }

  public InContext withObject(E object) {
    return new InContext(object, context);
  }

  public InContext withContext(hydra.context.Context context) {
    return new InContext(object, context);
  }
}
