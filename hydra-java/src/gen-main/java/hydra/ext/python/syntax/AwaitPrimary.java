// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class AwaitPrimary implements Serializable, Comparable<AwaitPrimary> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.AwaitPrimary");
  
  public static final hydra.core.Name FIELD_NAME_AWAIT = new hydra.core.Name("await");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY = new hydra.core.Name("primary");
  
  public final Boolean await;
  
  public final hydra.ext.python.syntax.Primary primary;
  
  public AwaitPrimary (Boolean await, hydra.ext.python.syntax.Primary primary) {
    this.await = await;
    this.primary = primary;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AwaitPrimary)) {
      return false;
    }
    AwaitPrimary o = (AwaitPrimary) other;
    return java.util.Objects.equals(
      this.await,
      o.await) && java.util.Objects.equals(
      this.primary,
      o.primary);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(await) + 3 * java.util.Objects.hashCode(primary);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AwaitPrimary other) {
    int cmp = 0;
    cmp = ((Comparable) await).compareTo(other.await);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) primary).compareTo(other.primary);
  }
  
  public AwaitPrimary withAwait(Boolean await) {
    return new AwaitPrimary(await, primary);
  }
  
  public AwaitPrimary withPrimary(hydra.ext.python.syntax.Primary primary) {
    return new AwaitPrimary(await, primary);
  }
}
