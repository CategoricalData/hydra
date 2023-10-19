package hydra.langs.tinkerpop.errors;

public class Validator<T, V> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/errors.Validator");
  
  public final java.util.function.Function<T, String> showType;
  
  public final java.util.function.Function<V, String> showValue;
  
  public final java.util.function.Function<T, java.util.function.Function<V, java.util.Optional<hydra.langs.tinkerpop.errors.TypeError<T, V>>>> checkValue;
  
  public Validator (java.util.function.Function<T, String> showType, java.util.function.Function<V, String> showValue, java.util.function.Function<T, java.util.function.Function<V, java.util.Optional<hydra.langs.tinkerpop.errors.TypeError<T, V>>>> checkValue) {
    this.showType = showType;
    this.showValue = showValue;
    this.checkValue = checkValue;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Validator)) {
      return false;
    }
    Validator o = (Validator) (other);
    return showType.equals(o.showType) && showValue.equals(o.showValue) && checkValue.equals(o.checkValue);
  }
  
  @Override
  public int hashCode() {
    return 2 * showType.hashCode() + 3 * showValue.hashCode() + 5 * checkValue.hashCode();
  }
  
  public Validator withShowType(java.util.function.Function<T, String> showType) {
    return new Validator(showType, showValue, checkValue);
  }
  
  public Validator withShowValue(java.util.function.Function<V, String> showValue) {
    return new Validator(showType, showValue, checkValue);
  }
  
  public Validator withCheckValue(java.util.function.Function<T, java.util.function.Function<V, java.util.Optional<hydra.langs.tinkerpop.errors.TypeError<T, V>>>> checkValue) {
    return new Validator(showType, showValue, checkValue);
  }
}