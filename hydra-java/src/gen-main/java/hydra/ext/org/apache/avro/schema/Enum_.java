// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.avro.schema;

import java.io.Serializable;

public class Enum_ implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.avro.schema.Enum");
  
  public static final hydra.core.Name FIELD_NAME_SYMBOLS = new hydra.core.Name("symbols");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT = new hydra.core.Name("default");
  
  /**
   * a JSON array, listing symbols, as JSON strings. All symbols in an enum must be unique; duplicates are prohibited. Every symbol must match the regular expression [A-Za-z_][A-Za-z0-9_]* (the same requirement as for names)
   */
  public final java.util.List<String> symbols;
  
  /**
   * A default value for this enumeration, used during resolution when the reader encounters a symbol from the writer that isn't defined in the reader's schema. The value provided here must be a JSON string that's a member of the symbols array
   */
  public final hydra.util.Opt<String> default_;
  
  public Enum_ (java.util.List<String> symbols, hydra.util.Opt<String> default_) {
    java.util.Objects.requireNonNull((symbols));
    java.util.Objects.requireNonNull((default_));
    this.symbols = symbols;
    this.default_ = default_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Enum_)) {
      return false;
    }
    Enum_ o = (Enum_) (other);
    return symbols.equals(o.symbols) && default_.equals(o.default_);
  }
  
  @Override
  public int hashCode() {
    return 2 * symbols.hashCode() + 3 * default_.hashCode();
  }
  
  public Enum_ withSymbols(java.util.List<String> symbols) {
    java.util.Objects.requireNonNull((symbols));
    return new Enum_(symbols, default_);
  }
  
  public Enum_ withDefault(hydra.util.Opt<String> default_) {
    java.util.Objects.requireNonNull((default_));
    return new Enum_(symbols, default_);
  }
}