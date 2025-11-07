// Note: this is an automatically generated file. Do not edit.

package hydra.accessors;

import java.io.Serializable;

/**
 * A function which maps from a term to a particular immediate subterm
 */
public abstract class TermAccessor implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.accessors.TermAccessor");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATED_BODY = new hydra.core.Name("annotatedBody");
  
  public static final hydra.core.Name FIELD_NAME_APPLICATION_FUNCTION = new hydra.core.Name("applicationFunction");
  
  public static final hydra.core.Name FIELD_NAME_APPLICATION_ARGUMENT = new hydra.core.Name("applicationArgument");
  
  public static final hydra.core.Name FIELD_NAME_LAMBDA_BODY = new hydra.core.Name("lambdaBody");
  
  public static final hydra.core.Name FIELD_NAME_UNION_CASES_DEFAULT = new hydra.core.Name("unionCasesDefault");
  
  public static final hydra.core.Name FIELD_NAME_UNION_CASES_BRANCH = new hydra.core.Name("unionCasesBranch");
  
  public static final hydra.core.Name FIELD_NAME_LET_BODY = new hydra.core.Name("letBody");
  
  public static final hydra.core.Name FIELD_NAME_LET_BINDING = new hydra.core.Name("letBinding");
  
  public static final hydra.core.Name FIELD_NAME_LIST_ELEMENT = new hydra.core.Name("listElement");
  
  public static final hydra.core.Name FIELD_NAME_MAP_KEY = new hydra.core.Name("mapKey");
  
  public static final hydra.core.Name FIELD_NAME_MAP_VALUE = new hydra.core.Name("mapValue");
  
  public static final hydra.core.Name FIELD_NAME_MAYBE_TERM = new hydra.core.Name("maybeTerm");
  
  public static final hydra.core.Name FIELD_NAME_PRODUCT_TERM = new hydra.core.Name("productTerm");
  
  public static final hydra.core.Name FIELD_NAME_RECORD_FIELD = new hydra.core.Name("recordField");
  
  public static final hydra.core.Name FIELD_NAME_SET_ELEMENT = new hydra.core.Name("setElement");
  
  public static final hydra.core.Name FIELD_NAME_SUM_TERM = new hydra.core.Name("sumTerm");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_LAMBDA_BODY = new hydra.core.Name("typeLambdaBody");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_APPLICATION_TERM = new hydra.core.Name("typeApplicationTerm");
  
  public static final hydra.core.Name FIELD_NAME_INJECTION_TERM = new hydra.core.Name("injectionTerm");
  
  public static final hydra.core.Name FIELD_NAME_WRAPPED_TERM = new hydra.core.Name("wrappedTerm");
  
  private TermAccessor () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(AnnotatedBody instance) ;
    
    R visit(ApplicationFunction instance) ;
    
    R visit(ApplicationArgument instance) ;
    
    R visit(LambdaBody instance) ;
    
    R visit(UnionCasesDefault instance) ;
    
    R visit(UnionCasesBranch instance) ;
    
    R visit(LetBody instance) ;
    
    R visit(LetBinding instance) ;
    
    R visit(ListElement instance) ;
    
    R visit(MapKey instance) ;
    
    R visit(MapValue instance) ;
    
    R visit(MaybeTerm instance) ;
    
    R visit(ProductTerm instance) ;
    
    R visit(RecordField instance) ;
    
    R visit(SetElement instance) ;
    
    R visit(SumTerm instance) ;
    
    R visit(TypeLambdaBody instance) ;
    
    R visit(TypeApplicationTerm instance) ;
    
    R visit(InjectionTerm instance) ;
    
    R visit(WrappedTerm instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TermAccessor instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(AnnotatedBody instance) {
      return otherwise((instance));
    }
    
    default R visit(ApplicationFunction instance) {
      return otherwise((instance));
    }
    
    default R visit(ApplicationArgument instance) {
      return otherwise((instance));
    }
    
    default R visit(LambdaBody instance) {
      return otherwise((instance));
    }
    
    default R visit(UnionCasesDefault instance) {
      return otherwise((instance));
    }
    
    default R visit(UnionCasesBranch instance) {
      return otherwise((instance));
    }
    
    default R visit(LetBody instance) {
      return otherwise((instance));
    }
    
    default R visit(LetBinding instance) {
      return otherwise((instance));
    }
    
    default R visit(ListElement instance) {
      return otherwise((instance));
    }
    
    default R visit(MapKey instance) {
      return otherwise((instance));
    }
    
    default R visit(MapValue instance) {
      return otherwise((instance));
    }
    
    default R visit(MaybeTerm instance) {
      return otherwise((instance));
    }
    
    default R visit(ProductTerm instance) {
      return otherwise((instance));
    }
    
    default R visit(RecordField instance) {
      return otherwise((instance));
    }
    
    default R visit(SetElement instance) {
      return otherwise((instance));
    }
    
    default R visit(SumTerm instance) {
      return otherwise((instance));
    }
    
    default R visit(TypeLambdaBody instance) {
      return otherwise((instance));
    }
    
    default R visit(TypeApplicationTerm instance) {
      return otherwise((instance));
    }
    
    default R visit(InjectionTerm instance) {
      return otherwise((instance));
    }
    
    default R visit(WrappedTerm instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * Access the body of an annotated term
   */
  public static final class AnnotatedBody extends hydra.accessors.TermAccessor implements Serializable {
    public final Boolean value;
    
    public AnnotatedBody (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnnotatedBody)) {
        return false;
      }
      AnnotatedBody o = (AnnotatedBody) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Access the function of an application term
   */
  public static final class ApplicationFunction extends hydra.accessors.TermAccessor implements Serializable {
    public final Boolean value;
    
    public ApplicationFunction (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ApplicationFunction)) {
        return false;
      }
      ApplicationFunction o = (ApplicationFunction) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Access the argument of an application term
   */
  public static final class ApplicationArgument extends hydra.accessors.TermAccessor implements Serializable {
    public final Boolean value;
    
    public ApplicationArgument (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ApplicationArgument)) {
        return false;
      }
      ApplicationArgument o = (ApplicationArgument) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Access the body of a lambda term
   */
  public static final class LambdaBody extends hydra.accessors.TermAccessor implements Serializable {
    public final Boolean value;
    
    public LambdaBody (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LambdaBody)) {
        return false;
      }
      LambdaBody o = (LambdaBody) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Access the default case of a union elimination
   */
  public static final class UnionCasesDefault extends hydra.accessors.TermAccessor implements Serializable {
    public final Boolean value;
    
    public UnionCasesDefault (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnionCasesDefault)) {
        return false;
      }
      UnionCasesDefault o = (UnionCasesDefault) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Access a specific branch of a union elimination by field name
   */
  public static final class UnionCasesBranch extends hydra.accessors.TermAccessor implements Serializable {
    public final hydra.core.Name value;
    
    public UnionCasesBranch (hydra.core.Name value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnionCasesBranch)) {
        return false;
      }
      UnionCasesBranch o = (UnionCasesBranch) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Access the body of a let term
   */
  public static final class LetBody extends hydra.accessors.TermAccessor implements Serializable {
    public final Boolean value;
    
    public LetBody (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LetBody)) {
        return false;
      }
      LetBody o = (LetBody) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Access a specific binding in a let term by variable name
   */
  public static final class LetBinding extends hydra.accessors.TermAccessor implements Serializable {
    public final hydra.core.Name value;
    
    public LetBinding (hydra.core.Name value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LetBinding)) {
        return false;
      }
      LetBinding o = (LetBinding) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Access an element of a list by index
   */
  public static final class ListElement extends hydra.accessors.TermAccessor implements Serializable {
    public final Integer value;
    
    public ListElement (Integer value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ListElement)) {
        return false;
      }
      ListElement o = (ListElement) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Access a key in a map by index
   */
  public static final class MapKey extends hydra.accessors.TermAccessor implements Serializable {
    public final Integer value;
    
    public MapKey (Integer value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MapKey)) {
        return false;
      }
      MapKey o = (MapKey) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Access a value in a map by index
   */
  public static final class MapValue extends hydra.accessors.TermAccessor implements Serializable {
    public final Integer value;
    
    public MapValue (Integer value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MapValue)) {
        return false;
      }
      MapValue o = (MapValue) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Access the term inside a Just value
   */
  public static final class MaybeTerm extends hydra.accessors.TermAccessor implements Serializable {
    public final Boolean value;
    
    public MaybeTerm (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MaybeTerm)) {
        return false;
      }
      MaybeTerm o = (MaybeTerm) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Access an element of a product (tuple) by index
   */
  public static final class ProductTerm extends hydra.accessors.TermAccessor implements Serializable {
    public final Integer value;
    
    public ProductTerm (Integer value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ProductTerm)) {
        return false;
      }
      ProductTerm o = (ProductTerm) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Access a field of a record by field name
   */
  public static final class RecordField extends hydra.accessors.TermAccessor implements Serializable {
    public final hydra.core.Name value;
    
    public RecordField (hydra.core.Name value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RecordField)) {
        return false;
      }
      RecordField o = (RecordField) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Access an element of a set by index
   */
  public static final class SetElement extends hydra.accessors.TermAccessor implements Serializable {
    public final Integer value;
    
    public SetElement (Integer value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SetElement)) {
        return false;
      }
      SetElement o = (SetElement) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Access the term inside a sum variant
   */
  public static final class SumTerm extends hydra.accessors.TermAccessor implements Serializable {
    public final Boolean value;
    
    public SumTerm (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SumTerm)) {
        return false;
      }
      SumTerm o = (SumTerm) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Access the body of a type lambda term
   */
  public static final class TypeLambdaBody extends hydra.accessors.TermAccessor implements Serializable {
    public final Boolean value;
    
    public TypeLambdaBody (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeLambdaBody)) {
        return false;
      }
      TypeLambdaBody o = (TypeLambdaBody) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Access the term being applied to a type
   */
  public static final class TypeApplicationTerm extends hydra.accessors.TermAccessor implements Serializable {
    public final Boolean value;
    
    public TypeApplicationTerm (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeApplicationTerm)) {
        return false;
      }
      TypeApplicationTerm o = (TypeApplicationTerm) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Access the term inside a union injection
   */
  public static final class InjectionTerm extends hydra.accessors.TermAccessor implements Serializable {
    public final Boolean value;
    
    public InjectionTerm (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InjectionTerm)) {
        return false;
      }
      InjectionTerm o = (InjectionTerm) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Access the term inside a wrapped term
   */
  public static final class WrappedTerm extends hydra.accessors.TermAccessor implements Serializable {
    public final Boolean value;
    
    public WrappedTerm (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WrappedTerm)) {
        return false;
      }
      WrappedTerm o = (WrappedTerm) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
