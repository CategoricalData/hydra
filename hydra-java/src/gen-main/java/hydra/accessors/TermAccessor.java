// Note: this is an automatically generated file. Do not edit.

package hydra.accessors;

import java.io.Serializable;

/**
 * A function which maps from a term to a particular immediate subterm
 */
public abstract class TermAccessor implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.accessors.TermAccessor");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATED_SUBJECT = new hydra.core.Name("annotatedSubject");
  
  public static final hydra.core.Name FIELD_NAME_APPLICATION_FUNCTION = new hydra.core.Name("applicationFunction");
  
  public static final hydra.core.Name FIELD_NAME_APPLICATION_ARGUMENT = new hydra.core.Name("applicationArgument");
  
  public static final hydra.core.Name FIELD_NAME_LAMBDA_BODY = new hydra.core.Name("lambdaBody");
  
  public static final hydra.core.Name FIELD_NAME_UNION_CASES_DEFAULT = new hydra.core.Name("unionCasesDefault");
  
  public static final hydra.core.Name FIELD_NAME_UNION_CASES_BRANCH = new hydra.core.Name("unionCasesBranch");
  
  public static final hydra.core.Name FIELD_NAME_LET_ENVIRONMENT = new hydra.core.Name("letEnvironment");
  
  public static final hydra.core.Name FIELD_NAME_LET_BINDING = new hydra.core.Name("letBinding");
  
  public static final hydra.core.Name FIELD_NAME_LIST_ELEMENT = new hydra.core.Name("listElement");
  
  public static final hydra.core.Name FIELD_NAME_MAP_KEY = new hydra.core.Name("mapKey");
  
  public static final hydra.core.Name FIELD_NAME_MAP_VALUE = new hydra.core.Name("mapValue");
  
  public static final hydra.core.Name FIELD_NAME_OPTIONAL_TERM = new hydra.core.Name("optionalTerm");
  
  public static final hydra.core.Name FIELD_NAME_PRODUCT_TERM = new hydra.core.Name("productTerm");
  
  public static final hydra.core.Name FIELD_NAME_RECORD_FIELD = new hydra.core.Name("recordField");
  
  public static final hydra.core.Name FIELD_NAME_SET_ELEMENT = new hydra.core.Name("setElement");
  
  public static final hydra.core.Name FIELD_NAME_SUM_TERM = new hydra.core.Name("sumTerm");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ABSTRACTION_BODY = new hydra.core.Name("typeAbstractionBody");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_APPLICATION_TERM = new hydra.core.Name("typeApplicationTerm");
  
  public static final hydra.core.Name FIELD_NAME_INJECTION_TERM = new hydra.core.Name("injectionTerm");
  
  public static final hydra.core.Name FIELD_NAME_WRAPPED_TERM = new hydra.core.Name("wrappedTerm");
  
  private TermAccessor () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(AnnotatedSubject instance) ;
    
    R visit(ApplicationFunction instance) ;
    
    R visit(ApplicationArgument instance) ;
    
    R visit(LambdaBody instance) ;
    
    R visit(UnionCasesDefault instance) ;
    
    R visit(UnionCasesBranch instance) ;
    
    R visit(LetEnvironment instance) ;
    
    R visit(LetBinding instance) ;
    
    R visit(ListElement instance) ;
    
    R visit(MapKey instance) ;
    
    R visit(MapValue instance) ;
    
    R visit(OptionalTerm instance) ;
    
    R visit(ProductTerm instance) ;
    
    R visit(RecordField instance) ;
    
    R visit(SetElement instance) ;
    
    R visit(SumTerm instance) ;
    
    R visit(TypeAbstractionBody instance) ;
    
    R visit(TypeApplicationTerm instance) ;
    
    R visit(InjectionTerm instance) ;
    
    R visit(WrappedTerm instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TermAccessor instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(AnnotatedSubject instance) {
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
    
    default R visit(LetEnvironment instance) {
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
    
    default R visit(OptionalTerm instance) {
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
    
    default R visit(TypeAbstractionBody instance) {
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
  
  public static final class AnnotatedSubject extends hydra.accessors.TermAccessor implements Serializable {
    public final java.lang.Void value;
    
    public AnnotatedSubject (java.lang.Void value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnnotatedSubject)) {
        return false;
      }
      AnnotatedSubject o = (AnnotatedSubject) (other);
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
  
  public static final class ApplicationFunction extends hydra.accessors.TermAccessor implements Serializable {
    public final java.lang.Void value;
    
    public ApplicationFunction (java.lang.Void value) {
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
  
  public static final class ApplicationArgument extends hydra.accessors.TermAccessor implements Serializable {
    public final java.lang.Void value;
    
    public ApplicationArgument (java.lang.Void value) {
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
  
  public static final class LambdaBody extends hydra.accessors.TermAccessor implements Serializable {
    public final java.lang.Void value;
    
    public LambdaBody (java.lang.Void value) {
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
  
  public static final class UnionCasesDefault extends hydra.accessors.TermAccessor implements Serializable {
    public final java.lang.Void value;
    
    public UnionCasesDefault (java.lang.Void value) {
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
   * A unique identifier in some context; a string-valued key
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
  
  public static final class LetEnvironment extends hydra.accessors.TermAccessor implements Serializable {
    public final java.lang.Void value;
    
    public LetEnvironment (java.lang.Void value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LetEnvironment)) {
        return false;
      }
      LetEnvironment o = (LetEnvironment) (other);
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
   * A unique identifier in some context; a string-valued key
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
  
  public static final class OptionalTerm extends hydra.accessors.TermAccessor implements Serializable {
    public final java.lang.Void value;
    
    public OptionalTerm (java.lang.Void value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OptionalTerm)) {
        return false;
      }
      OptionalTerm o = (OptionalTerm) (other);
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
   * A unique identifier in some context; a string-valued key
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
  
  public static final class SumTerm extends hydra.accessors.TermAccessor implements Serializable {
    public final java.lang.Void value;
    
    public SumTerm (java.lang.Void value) {
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
  
  public static final class TypeAbstractionBody extends hydra.accessors.TermAccessor implements Serializable {
    public final java.lang.Void value;
    
    public TypeAbstractionBody (java.lang.Void value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeAbstractionBody)) {
        return false;
      }
      TypeAbstractionBody o = (TypeAbstractionBody) (other);
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
  
  public static final class TypeApplicationTerm extends hydra.accessors.TermAccessor implements Serializable {
    public final java.lang.Void value;
    
    public TypeApplicationTerm (java.lang.Void value) {
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
  
  public static final class InjectionTerm extends hydra.accessors.TermAccessor implements Serializable {
    public final java.lang.Void value;
    
    public InjectionTerm (java.lang.Void value) {
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
  
  public static final class WrappedTerm extends hydra.accessors.TermAccessor implements Serializable {
    public final java.lang.Void value;
    
    public WrappedTerm (java.lang.Void value) {
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
