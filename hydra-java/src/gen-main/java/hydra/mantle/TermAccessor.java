// Note: this is an automatically generated file. Do not edit.

package hydra.mantle;

import java.io.Serializable;

/**
 * A function which maps from a term to a particular immediate subterm
 */
public abstract class TermAccessor implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.mantle.TermAccessor");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATED_SUBJECT = new hydra.core.Name("annotatedSubject");
  
  public static final hydra.core.Name FIELD_NAME_APPLICATION_FUNCTION = new hydra.core.Name("applicationFunction");
  
  public static final hydra.core.Name FIELD_NAME_APPLICATION_ARGUMENT = new hydra.core.Name("applicationArgument");
  
  public static final hydra.core.Name FIELD_NAME_LAMBDA_BODY = new hydra.core.Name("lambdaBody");
  
  public static final hydra.core.Name FIELD_NAME_LIST_FOLD = new hydra.core.Name("listFold");
  
  public static final hydra.core.Name FIELD_NAME_OPTIONAL_CASES_NOTHING = new hydra.core.Name("optionalCasesNothing");
  
  public static final hydra.core.Name FIELD_NAME_OPTIONAL_CASES_JUST = new hydra.core.Name("optionalCasesJust");
  
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
  
  public static final hydra.core.Name FIELD_NAME_TYPED_TERM = new hydra.core.Name("typedTerm");
  
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
    
    R visit(ListFold instance) ;
    
    R visit(OptionalCasesNothing instance) ;
    
    R visit(OptionalCasesJust instance) ;
    
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
    
    R visit(TypedTerm instance) ;
    
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
    
    default R visit(ListFold instance) {
      return otherwise((instance));
    }
    
    default R visit(OptionalCasesNothing instance) {
      return otherwise((instance));
    }
    
    default R visit(OptionalCasesJust instance) {
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
    
    default R visit(TypedTerm instance) {
      return otherwise((instance));
    }
    
    default R visit(InjectionTerm instance) {
      return otherwise((instance));
    }
    
    default R visit(WrappedTerm instance) {
      return otherwise((instance));
    }
  }
  
  public static final class AnnotatedSubject extends hydra.mantle.TermAccessor implements Serializable {
    public AnnotatedSubject () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnnotatedSubject)) {
        return false;
      }
      AnnotatedSubject o = (AnnotatedSubject) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ApplicationFunction extends hydra.mantle.TermAccessor implements Serializable {
    public ApplicationFunction () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ApplicationFunction)) {
        return false;
      }
      ApplicationFunction o = (ApplicationFunction) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ApplicationArgument extends hydra.mantle.TermAccessor implements Serializable {
    public ApplicationArgument () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ApplicationArgument)) {
        return false;
      }
      ApplicationArgument o = (ApplicationArgument) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class LambdaBody extends hydra.mantle.TermAccessor implements Serializable {
    public LambdaBody () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LambdaBody)) {
        return false;
      }
      LambdaBody o = (LambdaBody) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ListFold extends hydra.mantle.TermAccessor implements Serializable {
    public ListFold () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ListFold)) {
        return false;
      }
      ListFold o = (ListFold) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class OptionalCasesNothing extends hydra.mantle.TermAccessor implements Serializable {
    public OptionalCasesNothing () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OptionalCasesNothing)) {
        return false;
      }
      OptionalCasesNothing o = (OptionalCasesNothing) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class OptionalCasesJust extends hydra.mantle.TermAccessor implements Serializable {
    public OptionalCasesJust () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OptionalCasesJust)) {
        return false;
      }
      OptionalCasesJust o = (OptionalCasesJust) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class UnionCasesDefault extends hydra.mantle.TermAccessor implements Serializable {
    public UnionCasesDefault () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnionCasesDefault)) {
        return false;
      }
      UnionCasesDefault o = (UnionCasesDefault) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class UnionCasesBranch extends hydra.mantle.TermAccessor implements Serializable {
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
  
  public static final class LetEnvironment extends hydra.mantle.TermAccessor implements Serializable {
    public LetEnvironment () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LetEnvironment)) {
        return false;
      }
      LetEnvironment o = (LetEnvironment) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class LetBinding extends hydra.mantle.TermAccessor implements Serializable {
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
  
  public static final class ListElement extends hydra.mantle.TermAccessor implements Serializable {
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
  
  public static final class MapKey extends hydra.mantle.TermAccessor implements Serializable {
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
  
  public static final class MapValue extends hydra.mantle.TermAccessor implements Serializable {
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
  
  public static final class OptionalTerm extends hydra.mantle.TermAccessor implements Serializable {
    public OptionalTerm () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OptionalTerm)) {
        return false;
      }
      OptionalTerm o = (OptionalTerm) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ProductTerm extends hydra.mantle.TermAccessor implements Serializable {
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
  
  public static final class RecordField extends hydra.mantle.TermAccessor implements Serializable {
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
  
  public static final class SetElement extends hydra.mantle.TermAccessor implements Serializable {
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
  
  public static final class SumTerm extends hydra.mantle.TermAccessor implements Serializable {
    public SumTerm () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SumTerm)) {
        return false;
      }
      SumTerm o = (SumTerm) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class TypeAbstractionBody extends hydra.mantle.TermAccessor implements Serializable {
    public TypeAbstractionBody () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeAbstractionBody)) {
        return false;
      }
      TypeAbstractionBody o = (TypeAbstractionBody) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class TypeApplicationTerm extends hydra.mantle.TermAccessor implements Serializable {
    public TypeApplicationTerm () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeApplicationTerm)) {
        return false;
      }
      TypeApplicationTerm o = (TypeApplicationTerm) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class TypedTerm extends hydra.mantle.TermAccessor implements Serializable {
    public TypedTerm () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypedTerm)) {
        return false;
      }
      TypedTerm o = (TypedTerm) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class InjectionTerm extends hydra.mantle.TermAccessor implements Serializable {
    public InjectionTerm () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InjectionTerm)) {
        return false;
      }
      InjectionTerm o = (InjectionTerm) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class WrappedTerm extends hydra.mantle.TermAccessor implements Serializable {
    public WrappedTerm () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WrappedTerm)) {
        return false;
      }
      WrappedTerm o = (WrappedTerm) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}