// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class PrimaryNoArrayCreationExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression");
  
  public static final hydra.core.Name FIELD_NAME_LITERAL = new hydra.core.Name("literal");
  
  public static final hydra.core.Name FIELD_NAME_INTERPOLATED_STRING = new hydra.core.Name("interpolatedString");
  
  public static final hydra.core.Name FIELD_NAME_SIMPLE_NAME = new hydra.core.Name("simpleName");
  
  public static final hydra.core.Name FIELD_NAME_PARENTHESIZED = new hydra.core.Name("parenthesized");
  
  public static final hydra.core.Name FIELD_NAME_TUPLE = new hydra.core.Name("tuple");
  
  public static final hydra.core.Name FIELD_NAME_MEMBER_ACCESS = new hydra.core.Name("memberAccess");
  
  public static final hydra.core.Name FIELD_NAME_NULL_CONDITIONAL_MEMBER_ACCESS = new hydra.core.Name("nullConditionalMemberAccess");
  
  public static final hydra.core.Name FIELD_NAME_INVOCATION = new hydra.core.Name("invocation");
  
  public static final hydra.core.Name FIELD_NAME_ELEMENT_ACCESS = new hydra.core.Name("elementAccess");
  
  public static final hydra.core.Name FIELD_NAME_NULL_CONDITIONAL_ELEMENT_ACCESS = new hydra.core.Name("nullConditionalElementAccess");
  
  public static final hydra.core.Name FIELD_NAME_THIS_ACCESS = new hydra.core.Name("thisAccess");
  
  public static final hydra.core.Name FIELD_NAME_BASE_ACCESS = new hydra.core.Name("baseAccess");
  
  public static final hydra.core.Name FIELD_NAME_POST_INCREMENT = new hydra.core.Name("postIncrement");
  
  public static final hydra.core.Name FIELD_NAME_POST_DECREMENT = new hydra.core.Name("postDecrement");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT_CREATION = new hydra.core.Name("objectCreation");
  
  public static final hydra.core.Name FIELD_NAME_DELEGATE_CREATION = new hydra.core.Name("delegateCreation");
  
  public static final hydra.core.Name FIELD_NAME_ANONYMOUS_OBJECT_CREATION = new hydra.core.Name("anonymousObjectCreation");
  
  public static final hydra.core.Name FIELD_NAME_TYPEOF = new hydra.core.Name("typeof");
  
  public static final hydra.core.Name FIELD_NAME_SIZEOF = new hydra.core.Name("sizeof");
  
  public static final hydra.core.Name FIELD_NAME_CHECKED = new hydra.core.Name("checked");
  
  public static final hydra.core.Name FIELD_NAME_UNCHECKED = new hydra.core.Name("unchecked");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT_VALUE = new hydra.core.Name("defaultValue");
  
  public static final hydra.core.Name FIELD_NAME_NAMEOF = new hydra.core.Name("nameof");
  
  public static final hydra.core.Name FIELD_NAME_ANONYMOUS_METHOD = new hydra.core.Name("anonymousMethod");
  
  public static final hydra.core.Name FIELD_NAME_POINTER_MEMBER_ACCESS = new hydra.core.Name("pointerMemberAccess");
  
  public static final hydra.core.Name FIELD_NAME_POINTER_ELEMENT_ACCESS = new hydra.core.Name("pointerElementAccess");
  
  public static final hydra.core.Name FIELD_NAME_STACKALLOC = new hydra.core.Name("stackalloc");
  
  private PrimaryNoArrayCreationExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Literal instance) ;
    
    R visit(InterpolatedString instance) ;
    
    R visit(SimpleName instance) ;
    
    R visit(Parenthesized instance) ;
    
    R visit(Tuple instance) ;
    
    R visit(MemberAccess instance) ;
    
    R visit(NullConditionalMemberAccess instance) ;
    
    R visit(Invocation instance) ;
    
    R visit(ElementAccess instance) ;
    
    R visit(NullConditionalElementAccess instance) ;
    
    R visit(ThisAccess instance) ;
    
    R visit(BaseAccess instance) ;
    
    R visit(PostIncrement instance) ;
    
    R visit(PostDecrement instance) ;
    
    R visit(ObjectCreation instance) ;
    
    R visit(DelegateCreation instance) ;
    
    R visit(AnonymousObjectCreation instance) ;
    
    R visit(Typeof instance) ;
    
    R visit(Sizeof instance) ;
    
    R visit(Checked instance) ;
    
    R visit(Unchecked instance) ;
    
    R visit(DefaultValue instance) ;
    
    R visit(Nameof instance) ;
    
    R visit(AnonymousMethod instance) ;
    
    R visit(PointerMemberAccess instance) ;
    
    R visit(PointerElementAccess instance) ;
    
    R visit(Stackalloc instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PrimaryNoArrayCreationExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Literal instance) {
      return otherwise((instance));
    }
    
    default R visit(InterpolatedString instance) {
      return otherwise((instance));
    }
    
    default R visit(SimpleName instance) {
      return otherwise((instance));
    }
    
    default R visit(Parenthesized instance) {
      return otherwise((instance));
    }
    
    default R visit(Tuple instance) {
      return otherwise((instance));
    }
    
    default R visit(MemberAccess instance) {
      return otherwise((instance));
    }
    
    default R visit(NullConditionalMemberAccess instance) {
      return otherwise((instance));
    }
    
    default R visit(Invocation instance) {
      return otherwise((instance));
    }
    
    default R visit(ElementAccess instance) {
      return otherwise((instance));
    }
    
    default R visit(NullConditionalElementAccess instance) {
      return otherwise((instance));
    }
    
    default R visit(ThisAccess instance) {
      return otherwise((instance));
    }
    
    default R visit(BaseAccess instance) {
      return otherwise((instance));
    }
    
    default R visit(PostIncrement instance) {
      return otherwise((instance));
    }
    
    default R visit(PostDecrement instance) {
      return otherwise((instance));
    }
    
    default R visit(ObjectCreation instance) {
      return otherwise((instance));
    }
    
    default R visit(DelegateCreation instance) {
      return otherwise((instance));
    }
    
    default R visit(AnonymousObjectCreation instance) {
      return otherwise((instance));
    }
    
    default R visit(Typeof instance) {
      return otherwise((instance));
    }
    
    default R visit(Sizeof instance) {
      return otherwise((instance));
    }
    
    default R visit(Checked instance) {
      return otherwise((instance));
    }
    
    default R visit(Unchecked instance) {
      return otherwise((instance));
    }
    
    default R visit(DefaultValue instance) {
      return otherwise((instance));
    }
    
    default R visit(Nameof instance) {
      return otherwise((instance));
    }
    
    default R visit(AnonymousMethod instance) {
      return otherwise((instance));
    }
    
    default R visit(PointerMemberAccess instance) {
      return otherwise((instance));
    }
    
    default R visit(PointerElementAccess instance) {
      return otherwise((instance));
    }
    
    default R visit(Stackalloc instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Literal extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.Literal value;
    
    public Literal (hydra.ext.csharp.syntax.Literal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) (other);
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
  
  public static final class InterpolatedString extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.InterpolatedStringExpression value;
    
    public InterpolatedString (hydra.ext.csharp.syntax.InterpolatedStringExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InterpolatedString)) {
        return false;
      }
      InterpolatedString o = (InterpolatedString) (other);
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
  
  public static final class SimpleName extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.SimpleName value;
    
    public SimpleName (hydra.ext.csharp.syntax.SimpleName value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SimpleName)) {
        return false;
      }
      SimpleName o = (SimpleName) (other);
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
  
  public static final class Parenthesized extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.Expression value;
    
    public Parenthesized (hydra.ext.csharp.syntax.Expression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parenthesized)) {
        return false;
      }
      Parenthesized o = (Parenthesized) (other);
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
  
  public static final class Tuple extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.TupleExpression value;
    
    public Tuple (hydra.ext.csharp.syntax.TupleExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tuple)) {
        return false;
      }
      Tuple o = (Tuple) (other);
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
  
  public static final class MemberAccess extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.MemberAccess value;
    
    public MemberAccess (hydra.ext.csharp.syntax.MemberAccess value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MemberAccess)) {
        return false;
      }
      MemberAccess o = (MemberAccess) (other);
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
  
  public static final class NullConditionalMemberAccess extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.NullConditionalMemberAccess value;
    
    public NullConditionalMemberAccess (hydra.ext.csharp.syntax.NullConditionalMemberAccess value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NullConditionalMemberAccess)) {
        return false;
      }
      NullConditionalMemberAccess o = (NullConditionalMemberAccess) (other);
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
  
  public static final class Invocation extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.InvocationExpression value;
    
    public Invocation (hydra.ext.csharp.syntax.InvocationExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Invocation)) {
        return false;
      }
      Invocation o = (Invocation) (other);
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
  
  public static final class ElementAccess extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.ElementAccess value;
    
    public ElementAccess (hydra.ext.csharp.syntax.ElementAccess value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ElementAccess)) {
        return false;
      }
      ElementAccess o = (ElementAccess) (other);
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
  
  public static final class NullConditionalElementAccess extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.NullConditionalElementAccess value;
    
    public NullConditionalElementAccess (hydra.ext.csharp.syntax.NullConditionalElementAccess value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NullConditionalElementAccess)) {
        return false;
      }
      NullConditionalElementAccess o = (NullConditionalElementAccess) (other);
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
  
  public static final class ThisAccess extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public ThisAccess () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ThisAccess)) {
        return false;
      }
      ThisAccess o = (ThisAccess) (other);
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
  
  public static final class BaseAccess extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.BaseAccess value;
    
    public BaseAccess (hydra.ext.csharp.syntax.BaseAccess value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BaseAccess)) {
        return false;
      }
      BaseAccess o = (BaseAccess) (other);
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
  
  public static final class PostIncrement extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.PrimaryExpression value;
    
    public PostIncrement (hydra.ext.csharp.syntax.PrimaryExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PostIncrement)) {
        return false;
      }
      PostIncrement o = (PostIncrement) (other);
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
  
  public static final class PostDecrement extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.PrimaryExpression value;
    
    public PostDecrement (hydra.ext.csharp.syntax.PrimaryExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PostDecrement)) {
        return false;
      }
      PostDecrement o = (PostDecrement) (other);
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
  
  public static final class ObjectCreation extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.ObjectCreationExpression value;
    
    public ObjectCreation (hydra.ext.csharp.syntax.ObjectCreationExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectCreation)) {
        return false;
      }
      ObjectCreation o = (ObjectCreation) (other);
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
  
  public static final class DelegateCreation extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.DelegateCreationExpression value;
    
    public DelegateCreation (hydra.ext.csharp.syntax.DelegateCreationExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DelegateCreation)) {
        return false;
      }
      DelegateCreation o = (DelegateCreation) (other);
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
  
  public static final class AnonymousObjectCreation extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.util.Opt<hydra.ext.csharp.syntax.MemberDeclaratorList> value;
    
    public AnonymousObjectCreation (hydra.util.Opt<hydra.ext.csharp.syntax.MemberDeclaratorList> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnonymousObjectCreation)) {
        return false;
      }
      AnonymousObjectCreation o = (AnonymousObjectCreation) (other);
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
  
  public static final class Typeof extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.TypeofExpression value;
    
    public Typeof (hydra.ext.csharp.syntax.TypeofExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Typeof)) {
        return false;
      }
      Typeof o = (Typeof) (other);
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
  
  public static final class Sizeof extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.UnmanagedType value;
    
    public Sizeof (hydra.ext.csharp.syntax.UnmanagedType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sizeof)) {
        return false;
      }
      Sizeof o = (Sizeof) (other);
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
  
  public static final class Checked extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.Expression value;
    
    public Checked (hydra.ext.csharp.syntax.Expression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Checked)) {
        return false;
      }
      Checked o = (Checked) (other);
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
  
  public static final class Unchecked extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.Expression value;
    
    public Unchecked (hydra.ext.csharp.syntax.Expression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unchecked)) {
        return false;
      }
      Unchecked o = (Unchecked) (other);
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
  
  public static final class DefaultValue extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.DefaultValueExpression value;
    
    public DefaultValue (hydra.ext.csharp.syntax.DefaultValueExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DefaultValue)) {
        return false;
      }
      DefaultValue o = (DefaultValue) (other);
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
  
  public static final class Nameof extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.NamedEntity value;
    
    public Nameof (hydra.ext.csharp.syntax.NamedEntity value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nameof)) {
        return false;
      }
      Nameof o = (Nameof) (other);
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
  
  public static final class AnonymousMethod extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.AnonymousMethodExpression value;
    
    public AnonymousMethod (hydra.ext.csharp.syntax.AnonymousMethodExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnonymousMethod)) {
        return false;
      }
      AnonymousMethod o = (AnonymousMethod) (other);
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
  
  public static final class PointerMemberAccess extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.PointerMemberAccess value;
    
    public PointerMemberAccess (hydra.ext.csharp.syntax.PointerMemberAccess value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PointerMemberAccess)) {
        return false;
      }
      PointerMemberAccess o = (PointerMemberAccess) (other);
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
  
  public static final class PointerElementAccess extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.PointerElementAccess value;
    
    public PointerElementAccess (hydra.ext.csharp.syntax.PointerElementAccess value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PointerElementAccess)) {
        return false;
      }
      PointerElementAccess o = (PointerElementAccess) (other);
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
  
  public static final class Stackalloc extends hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression implements Serializable {
    public final hydra.ext.csharp.syntax.StackallocExpression value;
    
    public Stackalloc (hydra.ext.csharp.syntax.StackallocExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Stackalloc)) {
        return false;
      }
      Stackalloc o = (Stackalloc) (other);
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