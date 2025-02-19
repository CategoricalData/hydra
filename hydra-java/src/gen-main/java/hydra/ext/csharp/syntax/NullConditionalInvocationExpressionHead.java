// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class NullConditionalInvocationExpressionHead implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.NullConditionalInvocationExpressionHead");
  
  public static final hydra.core.Name FIELD_NAME_MEMBER = new hydra.core.Name("member");
  
  public static final hydra.core.Name FIELD_NAME_ELEMENT = new hydra.core.Name("element");
  
  private NullConditionalInvocationExpressionHead () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Member instance) ;
    
    R visit(Element instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NullConditionalInvocationExpressionHead instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Member instance) {
      return otherwise((instance));
    }
    
    default R visit(Element instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Member extends hydra.ext.csharp.syntax.NullConditionalInvocationExpressionHead implements Serializable {
    public final hydra.ext.csharp.syntax.NullConditionalMemberAccess value;
    
    public Member (hydra.ext.csharp.syntax.NullConditionalMemberAccess value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Member)) {
        return false;
      }
      Member o = (Member) (other);
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
  
  public static final class Element extends hydra.ext.csharp.syntax.NullConditionalInvocationExpressionHead implements Serializable {
    public final hydra.ext.csharp.syntax.NullConditionalElementAccess value;
    
    public Element (hydra.ext.csharp.syntax.NullConditionalElementAccess value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Element)) {
        return false;
      }
      Element o = (Element) (other);
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