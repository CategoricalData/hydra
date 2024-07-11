// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public abstract class Expression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.Expression");
  
  private Expression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(And instance) ;
    
    R visit(Any instance) ;
    
    R visit(Between instance) ;
    
    R visit(Binary instance) ;
    
    R visit(Braces instance) ;
    
    R visit(Column instance) ;
    
    R visit(Dataset instance) ;
    
    R visit(Index instance) ;
    
    R visit(List instance) ;
    
    R visit(Literal instance) ;
    
    R visit(Or instance) ;
    
    R visit(Parentheses instance) ;
    
    R visit(Property instance) ;
    
    R visit(Unary instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Expression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(And instance) {
      return otherwise((instance));
    }
    
    default R visit(Any instance) {
      return otherwise((instance));
    }
    
    default R visit(Between instance) {
      return otherwise((instance));
    }
    
    default R visit(Binary instance) {
      return otherwise((instance));
    }
    
    default R visit(Braces instance) {
      return otherwise((instance));
    }
    
    default R visit(Column instance) {
      return otherwise((instance));
    }
    
    default R visit(Dataset instance) {
      return otherwise((instance));
    }
    
    default R visit(Index instance) {
      return otherwise((instance));
    }
    
    default R visit(List instance) {
      return otherwise((instance));
    }
    
    default R visit(Literal instance) {
      return otherwise((instance));
    }
    
    default R visit(Or instance) {
      return otherwise((instance));
    }
    
    default R visit(Parentheses instance) {
      return otherwise((instance));
    }
    
    default R visit(Property instance) {
      return otherwise((instance));
    }
    
    default R visit(Unary instance) {
      return otherwise((instance));
    }
  }
  
  public static final class And extends hydra.langs.kusto.kql.Expression implements Serializable {
    public final java.util.List<hydra.langs.kusto.kql.Expression> value;
    
    public And (java.util.List<hydra.langs.kusto.kql.Expression> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof And)) {
        return false;
      }
      And o = (And) (other);
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
  
  public static final class Any extends hydra.langs.kusto.kql.Expression implements Serializable {
    public Any () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Any)) {
        return false;
      }
      Any o = (Any) (other);
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
  
  public static final class Between extends hydra.langs.kusto.kql.Expression implements Serializable {
    public final hydra.langs.kusto.kql.BetweenExpression value;
    
    public Between (hydra.langs.kusto.kql.BetweenExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Between)) {
        return false;
      }
      Between o = (Between) (other);
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
  
  public static final class Binary extends hydra.langs.kusto.kql.Expression implements Serializable {
    public final hydra.langs.kusto.kql.BinaryExpression value;
    
    public Binary (hydra.langs.kusto.kql.BinaryExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Binary)) {
        return false;
      }
      Binary o = (Binary) (other);
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
  
  public static final class Braces extends hydra.langs.kusto.kql.Expression implements Serializable {
    public final hydra.langs.kusto.kql.Expression value;
    
    public Braces (hydra.langs.kusto.kql.Expression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Braces)) {
        return false;
      }
      Braces o = (Braces) (other);
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
  
  public static final class Column extends hydra.langs.kusto.kql.Expression implements Serializable {
    public final hydra.langs.kusto.kql.ColumnName value;
    
    public Column (hydra.langs.kusto.kql.ColumnName value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Column)) {
        return false;
      }
      Column o = (Column) (other);
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
  
  public static final class Dataset extends hydra.langs.kusto.kql.Expression implements Serializable {
    public final hydra.langs.kusto.kql.TableName value;
    
    public Dataset (hydra.langs.kusto.kql.TableName value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Dataset)) {
        return false;
      }
      Dataset o = (Dataset) (other);
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
  
  public static final class Index extends hydra.langs.kusto.kql.Expression implements Serializable {
    public final hydra.langs.kusto.kql.IndexExpression value;
    
    public Index (hydra.langs.kusto.kql.IndexExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Index)) {
        return false;
      }
      Index o = (Index) (other);
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
  
  public static final class List extends hydra.langs.kusto.kql.Expression implements Serializable {
    public final java.util.List<hydra.langs.kusto.kql.Expression> value;
    
    public List (java.util.List<hydra.langs.kusto.kql.Expression> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) (other);
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
  
  public static final class Literal extends hydra.langs.kusto.kql.Expression implements Serializable {
    public final hydra.langs.kusto.kql.Literal value;
    
    public Literal (hydra.langs.kusto.kql.Literal value) {
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
  
  public static final class Or extends hydra.langs.kusto.kql.Expression implements Serializable {
    public final java.util.List<hydra.langs.kusto.kql.Expression> value;
    
    public Or (java.util.List<hydra.langs.kusto.kql.Expression> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Or)) {
        return false;
      }
      Or o = (Or) (other);
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
  
  public static final class Parentheses extends hydra.langs.kusto.kql.Expression implements Serializable {
    public final hydra.langs.kusto.kql.Expression value;
    
    public Parentheses (hydra.langs.kusto.kql.Expression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parentheses)) {
        return false;
      }
      Parentheses o = (Parentheses) (other);
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
  
  public static final class Property extends hydra.langs.kusto.kql.Expression implements Serializable {
    public final hydra.langs.kusto.kql.PropertyExpression value;
    
    public Property (hydra.langs.kusto.kql.PropertyExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Property)) {
        return false;
      }
      Property o = (Property) (other);
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
  
  public static final class Unary extends hydra.langs.kusto.kql.Expression implements Serializable {
    public final hydra.langs.kusto.kql.UnaryExpression value;
    
    public Unary (hydra.langs.kusto.kql.UnaryExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unary)) {
        return false;
      }
      Unary o = (Unary) (other);
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