// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public abstract class TableCommitAction implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.ansi.sql.syntax.TableCommitAction");
  
  public static final hydra.core.Name FIELD_NAME_PRESERVE = new hydra.core.Name("preserve");
  
  public static final hydra.core.Name FIELD_NAME_DELETE = new hydra.core.Name("delete");
  
  private TableCommitAction () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Preserve instance) ;
    
    R visit(Delete instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TableCommitAction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Preserve instance) {
      return otherwise((instance));
    }
    
    default R visit(Delete instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Preserve extends hydra.ext.org.ansi.sql.syntax.TableCommitAction implements Serializable {
    public Preserve () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Preserve)) {
        return false;
      }
      Preserve o = (Preserve) (other);
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
  
  public static final class Delete extends hydra.ext.org.ansi.sql.syntax.TableCommitAction implements Serializable {
    public Delete () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Delete)) {
        return false;
      }
      Delete o = (Delete) (other);
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