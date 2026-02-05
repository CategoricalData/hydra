// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.operators;

/**
 * AST operators for Haskell
 */
public interface Operators {
  static hydra.ast.Op andOp() {
    return hydra.serialization.Serialization.op(
      "&&",
      3,
      new hydra.ast.Associativity.Right());
  }
  
  static hydra.ast.Op apOp() {
    return hydra.serialization.Serialization.op(
      "<*>",
      4,
      new hydra.ast.Associativity.Left());
  }
  
  static hydra.ast.Op appOp() {
    return new hydra.ast.Op(new hydra.ast.Symbol(""), new hydra.ast.Padding(new hydra.ast.Ws.None(), new hydra.ast.Ws.Space()), new hydra.ast.Precedence(0), new hydra.ast.Associativity.Left());
  }
  
  static hydra.ast.Op applyOp() {
    return hydra.serialization.Serialization.op(
      "$",
      0,
      new hydra.ast.Associativity.Right());
  }
  
  static hydra.ast.Op arrowOp() {
    return hydra.serialization.Serialization.op(
      "->",
      hydra.lib.math.Negate.apply(1),
      new hydra.ast.Associativity.Right());
  }
  
  static hydra.ast.Op assertOp() {
    return hydra.serialization.Serialization.op(
      "=>",
      0,
      new hydra.ast.Associativity.None());
  }
  
  static hydra.ast.Op bindOp() {
    return hydra.serialization.Serialization.op(
      ">>=",
      1,
      new hydra.ast.Associativity.Left());
  }
  
  static hydra.ast.Op caseOp() {
    return hydra.serialization.Serialization.op(
      "->",
      0,
      new hydra.ast.Associativity.None());
  }
  
  static hydra.ast.Op composeOp() {
    return hydra.serialization.Serialization.op(
      ".",
      9,
      new hydra.ast.Associativity.Left());
  }
  
  static hydra.ast.Op concatOp() {
    return hydra.serialization.Serialization.op(
      "++",
      5,
      new hydra.ast.Associativity.Right());
  }
  
  static hydra.ast.Op consOp() {
    return hydra.serialization.Serialization.op(
      ":",
      5,
      new hydra.ast.Associativity.Right());
  }
  
  static hydra.ast.Op defineOp() {
    return hydra.serialization.Serialization.op(
      "=",
      0,
      new hydra.ast.Associativity.None());
  }
  
  static hydra.ast.Op diamondOp() {
    return hydra.serialization.Serialization.op(
      "<>",
      6,
      new hydra.ast.Associativity.Right());
  }
  
  static hydra.ast.Op divOp() {
    return hydra.serialization.Serialization.op(
      "`div`",
      7,
      new hydra.ast.Associativity.Left());
  }
  
  static hydra.ast.Op divideOp() {
    return hydra.serialization.Serialization.op(
      "/",
      7,
      new hydra.ast.Associativity.Left());
  }
  
  static hydra.ast.Op elemOp() {
    return hydra.serialization.Serialization.op(
      "`elem`",
      4,
      new hydra.ast.Associativity.None());
  }
  
  static hydra.ast.Op equalOp() {
    return hydra.serialization.Serialization.op(
      "==",
      4,
      new hydra.ast.Associativity.None());
  }
  
  static hydra.ast.Op fmapOp() {
    return hydra.serialization.Serialization.op(
      "<$>",
      4,
      new hydra.ast.Associativity.Left());
  }
  
  static hydra.ast.Op gtOp() {
    return hydra.serialization.Serialization.op(
      ">",
      4,
      new hydra.ast.Associativity.None());
  }
  
  static hydra.ast.Op gteOp() {
    return hydra.serialization.Serialization.op(
      ">=",
      4,
      new hydra.ast.Associativity.None());
  }
  
  static hydra.ast.Op indexOp() {
    return hydra.serialization.Serialization.op(
      "!!",
      9,
      new hydra.ast.Associativity.Left());
  }
  
  static hydra.ast.Op lambdaOp() {
    return hydra.serialization.Serialization.op(
      "->",
      hydra.lib.math.Negate.apply(1),
      new hydra.ast.Associativity.Right());
  }
  
  static hydra.ast.Op ltOp() {
    return hydra.serialization.Serialization.op(
      "<",
      4,
      new hydra.ast.Associativity.None());
  }
  
  static hydra.ast.Op lteOp() {
    return hydra.serialization.Serialization.op(
      ">=",
      4,
      new hydra.ast.Associativity.None());
  }
  
  static hydra.ast.Op minusOp() {
    return hydra.serialization.Serialization.op(
      "-",
      6,
      new hydra.ast.Associativity.Both());
  }
  
  static hydra.ast.Op modOp() {
    return hydra.serialization.Serialization.op(
      "`mod`",
      7,
      new hydra.ast.Associativity.Left());
  }
  
  static hydra.ast.Op multOp() {
    return hydra.serialization.Serialization.op(
      "*",
      7,
      new hydra.ast.Associativity.Both());
  }
  
  static hydra.ast.Op neqOp() {
    return hydra.serialization.Serialization.op(
      "/=",
      4,
      new hydra.ast.Associativity.None());
  }
  
  static hydra.ast.Op notElemOp() {
    return hydra.serialization.Serialization.op(
      "`notElem`",
      4,
      new hydra.ast.Associativity.None());
  }
  
  static hydra.ast.Op orOp() {
    return hydra.serialization.Serialization.op(
      "||",
      2,
      new hydra.ast.Associativity.Right());
  }
  
  static hydra.ast.Op plusOp() {
    return hydra.serialization.Serialization.op(
      "+",
      6,
      new hydra.ast.Associativity.Both());
  }
  
  static hydra.ast.Op quotOp() {
    return hydra.serialization.Serialization.op(
      "`quot`",
      7,
      new hydra.ast.Associativity.Left());
  }
  
  static hydra.ast.Op remOp() {
    return hydra.serialization.Serialization.op(
      "`rem`",
      7,
      new hydra.ast.Associativity.Left());
  }
  
  static hydra.ast.Op typeOp() {
    return hydra.serialization.Serialization.op(
      "::",
      0,
      new hydra.ast.Associativity.None());
  }
}
