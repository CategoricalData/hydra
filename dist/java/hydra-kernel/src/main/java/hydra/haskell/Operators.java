// Note: this is an automatically generated file. Do not edit.

package hydra.haskell;

/**
 * AST operators for Haskell
 */
public interface Operators {
  static hydra.ast.Op andOp() {
    return hydra.Serialization.op(
      "&&",
      3,
      new hydra.ast.Associativity.Right());
  }

  static hydra.ast.Op apOp() {
    return hydra.Serialization.op(
      "<*>",
      4,
      new hydra.ast.Associativity.Left());
  }

  static hydra.ast.Op appOp() {
    return new hydra.ast.Op(new hydra.ast.Symbol(""), new hydra.ast.Padding(new hydra.ast.Ws.None(), new hydra.ast.Ws.Space()), new hydra.ast.Precedence(0), new hydra.ast.Associativity.Left());
  }

  static hydra.ast.Op applyOp() {
    return hydra.Serialization.op(
      "$",
      0,
      new hydra.ast.Associativity.Right());
  }

  static hydra.ast.Op arrowOp() {
    return hydra.Serialization.op(
      "->",
      hydra.lib.math.Negate.apply(1),
      new hydra.ast.Associativity.Right());
  }

  static hydra.ast.Op assertOp() {
    return hydra.Serialization.op(
      "=>",
      0,
      new hydra.ast.Associativity.None());
  }

  static hydra.ast.Op bindOp() {
    return hydra.Serialization.op(
      ">>=",
      1,
      new hydra.ast.Associativity.Left());
  }

  static hydra.ast.Op caseOp() {
    return hydra.Serialization.op(
      "->",
      0,
      new hydra.ast.Associativity.None());
  }

  static hydra.ast.Op composeOp() {
    return hydra.Serialization.op(
      ".",
      9,
      new hydra.ast.Associativity.Left());
  }

  static hydra.ast.Op concatOp() {
    return hydra.Serialization.op(
      "++",
      5,
      new hydra.ast.Associativity.Right());
  }

  static hydra.ast.Op consOp() {
    return hydra.Serialization.op(
      ":",
      5,
      new hydra.ast.Associativity.Right());
  }

  static hydra.ast.Op defineOp() {
    return hydra.Serialization.op(
      "=",
      0,
      new hydra.ast.Associativity.None());
  }

  static hydra.ast.Op diamondOp() {
    return hydra.Serialization.op(
      "<>",
      6,
      new hydra.ast.Associativity.Right());
  }

  static hydra.ast.Op divOp() {
    return hydra.Serialization.op(
      "`div`",
      7,
      new hydra.ast.Associativity.Left());
  }

  static hydra.ast.Op divideOp() {
    return hydra.Serialization.op(
      "/",
      7,
      new hydra.ast.Associativity.Left());
  }

  static hydra.ast.Op elemOp() {
    return hydra.Serialization.op(
      "`elem`",
      4,
      new hydra.ast.Associativity.None());
  }

  static hydra.ast.Op equalOp() {
    return hydra.Serialization.op(
      "==",
      4,
      new hydra.ast.Associativity.None());
  }

  static hydra.ast.Op fmapOp() {
    return hydra.Serialization.op(
      "<$>",
      4,
      new hydra.ast.Associativity.Left());
  }

  static hydra.ast.Op gtOp() {
    return hydra.Serialization.op(
      ">",
      4,
      new hydra.ast.Associativity.None());
  }

  static hydra.ast.Op gteOp() {
    return hydra.Serialization.op(
      ">=",
      4,
      new hydra.ast.Associativity.None());
  }

  static hydra.ast.Op indexOp() {
    return hydra.Serialization.op(
      "!!",
      9,
      new hydra.ast.Associativity.Left());
  }

  static hydra.ast.Op lambdaOp() {
    return hydra.Serialization.op(
      "->",
      hydra.lib.math.Negate.apply(1),
      new hydra.ast.Associativity.Right());
  }

  static hydra.ast.Op ltOp() {
    return hydra.Serialization.op(
      "<",
      4,
      new hydra.ast.Associativity.None());
  }

  static hydra.ast.Op lteOp() {
    return hydra.Serialization.op(
      ">=",
      4,
      new hydra.ast.Associativity.None());
  }

  static hydra.ast.Op minusOp() {
    return hydra.Serialization.op(
      "-",
      6,
      new hydra.ast.Associativity.Both());
  }

  static hydra.ast.Op modOp() {
    return hydra.Serialization.op(
      "`mod`",
      7,
      new hydra.ast.Associativity.Left());
  }

  static hydra.ast.Op multOp() {
    return hydra.Serialization.op(
      "*",
      7,
      new hydra.ast.Associativity.Both());
  }

  static hydra.ast.Op neqOp() {
    return hydra.Serialization.op(
      "/=",
      4,
      new hydra.ast.Associativity.None());
  }

  static hydra.ast.Op notElemOp() {
    return hydra.Serialization.op(
      "`notElem`",
      4,
      new hydra.ast.Associativity.None());
  }

  static hydra.ast.Op orOp() {
    return hydra.Serialization.op(
      "||",
      2,
      new hydra.ast.Associativity.Right());
  }

  static hydra.ast.Op plusOp() {
    return hydra.Serialization.op(
      "+",
      6,
      new hydra.ast.Associativity.Both());
  }

  static hydra.ast.Op quotOp() {
    return hydra.Serialization.op(
      "`quot`",
      7,
      new hydra.ast.Associativity.Left());
  }

  static hydra.ast.Op remOp() {
    return hydra.Serialization.op(
      "`rem`",
      7,
      new hydra.ast.Associativity.Left());
  }

  static hydra.ast.Op typeOp() {
    return hydra.Serialization.op(
      "::",
      0,
      new hydra.ast.Associativity.None());
  }
}
