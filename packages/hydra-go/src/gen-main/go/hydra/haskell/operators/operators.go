// Note: this is an automatically generated file. Do not edit.

package exthaskelloperators

import (
  "hydra.dev/hydra/ast"
  libmath "hydra.dev/hydra/lib/math"
  "hydra.dev/hydra/serialization"
)

var AndOp = serialization.Op("&&", 3, ast.AssociativityRight{})

var ApOp = serialization.Op("<*>", 4, ast.AssociativityLeft{})

var AppOp = ast.Op{Symbol: ast.Symbol(""), Padding: ast.Padding{Left: ast.WsNone{}, Right: ast.WsSpace{}}, Precedence: ast.Precedence(0), Associativity: ast.AssociativityLeft{}}

var ApplyOp = serialization.Op("$", 0, ast.AssociativityRight{})

var ArrowOp = serialization.Op("->", libmath.Negate(1).(int32), ast.AssociativityRight{})

var AssertOp = serialization.Op("=>", 0, ast.AssociativityNone{})

var BindOp = serialization.Op(">>=", 1, ast.AssociativityLeft{})

var CaseOp = serialization.Op("->", 0, ast.AssociativityNone{})

var ComposeOp = serialization.Op(".", 9, ast.AssociativityLeft{})

var ConcatOp = serialization.Op("++", 5, ast.AssociativityRight{})

var ConsOp = serialization.Op(":", 5, ast.AssociativityRight{})

var DefineOp = serialization.Op("=", 0, ast.AssociativityNone{})

var DiamondOp = serialization.Op("<>", 6, ast.AssociativityRight{})

var DivOp = serialization.Op("`div`", 7, ast.AssociativityLeft{})

var DivideOp = serialization.Op("/", 7, ast.AssociativityLeft{})

var ElemOp = serialization.Op("`elem`", 4, ast.AssociativityNone{})

var EqualOp = serialization.Op("==", 4, ast.AssociativityNone{})

var FmapOp = serialization.Op("<$>", 4, ast.AssociativityLeft{})

var GtOp = serialization.Op(">", 4, ast.AssociativityNone{})

var GteOp = serialization.Op(">=", 4, ast.AssociativityNone{})

var IndexOp = serialization.Op("!!", 9, ast.AssociativityLeft{})

var LambdaOp = serialization.Op("->", libmath.Negate(1).(int32), ast.AssociativityRight{})

var LtOp = serialization.Op("<", 4, ast.AssociativityNone{})

var LteOp = serialization.Op(">=", 4, ast.AssociativityNone{})

var MinusOp = serialization.Op("-", 6, ast.AssociativityBoth{})

var ModOp = serialization.Op("`mod`", 7, ast.AssociativityLeft{})

var MultOp = serialization.Op("*", 7, ast.AssociativityBoth{})

var NeqOp = serialization.Op("/=", 4, ast.AssociativityNone{})

var NotElemOp = serialization.Op("`notElem`", 4, ast.AssociativityNone{})

var OrOp = serialization.Op("||", 2, ast.AssociativityRight{})

var PlusOp = serialization.Op("+", 6, ast.AssociativityBoth{})

var QuotOp = serialization.Op("`quot`", 7, ast.AssociativityLeft{})

var RemOp = serialization.Op("`rem`", 7, ast.AssociativityLeft{})

var TypeOp = serialization.Op("::", 0, ast.AssociativityNone{})
