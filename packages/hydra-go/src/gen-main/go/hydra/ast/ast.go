// Note: this is an automatically generated file. Do not edit.

package ast

type Associativity interface {
  isAssociativity()
}

type AssociativityNone struct{}

func (AssociativityNone) isAssociativity() {

}

type AssociativityLeft struct{}

func (AssociativityLeft) isAssociativity() {

}

type AssociativityRight struct{}

func (AssociativityRight) isAssociativity() {

}

type AssociativityBoth struct{}

func (AssociativityBoth) isAssociativity() {

}

type BlockStyle struct {
  Indent any
  NewlineBeforeContent bool
  NewlineAfterContent bool
}

type BracketExpr struct {
  Brackets Brackets
  Enclosed Expr
  Style BlockStyle
}

type Brackets struct {
  Open Symbol
  Close_ Symbol
}

type Expr interface {
  isExpr()
}

type ExprConst_ struct {
  Value Symbol
}

func (ExprConst_) isExpr() {

}

type ExprIndent struct {
  Value IndentedExpression
}

func (ExprIndent) isExpr() {

}

type ExprOp struct {
  Value OpExpr
}

func (ExprOp) isExpr() {

}

type ExprBrackets struct {
  Value BracketExpr
}

func (ExprBrackets) isExpr() {

}

type ExprSeq struct {
  Value SeqExpr
}

func (ExprSeq) isExpr() {

}

type IndentedExpression struct {
  Style IndentStyle
  Expr Expr
}

type IndentStyle interface {
  isIndentStyle()
}

type IndentStyleAllLines struct {
  Value string
}

func (IndentStyleAllLines) isIndentStyle() {

}

type IndentStyleSubsequentLines struct {
  Value string
}

func (IndentStyleSubsequentLines) isIndentStyle() {

}

type Op struct {
  Symbol Symbol
  Padding Padding
  Precedence Precedence
  Associativity Associativity
}

type OpExpr struct {
  Op Op
  Lhs Expr
  Rhs Expr
}

type Padding struct {
  Left Ws
  Right Ws
}

type Precedence int32

type SeqExpr struct {
  Op Op
  Elements []any
}

type Symbol string

type Ws interface {
  isWs()
}

type WsNone struct{}

func (WsNone) isWs() {

}

type WsSpace struct{}

func (WsSpace) isWs() {

}

type WsBreak_ struct{}

func (WsBreak_) isWs() {

}

type WsBreakAndIndent struct {
  Value string
}

func (WsBreakAndIndent) isWs() {

}

type WsDoubleBreak struct{}

func (WsDoubleBreak) isWs() {

}
