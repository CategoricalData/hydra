{-
Example "Lispy" model created for wolfy

writePython "/tmp/python" mainModules [Org.Example.Lispy.module_]
-}

module Org.Example.Lispy where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "org.example.lispy"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [] [Core.ns] $
    Just "Data model definition for the Lispy interpreter"
  where
    elements = [
      atom,
      closure,
      env,
      expr,
      lispList,
      number,
      proc,
      sExpr,
      symbol,
      value]

atom = define "Atom" $
  doc "An atomic Lispy value: number, boolean, None, or symbol" $
  T.union [
    "number">: number,
    "boolean">: T.boolean,
    "none">: T.unit,
    "symbol">: symbol]

closure = define "Closure" $
  T.record [
    "params">: T.list symbol,
    "body">: expr,
    "env">: env]

env = define "Env" $
  doc "Lexical environment with optional parent chaining." $
  T.record [
    "bindings">: T.map T.string value,
    "parent">: T.optional env]

expr = define "Expr" $
  doc "An expression is either an atom or an S-expression." $
  T.union [
    "atom">: atom,
    "sexpr">: sExpr]

lispList = define "LispList" $
  T.list value

number = define "Number" $
  doc ("Either an integer or a floating point number. "
    ++ "NOTE: `bool` is a subclass of `int` at runtime, but we keep it explicit here to "
    ++ "reflect Lispy truthiness (only `False`/`None` are falsey).") $
  T.union [
    "int">: T.int32,
    "float">: T.float32]

proc = define "Proc" $
  doc "A built-in procedure" $
  T.string -- Placeholder for Callable[..., Value]

sExpr = define "SExpr" $
  doc "An S-expression is a tuple whose elements are expressions (recursive type)." $
  T.list expr

symbol = define "Symbol" $
  T.string

value = define "Value" $
  doc ("Runtime values:\n"
    ++ "- atoms (numbers/bools/None/symbols)\n"
    ++ "- Lisp lists (tuples of values)\n"
    ++ "- procedures (builtins and closures)\n\n"
    ++ "We keep Expr separate because it includes unevaluated S-expressions; Value is "
    ++ "what evaluation can produce.") $
  T.union [
    "atom">: atom,
    "list">: lispList,
    "proc">: proc,
    "closure">: closure]
