(* A model for language-agnostic graph pattern queries *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core.
Definition Variable_ : Type :=
  string.

Record Range : Type := Build_Range {
  range_min : Z ;
  range_max : Z
}.

Inductive RegexQuantifier : Type :=
| RegexQuantifier_One : unit -> RegexQuantifier
| RegexQuantifier_ZeroOrOne : unit -> RegexQuantifier
| RegexQuantifier_ZeroOrMore : unit -> RegexQuantifier
| RegexQuantifier_OneOrMore : unit -> RegexQuantifier
| RegexQuantifier_Exactly : Z -> RegexQuantifier
| RegexQuantifier_AtLeast : Z -> RegexQuantifier
| RegexQuantifier_Range : Range -> RegexQuantifier.

Inductive Node : Type :=
| Node_Term : Term -> Node
| Node_Variable : Variable_ -> Node
| Node_Wildcard : unit -> Node.

Record Edge : Type := Build_Edge {
  edge_type : Name ;
  edge_out : (option) (Name) ;
  edge_in_ : (option) (Name)
}.

Inductive ComparisonConstraint : Type :=
| ComparisonConstraint_Equal : unit -> ComparisonConstraint
| ComparisonConstraint_NotEqual : unit -> ComparisonConstraint
| ComparisonConstraint_LessThan : unit -> ComparisonConstraint
| ComparisonConstraint_GreaterThan : unit -> ComparisonConstraint
| ComparisonConstraint_LessThanOrEqual : unit -> ComparisonConstraint
| ComparisonConstraint_GreaterThanOrEqual : unit -> ComparisonConstraint.

Inductive Step : Type :=
| Step_Edge : Edge -> Step
| Step_Project : Projection -> Step
| Step_Compare : ComparisonConstraint -> Step.

Inductive Path : Type :=
| Path_Step : Step -> Path
| Path_Regex : RegexSequence -> Path
| Path_Inverse : Path -> Path
with RegexSequence : Type :=
| Build_RegexSequence : Path -> RegexQuantifier -> RegexSequence.

Definition regexSequence_path (r_ : RegexSequence) :=
  match r_ with
| Build_RegexSequence f0 f1 => f0
end.

Definition regexSequence_quantifier (r_ : RegexSequence) :=
  match r_ with
| Build_RegexSequence f0 f1 => f1
end.

Record PathEquation : Type := Build_PathEquation {
  pathEquation_left : Path ;
  pathEquation_right : Path
}.

Record TriplePattern : Type := Build_TriplePattern {
  triplePattern_subject : Node ;
  triplePattern_predicate : Path ;
  triplePattern_object : Node
}.

Inductive Pattern : Type :=
| Pattern_Triple : TriplePattern -> Pattern
| Pattern_Negation : Pattern -> Pattern
| Pattern_Conjunction : (list) (Pattern) -> Pattern
| Pattern_Disjunction : (list) (Pattern) -> Pattern
| Pattern_Graph : GraphPattern -> Pattern
with GraphPattern : Type :=
| Build_GraphPattern : Name -> (list) (Pattern) -> GraphPattern.

Definition graphPattern_graph_ (r_ : GraphPattern) :=
  match r_ with
| Build_GraphPattern f0 f1 => f0
end.

Definition graphPattern_patterns (r_ : GraphPattern) :=
  match r_ with
| Build_GraphPattern f0 f1 => f1
end.

Record PatternImplication : Type := Build_PatternImplication {
  patternImplication_antecedent : Pattern ;
  patternImplication_consequent : Pattern
}.

Record Query : Type := Build_Query {
  query_variables : (list) (Variable_) ;
  query_patterns : (list) (Pattern)
}.

