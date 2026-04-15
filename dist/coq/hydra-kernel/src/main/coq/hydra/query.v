(* A model for language-agnostic graph pattern queries *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core.
Inductive ComparisonConstraint : Type :=
| ComparisonConstraint_Equal : forall (_ : unit) , ComparisonConstraint
| ComparisonConstraint_NotEqual : forall (_ : unit) , ComparisonConstraint
| ComparisonConstraint_LessThan : forall (_ : unit) , ComparisonConstraint
| ComparisonConstraint_GreaterThan : forall (_ : unit) , ComparisonConstraint
| ComparisonConstraint_LessThanOrEqual : forall (_ : unit) , ComparisonConstraint
| ComparisonConstraint_GreaterThanOrEqual : forall (_ : unit) , ComparisonConstraint.

Record Edge : Type := Build_Edge {
edge_type : Name ;
edge_out : (option) (Name) ;
edge_in_ : (option) (Name) ;
}.

Definition Variable_ : Type := string.

Inductive Node : Type :=
| Node_Term : forall (_ : Term) , Node
| Node_Variable : forall (_ : Variable_) , Node
| Node_Wildcard : forall (_ : unit) , Node.

Record Range : Type := Build_Range {
range_min : Z ;
range_max : Z ;
}.

Inductive RegexQuantifier : Type :=
| RegexQuantifier_One : forall (_ : unit) , RegexQuantifier
| RegexQuantifier_ZeroOrOne : forall (_ : unit) , RegexQuantifier
| RegexQuantifier_ZeroOrMore : forall (_ : unit) , RegexQuantifier
| RegexQuantifier_OneOrMore : forall (_ : unit) , RegexQuantifier
| RegexQuantifier_Exactly : forall (_ : Z) , RegexQuantifier
| RegexQuantifier_AtLeast : forall (_ : Z) , RegexQuantifier
| RegexQuantifier_Range : forall (_ : Range) , RegexQuantifier.

Inductive Step : Type :=
| Step_Edge : forall (_ : Edge) , Step
| Step_Project : forall (_ : Projection) , Step
| Step_Compare : forall (_ : ComparisonConstraint) , Step.

Inductive Path : Type :=
| Path_Step : forall (_ : Step) , Path
| Path_Regex : forall (_ : RegexSequence) , Path
| Path_Inverse : forall (_ : Path) , Path
with RegexSequence : Type :=
| Build_RegexSequence : forall (_ : Path) , forall (_ : RegexQuantifier) , RegexSequence.

Definition regexSequence_path (r_ : RegexSequence) := match r_ with
| Build_RegexSequence f0 f1 => f0
end.

Definition regexSequence_quantifier (r_ : RegexSequence) := match r_ with
| Build_RegexSequence f0 f1 => f1
end.

Record TriplePattern : Type := Build_TriplePattern {
triplePattern_subject : Node ;
triplePattern_predicate : Path ;
triplePattern_object : Node ;
}.

Inductive GraphPattern : Type :=
| Build_GraphPattern : forall (_ : Name) , forall (_ : (list) (Pattern)) , GraphPattern
with Pattern : Type :=
| Pattern_Triple : forall (_ : TriplePattern) , Pattern
| Pattern_Negation : forall (_ : Pattern) , Pattern
| Pattern_Conjunction : forall (_ : (list) (Pattern)) , Pattern
| Pattern_Disjunction : forall (_ : (list) (Pattern)) , Pattern
| Pattern_Graph : forall (_ : GraphPattern) , Pattern.

Definition graphPattern_graph_ (r_ : GraphPattern) := match r_ with
| Build_GraphPattern f0 f1 => f0
end.

Definition graphPattern_patterns (r_ : GraphPattern) := match r_ with
| Build_GraphPattern f0 f1 => f1
end.

Record PathEquation : Type := Build_PathEquation {
pathEquation_left : Path ;
pathEquation_right : Path ;
}.

Record PatternImplication : Type := Build_PatternImplication {
patternImplication_antecedent : Pattern ;
patternImplication_consequent : Pattern ;
}.

Record Query : Type := Build_Query {
query_variables : (list) (Variable_) ;
query_patterns : (list) (Pattern) ;
}.

