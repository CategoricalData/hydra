(* Term encoders for hydra.query *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.query hydra.core hydra.encode.core hydra.lib.maybes hydra.lib.lists.

Definition variable : Variable_ -> Term :=
  fun (x : Variable_) => (Term_Wrap) ((Build_WrappedTerm) ("Variable_"%string) ((fun (x2 : string) => (Term_Literal) ((Literal_String) (x2))) ((fun w_ => w_) (x)))).
Definition range : Range -> Term :=
  fun (x : Range) => (Term_Record) ((Build_Record_) ("Range"%string) ((cons) ((Build_Field) ("min"%string) ((fun (x2 : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) (x2)))) ((fun r_ => (range_min) (r_)) (x)))) ((cons) ((Build_Field) ("max"%string) ((fun (x2 : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) (x2)))) ((fun r_ => (range_max) (r_)) (x)))) (nil)))).
Definition regexQuantifier : RegexQuantifier -> Term :=
  fun x_ => match x_ with
| RegexQuantifier_One v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("RegexQuantifier"%string) ((Build_Field) ("one"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| RegexQuantifier_ZeroOrOne v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("RegexQuantifier"%string) ((Build_Field) ("zeroOrOne"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| RegexQuantifier_ZeroOrMore v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("RegexQuantifier"%string) ((Build_Field) ("zeroOrMore"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| RegexQuantifier_OneOrMore v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("RegexQuantifier"%string) ((Build_Field) ("oneOrMore"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| RegexQuantifier_Exactly v_ => (fun (y : Z) => (Term_Inject) ((Build_Injection) ("RegexQuantifier"%string) ((Build_Field) ("exactly"%string) ((fun (x : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) (x)))) (y))))) (v_)
| RegexQuantifier_AtLeast v_ => (fun (y : Z) => (Term_Inject) ((Build_Injection) ("RegexQuantifier"%string) ((Build_Field) ("atLeast"%string) ((fun (x : Z) => (Term_Literal) ((Literal_Integer) ((IntegerValue_Int32) (x)))) (y))))) (v_)
| RegexQuantifier_Range v_ => (fun (y : Range) => (Term_Inject) ((Build_Injection) ("RegexQuantifier"%string) ((Build_Field) ("range"%string) ((range) (y))))) (v_)
end.
Definition node : Node -> Term :=
  fun x_ => match x_ with
| Node_Term v_ => (fun (y : Term) => (Term_Inject) ((Build_Injection) ("Node"%string) ((Build_Field) ("term"%string) ((hydra.encode.core.term) (y))))) (v_)
| Node_Variable v_ => (fun (y : Variable_) => (Term_Inject) ((Build_Injection) ("Node"%string) ((Build_Field) ("variable"%string) ((variable) (y))))) (v_)
| Node_Wildcard v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("Node"%string) ((Build_Field) ("wildcard"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
end.
Definition edge : Edge -> Term :=
  fun (x : Edge) => (Term_Record) ((Build_Record_) ("Edge"%string) ((cons) ((Build_Field) ("type"%string) ((hydra.encode.core.name) ((fun r_ => (edge_type) (r_)) (x)))) ((cons) ((Build_Field) ("out"%string) ((fun (opt : (option) (Name)) => (Term_Maybe) (((maybes.map) (hydra.encode.core.name)) (opt))) ((fun r_ => (edge_out) (r_)) (x)))) ((cons) ((Build_Field) ("in"%string) ((fun (opt : (option) (Name)) => (Term_Maybe) (((maybes.map) (hydra.encode.core.name)) (opt))) ((fun r_ => (edge_in_) (r_)) (x)))) (nil))))).
Definition comparisonConstraint : ComparisonConstraint -> Term :=
  fun x_ => match x_ with
| ComparisonConstraint_Equal v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("ComparisonConstraint"%string) ((Build_Field) ("equal"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| ComparisonConstraint_NotEqual v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("ComparisonConstraint"%string) ((Build_Field) ("notEqual"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| ComparisonConstraint_LessThan v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("ComparisonConstraint"%string) ((Build_Field) ("lessThan"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| ComparisonConstraint_GreaterThan v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("ComparisonConstraint"%string) ((Build_Field) ("greaterThan"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| ComparisonConstraint_LessThanOrEqual v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("ComparisonConstraint"%string) ((Build_Field) ("lessThanOrEqual"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
| ComparisonConstraint_GreaterThanOrEqual v_ => (fun (y : unit) => (Term_Inject) ((Build_Injection) ("ComparisonConstraint"%string) ((Build_Field) ("greaterThanOrEqual"%string) ((fun (_ : unit) => (Term_Unit) (tt)) (y))))) (v_)
end.
Definition step : Step -> Term :=
  fun x_ => match x_ with
| Step_Edge v_ => (fun (y : Edge) => (Term_Inject) ((Build_Injection) ("Step"%string) ((Build_Field) ("edge"%string) ((edge) (y))))) (v_)
| Step_Project v_ => (fun (y : Projection) => (Term_Inject) ((Build_Injection) ("Step"%string) ((Build_Field) ("project"%string) ((hydra.encode.core.projection) (y))))) (v_)
| Step_Compare v_ => (fun (y : ComparisonConstraint) => (Term_Inject) ((Build_Injection) ("Step"%string) ((Build_Field) ("compare"%string) ((comparisonConstraint) (y))))) (v_)
end.
Definition path_regexSequence_bundle :=
  hydra_fix (fun (bundle_ : prod (Path -> Term) (RegexSequence -> Term)) =>
    let path := (fst bundle_) in
    let regexSequence := (snd bundle_) in
    (pair (fun x_ => match x_ with
| Path_Step v_ => (fun (y : Step) => (Term_Inject) ((Build_Injection) ("Path"%string) ((Build_Field) ("step"%string) ((step) (y))))) (v_)
| Path_Regex v_ => (fun (y : RegexSequence) => (Term_Inject) ((Build_Injection) ("Path"%string) ((Build_Field) ("regex"%string) ((regexSequence) (y))))) (v_)
| Path_Inverse v_ => (fun (y : Path) => (Term_Inject) ((Build_Injection) ("Path"%string) ((Build_Field) ("inverse"%string) ((path) (y))))) (v_)
end) (fun (x : RegexSequence) => (Term_Record) ((Build_Record_) ("RegexSequence"%string) ((cons) ((Build_Field) ("path"%string) ((path) ((fun r_ => (regexSequence_path) (r_)) (x)))) ((cons) ((Build_Field) ("quantifier"%string) ((regexQuantifier) ((fun r_ => (regexSequence_quantifier) (r_)) (x)))) (nil))))))).

Definition path : Path -> Term :=
  (fst path_regexSequence_bundle).
Definition regexSequence : RegexSequence -> Term :=
  (snd path_regexSequence_bundle).
Definition pathEquation : PathEquation -> Term :=
  fun (x : PathEquation) => (Term_Record) ((Build_Record_) ("PathEquation"%string) ((cons) ((Build_Field) ("left"%string) ((path) ((fun r_ => (pathEquation_left) (r_)) (x)))) ((cons) ((Build_Field) ("right"%string) ((path) ((fun r_ => (pathEquation_right) (r_)) (x)))) (nil)))).
Definition triplePattern : TriplePattern -> Term :=
  fun (x : TriplePattern) => (Term_Record) ((Build_Record_) ("TriplePattern"%string) ((cons) ((Build_Field) ("subject"%string) ((node) ((fun r_ => (triplePattern_subject) (r_)) (x)))) ((cons) ((Build_Field) ("predicate"%string) ((path) ((fun r_ => (triplePattern_predicate) (r_)) (x)))) ((cons) ((Build_Field) ("object"%string) ((node) ((fun r_ => (triplePattern_object) (r_)) (x)))) (nil))))).
Definition pattern_graphPattern_bundle :=
  hydra_fix (fun (bundle_ : prod (Pattern -> Term) (GraphPattern -> Term)) =>
    let pattern := (fst bundle_) in
    let graphPattern := (snd bundle_) in
    (pair (fun x_ => match x_ with
| Pattern_Triple v_ => (fun (y : TriplePattern) => (Term_Inject) ((Build_Injection) ("Pattern"%string) ((Build_Field) ("triple"%string) ((triplePattern) (y))))) (v_)
| Pattern_Negation v_ => (fun (y : Pattern) => (Term_Inject) ((Build_Injection) ("Pattern"%string) ((Build_Field) ("negation"%string) ((pattern) (y))))) (v_)
| Pattern_Conjunction v_ => (fun (y : (list) (Pattern)) => (Term_Inject) ((Build_Injection) ("Pattern"%string) ((Build_Field) ("conjunction"%string) ((fun (xs : (list) (Pattern)) => (Term_List) (((lists.map) (pattern)) (xs))) (y))))) (v_)
| Pattern_Disjunction v_ => (fun (y : (list) (Pattern)) => (Term_Inject) ((Build_Injection) ("Pattern"%string) ((Build_Field) ("disjunction"%string) ((fun (xs : (list) (Pattern)) => (Term_List) (((lists.map) (pattern)) (xs))) (y))))) (v_)
| Pattern_Graph v_ => (fun (y : GraphPattern) => (Term_Inject) ((Build_Injection) ("Pattern"%string) ((Build_Field) ("graph"%string) ((graphPattern) (y))))) (v_)
end) (fun (x : GraphPattern) => (Term_Record) ((Build_Record_) ("GraphPattern"%string) ((cons) ((Build_Field) ("graph"%string) ((hydra.encode.core.name) ((fun r_ => (graphPattern_graph_) (r_)) (x)))) ((cons) ((Build_Field) ("patterns"%string) ((fun (xs : (list) (Pattern)) => (Term_List) (((lists.map) (pattern)) (xs))) ((fun r_ => (graphPattern_patterns) (r_)) (x)))) (nil))))))).

Definition pattern : Pattern -> Term :=
  (fst pattern_graphPattern_bundle).
Definition graphPattern : GraphPattern -> Term :=
  (snd pattern_graphPattern_bundle).
Definition patternImplication : PatternImplication -> Term :=
  fun (x : PatternImplication) => (Term_Record) ((Build_Record_) ("PatternImplication"%string) ((cons) ((Build_Field) ("antecedent"%string) ((pattern) ((fun r_ => (patternImplication_antecedent) (r_)) (x)))) ((cons) ((Build_Field) ("consequent"%string) ((pattern) ((fun r_ => (patternImplication_consequent) (r_)) (x)))) (nil)))).
Definition query : Query -> Term :=
  fun (x : Query) => (Term_Record) ((Build_Record_) ("Query"%string) ((cons) ((Build_Field) ("variables"%string) ((fun (xs : (list) (Variable_)) => (Term_List) (((lists.map) (variable)) (xs))) ((fun r_ => (query_variables) (r_)) (x)))) ((cons) ((Build_Field) ("patterns"%string) ((fun (xs : (list) (Pattern)) => (Term_List) (((lists.map) (pattern)) (xs))) ((fun r_ => (query_patterns) (r_)) (x)))) (nil)))).

