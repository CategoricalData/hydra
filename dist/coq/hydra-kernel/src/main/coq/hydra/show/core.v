(* String representations of hydra.core types *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.core hydra.lib.strings hydra.lib.pairs hydra.lib.maybes hydra.lib.lists hydra.lib.maps hydra.lib.sets hydra.lib.literals hydra.lib.logic hydra.lib.eithers.

Definition readTerm : forall (_ : string) , (option) (Term) := fun (s : string) => (Some) ((Term_Literal) ((Literal_String) (s))).
Definition projection : forall (_ : Projection) , string := fun (proj : Projection) => let tname := (fun w_ => w_) ((fun r_ => (projection_typeName) (r_)) (proj)) in let fname := (fun w_ => w_) ((fun r_ => (projection_field) (r_)) (proj)) in (strings.cat) ((cons) ("project("%string) ((cons) (tname) ((cons) ("){"%string) ((cons) (fname) ((cons) ("}"%string) (nil)))))).
Definition pair_ (t0 : Type) (t1 : Type) : forall (_ : forall (_ : t0) , string) , forall (_ : forall (_ : t1) , string) , forall (_ : (prod) (t0) (t1)) , string := fun (showA : forall (_ : t0) , string) => fun (showB : forall (_ : t1) , string) => fun (p : (prod) (t0) (t1)) => (strings.cat) ((cons) ("("%string) ((cons) ((showA) ((pairs.first) (p))) ((cons) (", "%string) ((cons) ((showB) ((pairs.second) (p))) ((cons) (")"%string) (nil)))))).
Arguments pair_ {t0} {t1}.
Definition maybe_bundle (t0 : Type) :=
  hydra_fix (fun (bundle_ : forall (_ : forall (_ : t0) , string) , forall (_ : (option) (t0)) , string) =>
    let maybe := bundle_ in
    fun (f : forall (_ : t0) , string) => fun (mx : (option) (t0)) => (((maybes.maybe) ("nothing"%string)) (fun (x : t0) => ((strings.cat2) ("just("%string)) (((strings.cat2) ((f) (x))) (")"%string)))) (mx)).
Arguments maybe_bundle {t0}.

Definition maybe (t0 : Type) : forall (_ : forall (_ : t0) , string) , forall (_ : (option) (t0)) , string :=
  maybe_bundle.
Arguments maybe {t0}.
Definition map_bundle (t0 : Type) (t1 : Type) :=
  hydra_fix (fun (bundle_ : forall (_ : forall (_ : t0) , string) , forall (_ : forall (_ : t1) , string) , forall (_ : (list) ((prod) (t0) (t1))) , string) =>
    let map := bundle_ in
    fun (showK : forall (_ : t0) , string) => fun (showV : forall (_ : t1) , string) => fun (m : (list) ((prod) (t0) (t1))) => let pairStrs := ((lists.map) (fun (p : (prod) (t0) (t1)) => (strings.cat) ((cons) ((showK) ((pairs.first) (p))) ((cons) (": "%string) ((cons) ((showV) ((pairs.second) (p))) (nil)))))) ((maps.toList) (m)) in (strings.cat) ((cons) ("{"%string) ((cons) (((strings.intercalate) (", "%string)) (pairStrs)) ((cons) ("}"%string) (nil))))).
Arguments map_bundle {t0} {t1}.

Definition map (t0 : Type) (t1 : Type) : forall (_ : forall (_ : t0) , string) , forall (_ : forall (_ : t1) , string) , forall (_ : (list) ((prod) (t0) (t1))) , string :=
  map_bundle.
Arguments map {t0} {t1}.
Definition set (t0 : Type) : forall (_ : forall (_ : t0) , string) , forall (_ : (list) (t0)) , string := fun (f : forall (_ : t0) , string) => fun (xs : (list) (t0)) => let elementStrs := ((lists.map) (f)) ((sets.toList) (xs)) in (strings.cat) ((cons) ("{"%string) ((cons) (((strings.intercalate) (", "%string)) (elementStrs)) ((cons) ("}"%string) (nil)))).
Arguments set {t0}.
Definition list_ (t0 : Type) : forall (_ : forall (_ : t0) , string) , forall (_ : (list) (t0)) , string := fun (f : forall (_ : t0) , string) => fun (xs : (list) (t0)) => let elementStrs := ((lists.map) (f)) (xs) in (strings.cat) ((cons) ("["%string) ((cons) (((strings.intercalate) (", "%string)) (elementStrs)) ((cons) ("]"%string) (nil)))).
Arguments list_ {t0}.
Definition integerType : forall (_ : IntegerType) , string := fun (it : IntegerType) => (fun x_ => match x_ with
| IntegerType_Bigint _ => "bigint"%string
| IntegerType_Int8 _ => "int8"%string
| IntegerType_Int16 _ => "int16"%string
| IntegerType_Int32 _ => "int32"%string
| IntegerType_Int64 _ => "int64"%string
| IntegerType_Uint8 _ => "uint8"%string
| IntegerType_Uint16 _ => "uint16"%string
| IntegerType_Uint32 _ => "uint32"%string
| IntegerType_Uint64 _ => "uint64"%string
end) (it).
Definition integer : forall (_ : IntegerValue) , string := fun (iv : IntegerValue) => (fun x_ => match x_ with
| IntegerValue_Bigint v_ => (fun (v : Z) => ((strings.cat2) ((literals.showBigint) (v))) (":bigint"%string)) (v_)
| IntegerValue_Int8 v_ => (fun (v : Z) => ((strings.cat2) ((literals.showInt8) (v))) (":int8"%string)) (v_)
| IntegerValue_Int16 v_ => (fun (v : Z) => ((strings.cat2) ((literals.showInt16) (v))) (":int16"%string)) (v_)
| IntegerValue_Int32 v_ => (fun (v : Z) => ((strings.cat2) ((literals.showInt32) (v))) (":int32"%string)) (v_)
| IntegerValue_Int64 v_ => (fun (v : Z) => ((strings.cat2) ((literals.showInt64) (v))) (":int64"%string)) (v_)
| IntegerValue_Uint8 v_ => (fun (v : Z) => ((strings.cat2) ((literals.showUint8) (v))) (":uint8"%string)) (v_)
| IntegerValue_Uint16 v_ => (fun (v : Z) => ((strings.cat2) ((literals.showUint16) (v))) (":uint16"%string)) (v_)
| IntegerValue_Uint32 v_ => (fun (v : Z) => ((strings.cat2) ((literals.showUint32) (v))) (":uint32"%string)) (v_)
| IntegerValue_Uint64 v_ => (fun (v : Z) => ((strings.cat2) ((literals.showUint64) (v))) (":uint64"%string)) (v_)
end) (iv).
Definition floatType : forall (_ : FloatType) , string := fun (ft : FloatType) => (fun x_ => match x_ with
| FloatType_Bigfloat _ => "bigfloat"%string
| FloatType_Float32 _ => "float32"%string
| FloatType_Float64 _ => "float64"%string
end) (ft).
Definition literalType : forall (_ : LiteralType) , string := fun (lt : LiteralType) => (fun x_ => match x_ with
| LiteralType_Binary _ => "binary"%string
| LiteralType_Boolean _ => "boolean"%string
| LiteralType_Float v_ => (fun (ft : FloatType) => (floatType) (ft)) (v_)
| LiteralType_Integer v_ => (fun (it : IntegerType) => (integerType) (it)) (v_)
| LiteralType_String _ => "string"%string
end) (lt).
Definition float : forall (_ : FloatValue) , string := fun (fv : FloatValue) => (fun x_ => match x_ with
| FloatValue_Bigfloat v_ => (fun (v : Q) => ((strings.cat2) ((literals.showBigfloat) (v))) (":bigfloat"%string)) (v_)
| FloatValue_Float32 v_ => (fun (v : Q) => ((strings.cat2) ((literals.showFloat32) (v))) (":float32"%string)) (v_)
| FloatValue_Float64 v_ => (fun (v : Q) => ((strings.cat2) ((literals.showFloat64) (v))) (":float64"%string)) (v_)
end) (fv).
Definition literal : forall (_ : Literal) , string := fun (l : Literal) => (fun x_ => match x_ with
| Literal_Binary v_ => (fun (_ : string) => "[binary]"%string) (v_)
| Literal_Boolean v_ => (fun (b : bool) => (((logic.ifElse) (b)) ("true"%string)) ("false"%string)) (v_)
| Literal_Float v_ => (fun (fv : FloatValue) => (float) (fv)) (v_)
| Literal_Integer v_ => (fun (iv : IntegerValue) => (integer) (iv)) (v_)
| Literal_String v_ => (fun (s : string) => (literals.showString) (s)) (v_)
end) (l).
Definition fieldType_type_bundle :=
  hydra_fix (fun (bundle_ : prod (forall (_ : FieldType) , string) (forall (_ : Type_) , string)) =>
    let fieldType := (fst bundle_) in
    let type := (snd bundle_) in
    (pair (fun (ft : FieldType) => let ftyp := (fun r_ => (fieldType_type) (r_)) (ft) in let fname := (fun w_ => w_) ((fun r_ => (fieldType_name) (r_)) (ft)) in (strings.cat) ((cons) (fname) ((cons) (":"%string) ((cons) ((type) (ftyp)) (nil))))) (fun (typ : Type_) => let showRowType := fun (flds : (list) (FieldType)) => let fieldStrs := ((lists.map) (fieldType)) (flds) in (strings.cat) ((cons) ("{"%string) ((cons) (((strings.intercalate) (", "%string)) (fieldStrs)) ((cons) ("}"%string) (nil)))) in let gatherTypes := (hydra_fix) (fun gatherTypes => fun (prev : (list) (Type_)) => fun (app : ApplicationType) => let rhs := (fun r_ => (applicationType_argument) (r_)) (app) in let lhs := (fun r_ => (applicationType_function) (r_)) (app) in (fun x_ => match x_ with
| Type__Application v_ => (fun (app2 : ApplicationType) => ((gatherTypes) (((lists.cons) (rhs)) (prev))) (app2)) (v_)
| _ => ((lists.cons) (lhs)) (((lists.cons) (rhs)) (prev))
end) (lhs)) in let gatherFunctionTypes := (hydra_fix) (fun gatherFunctionTypes => fun (prev : (list) (Type_)) => fun (t : Type_) => (fun x_ => match x_ with
| Type__Function v_ => (fun (ft : FunctionType) => let dom := (fun r_ => (functionType_domain) (r_)) (ft) in let cod := (fun r_ => (functionType_codomain) (r_)) (ft) in ((gatherFunctionTypes) (((lists.cons) (dom)) (prev))) (cod)) (v_)
| _ => (lists.reverse) (((lists.cons) (t)) (prev))
end) (t)) in (fun x_ => match x_ with
| Type__Annotated v_ => (fun (at_ : AnnotatedType) => (type) ((fun r_ => (annotatedType_body) (r_)) (at_))) (v_)
| Type__Application v_ => (fun (app : ApplicationType) => let types := ((gatherTypes) (nil)) (app) in let typeStrs := ((lists.map) (type)) (types) in (strings.cat) ((cons) ("("%string) ((cons) (((strings.intercalate) (" @ "%string)) (typeStrs)) ((cons) (")"%string) (nil))))) (v_)
| Type__Either v_ => (fun (et : EitherType) => let rightTyp := (fun r_ => (eitherType_right) (r_)) (et) in let leftTyp := (fun r_ => (eitherType_left) (r_)) (et) in (strings.cat) ((cons) ("either<"%string) ((cons) ((type) (leftTyp)) ((cons) (", "%string) ((cons) ((type) (rightTyp)) ((cons) (">"%string) (nil))))))) (v_)
| Type__Forall v_ => (fun (ft : ForallType) => let var := (fun w_ => w_) ((fun r_ => (forallType_parameter) (r_)) (ft)) in let body := (fun r_ => (forallType_body) (r_)) (ft) in (strings.cat) ((cons) ("(∀"%string) ((cons) (var) ((cons) ("."%string) ((cons) ((type) (body)) ((cons) (")"%string) (nil))))))) (v_)
| Type__Function v_ => (fun (ft : FunctionType) => let types := ((gatherFunctionTypes) (nil)) (typ) in let typeStrs := ((lists.map) (type)) (types) in (strings.cat) ((cons) ("("%string) ((cons) (((strings.intercalate) (" → "%string)) (typeStrs)) ((cons) (")"%string) (nil))))) (v_)
| Type__List v_ => (fun (etyp : Type_) => (strings.cat) ((cons) ("list<"%string) ((cons) ((type) (etyp)) ((cons) (">"%string) (nil))))) (v_)
| Type__Literal v_ => (fun (lt : LiteralType) => (literalType) (lt)) (v_)
| Type__Map v_ => (fun (mt : MapType) => let valTyp := (fun r_ => (mapType_values) (r_)) (mt) in let keyTyp := (fun r_ => (mapType_keys) (r_)) (mt) in (strings.cat) ((cons) ("map<"%string) ((cons) ((type) (keyTyp)) ((cons) (", "%string) ((cons) ((type) (valTyp)) ((cons) (">"%string) (nil))))))) (v_)
| Type__Maybe v_ => (fun (etyp : Type_) => (strings.cat) ((cons) ("maybe<"%string) ((cons) ((type) (etyp)) ((cons) (">"%string) (nil))))) (v_)
| Type__Pair v_ => (fun (pt : PairType) => let secondTyp := (fun r_ => (pairType_second) (r_)) (pt) in let firstTyp := (fun r_ => (pairType_first) (r_)) (pt) in (strings.cat) ((cons) ("("%string) ((cons) ((type) (firstTyp)) ((cons) (", "%string) ((cons) ((type) (secondTyp)) ((cons) (")"%string) (nil))))))) (v_)
| Type__Record v_ => (fun (rt : (list) (FieldType)) => ((strings.cat2) ("record"%string)) ((showRowType) (rt))) (v_)
| Type__Set v_ => (fun (etyp : Type_) => (strings.cat) ((cons) ("set<"%string) ((cons) ((type) (etyp)) ((cons) (">"%string) (nil))))) (v_)
| Type__Union v_ => (fun (rt : (list) (FieldType)) => ((strings.cat2) ("union"%string)) ((showRowType) (rt))) (v_)
| Type__Unit _ => "unit"%string
| Type__Variable v_ => (fun (name : Name) => (fun w_ => w_) (name)) (v_)
| Type__Void _ => "void"%string
| Type__Wrap v_ => (fun (wt : Type_) => (strings.cat) ((cons) ("wrap("%string) ((cons) ((type) (wt)) ((cons) (")"%string) (nil))))) (v_)
end) (typ)))).

Definition fieldType : forall (_ : FieldType) , string :=
  (fst fieldType_type_bundle).
Definition type : forall (_ : Type_) , string :=
  (snd fieldType_type_bundle).
Definition typeScheme : forall (_ : TypeScheme) , string := fun (ts : TypeScheme) => let vars := (fun r_ => (typeScheme_variables) (r_)) (ts) in let varNames := ((lists.map) (fun w_ => w_)) (vars) in let toConstraintPair := fun (v : Name) => fun (c : Name) => (strings.cat) ((cons) ((fun w_ => w_) (c)) ((cons) (" "%string) ((cons) ((fun w_ => w_) (v)) (nil)))) in let toConstraintPairs := fun (p : (prod) (Name) (TypeVariableMetadata)) => ((lists.map) ((toConstraintPair) ((pairs.first) (p)))) ((sets.toList) ((fun r_ => (typeVariableMetadata_classes) (r_)) ((pairs.second) (p)))) in let tc := (((maybes.maybe) (nil)) (fun (m : (list) ((prod) (Name) (TypeVariableMetadata))) => (lists.concat) (((lists.map) (toConstraintPairs)) ((maps.toList) (m))))) ((fun r_ => (typeScheme_constraints) (r_)) (ts)) in let fa := (((logic.ifElse) ((lists.null) (vars))) (""%string)) ((strings.cat) ((cons) ("forall "%string) ((cons) (((strings.intercalate) (","%string)) (varNames)) ((cons) (". "%string) (nil))))) in let body := (fun r_ => (typeScheme_type) (r_)) (ts) in (strings.cat) ((cons) ("("%string) ((cons) (fa) ((cons) ((((logic.ifElse) ((lists.null) (tc))) (""%string)) ((strings.cat) ((cons) ("("%string) ((cons) (((strings.intercalate) (", "%string)) (tc)) ((cons) (") => "%string) (nil)))))) ((cons) ((type) (body)) ((cons) (")"%string) (nil)))))).
Definition either_bundle (t0 : Type) (t1 : Type) :=
  hydra_fix (fun (bundle_ : forall (_ : forall (_ : t0) , string) , forall (_ : forall (_ : t1) , string) , forall (_ : (sum) (t0) (t1)) , string) =>
    let either := bundle_ in
    fun (showA : forall (_ : t0) , string) => fun (showB : forall (_ : t1) , string) => fun (e : (sum) (t0) (t1)) => (((eithers.either) (fun (a : t0) => ((strings.cat2) ("left("%string)) (((strings.cat2) ((showA) (a))) (")"%string)))) (fun (b : t1) => ((strings.cat2) ("right("%string)) (((strings.cat2) ((showB) (b))) (")"%string)))) (e)).
Arguments either_bundle {t0} {t1}.

Definition either (t0 : Type) (t1 : Type) : forall (_ : forall (_ : t0) , string) , forall (_ : forall (_ : t1) , string) , forall (_ : (sum) (t0) (t1)) , string :=
  either_bundle.
Arguments either {t0} {t1}.
Definition binding_term_bundle :=
  hydra_fix (fun (bundle_ : prod (forall (_ : Binding) , string) (prod (forall (_ : Term) , string) (prod (forall (_ : CaseStatement) , string) (prod (forall (_ : (list) (Field)) , string) (prod (forall (_ : Field) , string) (prod (forall (_ : Injection) , string) (prod (forall (_ : Lambda) , string) (forall (_ : Let) , string)))))))) =>
    let binding := (fst bundle_) in
    let term := (fst (snd bundle_)) in
    let caseStatement := (fst (snd (snd bundle_))) in
    let fields := (fst (snd (snd (snd bundle_)))) in
    let field := (fst (snd (snd (snd (snd bundle_))))) in
    let injection := (fst (snd (snd (snd (snd (snd bundle_)))))) in
    let lambda := (fst (snd (snd (snd (snd (snd (snd bundle_))))))) in
    let let_ := (snd (snd (snd (snd (snd (snd (snd bundle_))))))) in
    (pair (fun (el : Binding) => let typeStr := (((maybes.maybe) (""%string)) (fun (ts : TypeScheme) => (strings.cat) ((cons) (":("%string) ((cons) ((typeScheme) (ts)) ((cons) (")"%string) (nil)))))) ((fun r_ => (binding_type) (r_)) (el)) in let t := (fun r_ => (binding_term) (r_)) (el) in let name := (fun w_ => w_) ((fun r_ => (binding_name) (r_)) (el)) in (strings.cat) ((cons) (name) ((cons) (typeStr) ((cons) (" = "%string) ((cons) ((term) (t)) (nil)))))) ((pair (fun (t : Term) => let gatherTerms := (hydra_fix) (fun gatherTerms => fun (prev : (list) (Term)) => fun (app : Application) => let rhs := (fun r_ => (application_argument) (r_)) (app) in let lhs := (fun r_ => (application_function) (r_)) (app) in (fun x_ => match x_ with
| Term_Application v_ => (fun (app2 : Application) => ((gatherTerms) (((lists.cons) (rhs)) (prev))) (app2)) (v_)
| _ => ((lists.cons) (lhs)) (((lists.cons) (rhs)) (prev))
end) (lhs)) in (fun x_ => match x_ with
| Term_Annotated v_ => (fun (at_ : AnnotatedTerm) => (term) ((fun r_ => (annotatedTerm_body) (r_)) (at_))) (v_)
| Term_Application v_ => (fun (app : Application) => let terms := ((gatherTerms) (nil)) (app) in let termStrs := ((lists.map) (term)) (terms) in (strings.cat) ((cons) ("("%string) ((cons) (((strings.intercalate) (" @ "%string)) (termStrs)) ((cons) (")"%string) (nil))))) (v_)
| Term_Cases v_ => (caseStatement) (v_)
| Term_Either v_ => (fun (e : (sum) (Term) (Term)) => (((eithers.either) (fun (l : Term) => (strings.cat) ((cons) ("left("%string) ((cons) ((term) (l)) ((cons) (")"%string) (nil)))))) (fun (r : Term) => (strings.cat) ((cons) ("right("%string) ((cons) ((term) (r)) ((cons) (")"%string) (nil)))))) (e)) (v_)
| Term_Lambda v_ => (lambda) (v_)
| Term_Let v_ => (fun (l : Let) => (let_) (l)) (v_)
| Term_List v_ => (fun (els : (list) (Term)) => let termStrs := ((lists.map) (term)) (els) in (strings.cat) ((cons) ("["%string) ((cons) (((strings.intercalate) (", "%string)) (termStrs)) ((cons) ("]"%string) (nil))))) (v_)
| Term_Literal v_ => (fun (lit : Literal) => (literal) (lit)) (v_)
| Term_Map v_ => (fun (m : (list) ((prod) (Term) (Term))) => let entry := fun (p : (prod) (Term) (Term)) => (strings.cat) ((cons) ((term) ((pairs.first) (p))) ((cons) ("="%string) ((cons) ((term) ((pairs.second) (p))) (nil)))) in (strings.cat) ((cons) ("{"%string) ((cons) (((strings.intercalate) (", "%string)) (((lists.map) (entry)) ((maps.toList) (m)))) ((cons) ("}"%string) (nil))))) (v_)
| Term_Maybe v_ => (fun (mt : (option) (Term)) => (((maybes.maybe) ("nothing"%string)) (fun (t2 : Term) => (strings.cat) ((cons) ("just("%string) ((cons) ((term) (t2)) ((cons) (")"%string) (nil)))))) (mt)) (v_)
| Term_Pair v_ => (fun (p : (prod) (Term) (Term)) => (strings.cat) ((cons) ("("%string) ((cons) ((term) ((pairs.first) (p))) ((cons) (", "%string) ((cons) ((term) ((pairs.second) (p))) ((cons) (")"%string) (nil))))))) (v_)
| Term_Project v_ => (projection) (v_)
| Term_Record v_ => (fun (rec : Record_) => let tname := (fun w_ => w_) ((fun r_ => (record__typeName) (r_)) (rec)) in let flds := (fun r_ => (record__fields) (r_)) (rec) in (strings.cat) ((cons) ("record("%string) ((cons) (tname) ((cons) (")"%string) ((cons) ((fields) (flds)) (nil)))))) (v_)
| Term_Set v_ => (fun (s : (list) (Term)) => (strings.cat) ((cons) ("{"%string) ((cons) (((strings.intercalate) (", "%string)) (((lists.map) (term)) ((sets.toList) (s)))) ((cons) ("}"%string) (nil))))) (v_)
| Term_TypeLambda v_ => (fun (ta : TypeLambda) => let param := (fun w_ => w_) ((fun r_ => (typeLambda_parameter) (r_)) (ta)) in let body := (fun r_ => (typeLambda_body) (r_)) (ta) in (strings.cat) ((cons) ("Λ"%string) ((cons) (param) ((cons) ("."%string) ((cons) ((term) (body)) (nil)))))) (v_)
| Term_TypeApplication v_ => (fun (tt : TypeApplicationTerm) => let typ := (fun r_ => (typeApplicationTerm_type) (r_)) (tt) in let t2 := (fun r_ => (typeApplicationTerm_body) (r_)) (tt) in (strings.cat) ((cons) ((term) (t2)) ((cons) ("⟨"%string) ((cons) ((type) (typ)) ((cons) ("⟩"%string) (nil)))))) (v_)
| Term_Inject v_ => (injection) (v_)
| Term_Unit _ => "unit"%string
| Term_Unwrap v_ => (fun (tname : Name) => (strings.cat) ((cons) ("unwrap("%string) ((cons) ((fun w_ => w_) (tname)) ((cons) (")"%string) (nil))))) (v_)
| Term_Variable v_ => (fun (name : Name) => (fun w_ => w_) (name)) (v_)
| Term_Wrap v_ => (fun (wt : WrappedTerm) => let tname := (fun w_ => w_) ((fun r_ => (wrappedTerm_typeName) (r_)) (wt)) in let term1 := (fun r_ => (wrappedTerm_body) (r_)) (wt) in (strings.cat) ((cons) ("wrap("%string) ((cons) (tname) ((cons) ("){"%string) ((cons) ((term) (term1)) ((cons) ("}"%string) (nil))))))) (v_)
end) (t)) ((pair (fun (cs : CaseStatement) => let tname := (fun w_ => w_) ((fun r_ => (caseStatement_typeName) (r_)) (cs)) in let mdef := (fun r_ => (caseStatement_default) (r_)) (cs) in let defaultField := (((maybes.maybe) (nil)) (fun (d : Term) => (cons) ((Build_Field) ("[default]"%string) (d)) (nil))) (mdef) in let csCases := (fun r_ => (caseStatement_cases) (r_)) (cs) in let allFields := (lists.concat) ((cons) (csCases) ((cons) (defaultField) (nil))) in (strings.cat) ((cons) ("case("%string) ((cons) (tname) ((cons) (")"%string) ((cons) ((fields) (allFields)) (nil)))))) ((pair (fun (flds : (list) (Field)) => let fieldStrs := ((lists.map) (field)) (flds) in (strings.cat) ((cons) ("{"%string) ((cons) (((strings.intercalate) (", "%string)) (fieldStrs)) ((cons) ("}"%string) (nil))))) ((pair (fun (field : Field) => let fterm := (fun r_ => (field_term) (r_)) (field) in let fname := (fun w_ => w_) ((fun r_ => (field_name) (r_)) (field)) in (strings.cat) ((cons) (fname) ((cons) ("="%string) ((cons) ((term) (fterm)) (nil))))) ((pair (fun (inj : Injection) => let tname := (fun r_ => (injection_typeName) (r_)) (inj) in let f := (fun r_ => (injection_field) (r_)) (inj) in (strings.cat) ((cons) ("inject("%string) ((cons) ((fun w_ => w_) (tname)) ((cons) (")"%string) ((cons) ((fields) ((cons) (f) (nil))) (nil)))))) ((pair (fun (l : Lambda) => let v := (fun w_ => w_) ((fun r_ => (lambda_parameter) (r_)) (l)) in let mt := (fun r_ => (lambda_domain) (r_)) (l) in let typeStr := (((maybes.maybe) (""%string)) (fun (t : Type_) => ((strings.cat2) (":"%string)) ((type) (t)))) (mt) in let body := (fun r_ => (lambda_body) (r_)) (l) in (strings.cat) ((cons) ("λ"%string) ((cons) (v) ((cons) (typeStr) ((cons) ("."%string) ((cons) ((term) (body)) (nil))))))) (fun (l : Let) => let env := (fun r_ => (let_body) (r_)) (l) in let bindings := (fun r_ => (let_bindings) (r_)) (l) in let bindingStrs := ((lists.map) (binding)) (bindings) in (strings.cat) ((cons) ("let "%string) ((cons) (((strings.intercalate) (", "%string)) (bindingStrs)) ((cons) (" in "%string) ((cons) ((term) (env)) (nil)))))))))))))))))))).

Definition binding : forall (_ : Binding) , string :=
  (fst binding_term_bundle).
Definition term : forall (_ : Term) , string :=
  (fst (snd binding_term_bundle)).
Definition caseStatement : forall (_ : CaseStatement) , string :=
  (fst (snd (snd binding_term_bundle))).
Definition fields : forall (_ : (list) (Field)) , string :=
  (fst (snd (snd (snd binding_term_bundle)))).
Definition field : forall (_ : Field) , string :=
  (fst (snd (snd (snd (snd binding_term_bundle))))).
Definition injection : forall (_ : Injection) , string :=
  (fst (snd (snd (snd (snd (snd binding_term_bundle)))))).
Definition lambda : forall (_ : Lambda) , string :=
  (fst (snd (snd (snd (snd (snd (snd binding_term_bundle))))))).
Definition let_ : forall (_ : Let) , string :=
  (snd (snd (snd (snd (snd (snd (snd binding_term_bundle))))))).

