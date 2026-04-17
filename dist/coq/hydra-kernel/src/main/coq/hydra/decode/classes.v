(* Term decoders for hydra.classes *)

(* Standard library imports *)
Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith hydra.lib.base.

(* Module dependencies *)
Require Import hydra.classes hydra.core hydra.errors hydra.extract.core hydra.graph hydra.lib.eithers hydra.lib.maps hydra.lib.maybes hydra.lib.strings.

Definition typeClass : forall (_ : hydra.graph.Graph) , forall (_ : Term) , (sum) (DecodingError) (TypeClass) := fun (cx : hydra.graph.Graph) => fun (raw : Term) => (((eithers.either) (fun (err : DecodingError) => ((inl) (err)) : (sum) (DecodingError) (TypeClass))) (fun (stripped : Term) => (fun x_ => match x_ with
| Term_Inject v_ => (fun (inj : Injection) => let field := (fun r_ => (injection_field) (r_)) (inj) in let fname := (fun r_ => (field_name) (r_)) (field) in let fterm := (fun r_ => (field_term) (r_)) (field) in let variantMap := (maps.fromList) ((cons) ((pair) ("equality"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (TypeClass_Equality) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) ((cons) ((pair) ("ordering"%string) (fun (input : Term) => ((eithers.map) (fun (t : unit) => (TypeClass_Ordering) (t))) (((fun (cx2 : hydra.graph.Graph) => fun (t : Term) => ((decodeUnit) (cx2)) (t)) (cx)) (input)))) (nil))) in (((maybes.maybe) (((inl) ((strings.cat) ((cons) ("no such field "%string) ((cons) ((fun w_ => w_) (fname)) ((cons) (" in union"%string) (nil)))))) : (sum) (DecodingError) (TypeClass))) (fun (f : forall (_ : Term) , (sum) (DecodingError) (TypeClass)) => (f) (fterm))) (((maps.lookup) (fname)) (variantMap))) (v_)
| _ => ((inl) ("expected union"%string)) : (sum) (DecodingError) (TypeClass)
end) (stripped))) (((stripWithDecodingError) (cx)) (raw)).

