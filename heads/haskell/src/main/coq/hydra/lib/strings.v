(* Hydra primitive library: hydra.lib.strings *)

Require Import Stdlib.Strings.String Stdlib.Lists.List Stdlib.ZArith.ZArith Stdlib.QArith.QArith.
Require Import Stdlib.Strings.Ascii.
Require Import hydra.lib.base.
Import ListNotations.

Open Scope string_scope.

(* Helper: get the nth character of a string (0-indexed), returning option Ascii. *)
Fixpoint string_nth (n : nat) (s : string) : option Ascii.ascii :=
  match s with
  | EmptyString => None
  | String c rest =>
    match n with
    | O => Some c
    | S n' => string_nth n' rest
    end
  end.

(* Helper: convert an Ascii character to its Z code point. *)
Definition ascii_to_Z (c : Ascii.ascii) : Z :=
  Z.of_N (Ascii.N_of_ascii c).

(* Helper: convert a Z code point to an Ascii character (truncates to 8 bits). *)
Definition Z_to_ascii (z : Z) : Ascii.ascii :=
  Ascii.ascii_of_N (Z.to_N z).

(* Helper: check if two ascii characters are equal. *)
Definition ascii_eqb (a b : Ascii.ascii) : bool :=
  N.eqb (Ascii.N_of_ascii a) (Ascii.N_of_ascii b).

(* Helper: check if an ascii character is within a range. *)
Definition ascii_in_range (lo hi : N) (c : Ascii.ascii) : bool :=
  let n := Ascii.N_of_ascii c in
  andb (N.leb lo n) (N.leb n hi).

(* Helper: convert uppercase ASCII letter to lowercase. *)
Definition ascii_toLower (c : Ascii.ascii) : Ascii.ascii :=
  if ascii_in_range 65 90 c
  then Ascii.ascii_of_N (Ascii.N_of_ascii c + 32)%N
  else c.

(* Helper: convert lowercase ASCII letter to uppercase. *)
Definition ascii_toUpper (c : Ascii.ascii) : Ascii.ascii :=
  if ascii_in_range 97 122 c
  then Ascii.ascii_of_N (Ascii.N_of_ascii c - 32)%N
  else c.

(* Helper: map a function over each character of a string. *)
Fixpoint string_map (f : Ascii.ascii -> Ascii.ascii) (s : string) : string :=
  match s with
  | EmptyString => EmptyString
  | String c rest => String (f c) (string_map f rest)
  end.

(* Helper: check if string s starts with prefix p, returning the remainder if so. *)
Fixpoint string_starts_with (prefix s : string) : option string :=
  match prefix with
  | EmptyString => Some s
  | String pc prest =>
    match s with
    | EmptyString => None
    | String sc srest =>
      if ascii_eqb pc sc then string_starts_with prest srest else None
    end
  end.

(* Helper: is the delimiter empty? *)
Definition string_is_empty (s : string) : bool :=
  match s with
  | EmptyString => true
  | _ => false
  end.

(* Helper: split a string on a single-character delimiter. *)
Fixpoint split_on_char (delim : Ascii.ascii) (s : string) : list string :=
  match s with
  | EmptyString => [""%string]
  | String c rest =>
    if ascii_eqb c delim
    then EmptyString :: split_on_char delim rest
    else
      match split_on_char delim rest with
      | [] => [String c EmptyString]
      | h :: t => (String c h) :: t
      end
  end.

(* Helper: split on a multi-character delimiter using fuel. *)
Fixpoint split_on_aux (delim : string) (s : string) (acc : string) (fuel : nat) : list string :=
  match fuel with
  | O => [acc]
  | S fuel' =>
    match s with
    | EmptyString => [acc]
    | _ =>
      match string_starts_with delim s with
      | Some rest => acc :: split_on_aux delim rest EmptyString fuel'
      | None =>
        match s with
        | EmptyString => [acc]
        | String c rest => split_on_aux delim rest (String.append acc (String c EmptyString)) fuel'
        end
      end
    end
  end.

(* Helper: reverse a string. *)
Fixpoint string_rev_aux (s acc : string) : string :=
  match s with
  | EmptyString => acc
  | String c rest => string_rev_aux rest (String c acc)
  end.

Definition string_rev (s : string) : string := string_rev_aux s EmptyString.

(* Helper: string length as nat. *)
Fixpoint string_length_nat (s : string) : nat :=
  match s with
  | EmptyString => O
  | String _ rest => S (string_length_nat rest)
  end.

Definition cat (ss : list string) : string :=
  List.fold_left String.append ss EmptyString.

Definition cat2 (s1 s2 : string) : string :=
  String.append s1 s2.

Definition charAt (i : Z) (s : string) : Z :=
  match string_nth (Z.to_nat i) s with
  | Some c => ascii_to_Z c
  | None => hydra_unreachable
  end.

Definition fromList (cps : list Z) : string :=
  List.fold_right (fun z acc => String (Z_to_ascii z) acc) EmptyString cps.

Definition intercalate (sep : string) (ss : list string) : string :=
  match ss with
  | [] => EmptyString
  | h :: t => List.fold_left (fun acc s => String.append (String.append acc sep) s) t h
  end.

Definition length (s : string) : Z :=
  Z.of_nat (String.length s).

Definition lines (s : string) : list string :=
  split_on_char "010"%char s.

Definition maybeCharAt (i : Z) (s : string) : option Z :=
  match string_nth (Z.to_nat i) s with
  | Some c => Some (ascii_to_Z c)
  | None => None
  end.

Definition null (s : string) : bool :=
  string_is_empty s.

Definition splitOn (delim s : string) : list string :=
  split_on_aux delim s EmptyString (string_length_nat s).

Definition toList (s : string) : list Z :=
  List.map ascii_to_Z (list_ascii_of_string s).

Definition toLower (s : string) : string :=
  string_map ascii_toLower s.

Definition toUpper (s : string) : string :=
  string_map ascii_toUpper s.

Definition unlines (ss : list string) : string :=
  cat (List.map (fun s => String.append s (String "010"%char EmptyString)) ss).
