





(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below *)
Require Import BuiltIn.
Require BuiltIn.
Require HighOrd.
Require int.Int.
Require bool.Bool.
Require map.Map.

Axiom us_private : Type.
Parameter us_private_WhyType : WhyType us_private.
Existing Instance us_private_WhyType.

(* Why3 assumption *)
Definition us_fixed := Numbers.BinNums.Z.

Parameter private__bool_eq: us_private -> us_private -> Init.Datatypes.bool.

Parameter us_null_ext__: us_private.

Axiom us_type_of_heap : Type.
Parameter us_type_of_heap_WhyType : WhyType us_type_of_heap.
Existing Instance us_type_of_heap_WhyType.

(* Why3 assumption *)
Inductive us_type_of_heap__ref :=
  | us_type_of_heap__ref'mk : us_type_of_heap -> us_type_of_heap__ref.
Axiom us_type_of_heap__ref_WhyType : WhyType us_type_of_heap__ref.
Existing Instance us_type_of_heap__ref_WhyType.

(* Why3 assumption *)
Definition us_type_of_heap__content (v:us_type_of_heap__ref) :
    us_type_of_heap :=
  match v with
  | us_type_of_heap__ref'mk x => x
  end.

Axiom us_image : Type.
Parameter us_image_WhyType : WhyType us_image.
Existing Instance us_image_WhyType.

(* Why3 assumption *)
Inductive int__ref :=
  | int__ref'mk : Numbers.BinNums.Z -> int__ref.
Axiom int__ref_WhyType : WhyType int__ref.
Existing Instance int__ref_WhyType.

(* Why3 assumption *)
Definition int__content (v:int__ref) : Numbers.BinNums.Z :=
  match v with
  | int__ref'mk x => x
  end.

(* Why3 assumption *)
Inductive bool__ref :=
  | bool__ref'mk : Init.Datatypes.bool -> bool__ref.
Axiom bool__ref_WhyType : WhyType bool__ref.
Existing Instance bool__ref_WhyType.

(* Why3 assumption *)
Definition bool__content (v:bool__ref) : Init.Datatypes.bool :=
  match v with
  | bool__ref'mk x => x
  end.

(* Why3 assumption *)
Inductive us_fixed__ref :=
  | us_fixed__ref'mk : Numbers.BinNums.Z -> us_fixed__ref.
Axiom us_fixed__ref_WhyType : WhyType us_fixed__ref.
Existing Instance us_fixed__ref_WhyType.

(* Why3 assumption *)
Definition us_fixed__content (v:us_fixed__ref) : Numbers.BinNums.Z :=
  match v with
  | us_fixed__ref'mk x => x
  end.

(* Why3 assumption *)
Inductive real__ref :=
  | real__ref'mk : Reals.Rdefinitions.R -> real__ref.
Axiom real__ref_WhyType : WhyType real__ref.
Existing Instance real__ref_WhyType.

(* Why3 assumption *)
Definition real__content (v:real__ref) : Reals.Rdefinitions.R :=
  match v with
  | real__ref'mk x => x
  end.

(* Why3 assumption *)
Inductive us_private__ref :=
  | us_private__ref'mk : us_private -> us_private__ref.
Axiom us_private__ref_WhyType : WhyType us_private__ref.
Existing Instance us_private__ref_WhyType.

(* Why3 assumption *)
Definition us_private__content (v:us_private__ref) : us_private :=
  match v with
  | us_private__ref'mk x => x
  end.

(* Why3 assumption *)
Definition int__ref___projection (a:int__ref) : Numbers.BinNums.Z :=
  int__content a.

(* Why3 assumption *)
Definition us_fixed__ref___projection (a:us_fixed__ref) : Numbers.BinNums.Z :=
  us_fixed__content a.

(* Why3 assumption *)
Definition bool__ref___projection (a:bool__ref) : Init.Datatypes.bool :=
  bool__content a.

(* Why3 assumption *)
Definition real__ref___projection (a:real__ref) : Reals.Rdefinitions.R :=
  real__content a.

(* Why3 assumption *)
Definition us_private__ref___projection (a:us_private__ref) : us_private :=
  us_private__content a.

(* Why3 assumption *)
Definition ite {a:Type} {a_WT:WhyType a} (b:Init.Datatypes.bool) (x:a)
    (y:a) : a :=
  match b with
  | Init.Datatypes.true => x
  | Init.Datatypes.false => y
  end.

Parameter bool_eq:
  Init.Datatypes.bool -> Init.Datatypes.bool -> Init.Datatypes.bool.

Axiom bool_eq'def :
  forall (x:Init.Datatypes.bool) (y:Init.Datatypes.bool),
  ((x = y) -> ((bool_eq x y) = Init.Datatypes.true)) /\ (* non escaped EOL for cpp parsing *)
  (~ (x = y) -> ((bool_eq x y) = Init.Datatypes.false)).

Parameter to_int: Init.Datatypes.bool -> Numbers.BinNums.Z.

Axiom to_int'def :
  forall (b:Init.Datatypes.bool),
  ((b = Init.Datatypes.true) -> ((to_int b) = 1%Z)) /\ (* non escaped EOL for cpp parsing *)
  (~ (b = Init.Datatypes.true) -> ((to_int b) = 0%Z)).

Parameter of_int: Numbers.BinNums.Z -> Init.Datatypes.bool.

Axiom of_int'def :
  forall (i:Numbers.BinNums.Z),
  ((i = 0%Z) -> ((of_int i) = Init.Datatypes.false)) /\ (* non escaped EOL for cpp parsing *)
  (~ (i = 0%Z) -> ((of_int i) = Init.Datatypes.true)).

(* Why3 assumption *)
Definition in_range (x:Numbers.BinNums.Z) : Prop := (x = 0%Z) \/ (x = 1%Z).

Parameter attr__ATTRIBUTE_IMAGE: Init.Datatypes.bool -> us_image.

Parameter attr__ATTRIBUTE_VALUE__pre_check: us_image -> Prop.

Parameter attr__ATTRIBUTE_VALUE: us_image -> Init.Datatypes.bool.

Axiom integer : Type.
Parameter integer_WhyType : WhyType integer.
Existing Instance integer_WhyType.

Parameter integer'int: integer -> Numbers.BinNums.Z.

Axiom integer'axiom :
  forall (i:integer),
  ((-2147483648%Z)%Z <= (integer'int i))%Z /\ (* non escaped EOL for cpp parsing *)
  ((integer'int i) <= 2147483647%Z)%Z.

(* Why3 assumption *)
Definition in_range1 (x:Numbers.BinNums.Z) : Prop :=
  ((-2147483648%Z)%Z <= x)%Z /\ (x <= 2147483647%Z)%Z.

Parameter bool_eq1:
  Numbers.BinNums.Z -> Numbers.BinNums.Z -> Init.Datatypes.bool.

Axiom bool_eq'def1 :
  forall (x:Numbers.BinNums.Z) (y:Numbers.BinNums.Z),
  ((x = y) -> ((bool_eq1 x y) = Init.Datatypes.true)) /\ (* non escaped EOL for cpp parsing *)
  (~ (x = y) -> ((bool_eq1 x y) = Init.Datatypes.false)).

Parameter attr__ATTRIBUTE_IMAGE1: Numbers.BinNums.Z -> us_image.

Parameter attr__ATTRIBUTE_VALUE__pre_check1: us_image -> Prop.

Parameter attr__ATTRIBUTE_VALUE1: us_image -> Numbers.BinNums.Z.

Parameter user_eq: integer -> integer -> Init.Datatypes.bool.

Parameter dummy: integer.

(* Why3 assumption *)
Inductive integer__ref :=
  | integer__ref'mk : integer -> integer__ref.
Axiom integer__ref_WhyType : WhyType integer__ref.
Existing Instance integer__ref_WhyType.

(* Why3 assumption *)
Definition integer__content (v:integer__ref) : integer :=
  match v with
  | integer__ref'mk x => x
  end.

(* Why3 assumption *)
Definition integer__ref_integer__content__projection (a:integer__ref) :
    integer :=
  integer__content a.

(* Why3 assumption *)
Definition to_rep (x:integer) : Numbers.BinNums.Z := integer'int x.

Parameter of_rep: Numbers.BinNums.Z -> integer.

Axiom inversion_axiom : forall (x:integer), ((of_rep (to_rep x)) = x).

Axiom range_axiom : forall (x:integer), in_range1 (to_rep x).

Axiom coerce_axiom :
  forall (x:Numbers.BinNums.Z), in_range1 x -> ((to_rep (of_rep x)) = x).

(* Why3 assumption *)
Definition component_type := integer.

(* Why3 assumption *)
Definition map := Numbers.BinNums.Z -> integer.

(* Why3 assumption *)
Inductive map__ref :=
  | map__ref'mk : (Numbers.BinNums.Z -> integer) -> map__ref.
Axiom map__ref_WhyType : WhyType map__ref.
Existing Instance map__ref_WhyType.

(* Why3 assumption *)
Definition map__content (v:map__ref) : Numbers.BinNums.Z -> integer :=
  match v with
  | map__ref'mk x => x
  end.

Parameter slide:
  (Numbers.BinNums.Z -> integer) -> Numbers.BinNums.Z -> Numbers.BinNums.Z ->
  Numbers.BinNums.Z -> integer.

Axiom slide_eq :
  forall (a:Numbers.BinNums.Z -> integer), forall (first:Numbers.BinNums.Z),
  ((slide a first first) = a).

Axiom slide_def :
  forall (a:Numbers.BinNums.Z -> integer),
  forall (old_first:Numbers.BinNums.Z), forall (new_first:Numbers.BinNums.Z),
  forall (i:Numbers.BinNums.Z),
  ((slide a old_first new_first i) = (a (i - (new_first - old_first)%Z)%Z)).

Parameter bool_eq2:
  (Numbers.BinNums.Z -> integer) -> Numbers.BinNums.Z -> Numbers.BinNums.Z ->
  (Numbers.BinNums.Z -> integer) -> Numbers.BinNums.Z -> Numbers.BinNums.Z ->
  Init.Datatypes.bool.

Axiom bool_eq'def2 :
  forall (a:Numbers.BinNums.Z -> integer) (a__first:Numbers.BinNums.Z)
    (a__last:Numbers.BinNums.Z) (b:Numbers.BinNums.Z -> integer)
    (b__first:Numbers.BinNums.Z) (b__last:Numbers.BinNums.Z),
  (((a__first <= a__last)%Z /\ (* non escaped EOL for cpp parsing *)
    (b__first <= b__last)%Z /\ (* non escaped EOL for cpp parsing *)
    ((a__last - a__first)%Z = (b__last - b__first)%Z) \/
    ~ (a__first <= a__last)%Z /\ (b__last < b__first)%Z) /\ (* non escaped EOL for cpp parsing *)
   (forall (temp___idx_161:Numbers.BinNums.Z),
    ((a__first <= temp___idx_161)%Z /\ (temp___idx_161 <= a__last)%Z) /\ (* non escaped EOL for cpp parsing *)
    ((to_rep (a temp___idx_161)) =
     (to_rep (b ((b__first - a__first)%Z + temp___idx_161)%Z))) \/
    ~ ((a__first <= temp___idx_161)%Z /\ (temp___idx_161 <= a__last)%Z)) ->
   ((bool_eq2 a a__first a__last b b__first b__last) = Init.Datatypes.true)) /\ (* non escaped EOL for cpp parsing *)
  (~ ((((a__first <= a__last)%Z ->
        (b__first <= b__last)%Z /\ (* non escaped EOL for cpp parsing *)
        ((a__last - a__first)%Z = (b__last - b__first)%Z)) /\ (* non escaped EOL for cpp parsing *)
       (~ (a__first <= a__last)%Z -> (b__last < b__first)%Z)) /\ (* non escaped EOL for cpp parsing *)
      (forall (temp___idx_161:Numbers.BinNums.Z),
       (a__first <= temp___idx_161)%Z /\ (temp___idx_161 <= a__last)%Z ->
       ((to_rep (a temp___idx_161)) =
        (to_rep (b ((b__first - a__first)%Z + temp___idx_161)%Z))))) ->
   ((bool_eq2 a a__first a__last b b__first b__last) = Init.Datatypes.false)).

Axiom bool_eq_rev :
  forall (a:Numbers.BinNums.Z -> integer) (b:Numbers.BinNums.Z -> integer),
  forall (a__first:Numbers.BinNums.Z) (a__last:Numbers.BinNums.Z)
    (b__first:Numbers.BinNums.Z) (b__last:Numbers.BinNums.Z),
  ((bool_eq2 b b__first b__last a a__first a__last) = Init.Datatypes.true) ->
  (((a__first <= a__last)%Z ->
    (b__first <= b__last)%Z /\ (* non escaped EOL for cpp parsing *)
    ((a__last - a__first)%Z = (b__last - b__first)%Z)) /\ (* non escaped EOL for cpp parsing *)
   (~ (a__first <= a__last)%Z -> (b__last < b__first)%Z)) /\ (* non escaped EOL for cpp parsing *)
  (forall (temp___idx_161:Numbers.BinNums.Z),
   (a__first <= temp___idx_161)%Z /\ (temp___idx_161 <= a__last)%Z ->
   ((to_rep (a temp___idx_161)) =
    (to_rep (b ((b__first - a__first)%Z + temp___idx_161)%Z)))).

(* Why3 assumption *)
Definition component_type1 := integer.

Axiom t : Type.
Parameter t_WhyType : WhyType t.
Existing Instance t_WhyType.

Parameter first: t -> integer.

Parameter last: t -> integer.

Parameter mk: Numbers.BinNums.Z -> Numbers.BinNums.Z -> t.

Axiom mk_def :
  forall (f:Numbers.BinNums.Z) (l:Numbers.BinNums.Z), in_range1 f ->
  in_range1 l ->
  ((to_rep (first (mk f l))) = f) /\ ((to_rep (last (mk f l))) = l).

(* Why3 assumption *)
Definition dynamic_property (range_first:Numbers.BinNums.Z)
    (range_last:Numbers.BinNums.Z) (low:Numbers.BinNums.Z)
    (high:Numbers.BinNums.Z) : Prop :=
  in_range1 low /\ (* non escaped EOL for cpp parsing *)
  in_range1 high /\ ((low <= high)%Z -> in_range1 low /\ in_range1 high).

(* Why3 assumption *)
Inductive us_t :=
  | us_t'mk : (Numbers.BinNums.Z -> integer) -> t -> us_t.
Axiom us_t_WhyType : WhyType us_t.
Existing Instance us_t_WhyType.

(* Why3 assumption *)
Definition rt (v:us_t) : t := match v with
                              | us_t'mk x x1 => x1
                              end.

(* Why3 assumption *)
Definition elts (v:us_t) : Numbers.BinNums.Z -> integer :=
  match v with
  | us_t'mk x x1 => x
  end.

(* Why3 assumption *)
Definition to_array (a:us_t) : Numbers.BinNums.Z -> integer := elts a.

(* Why3 assumption *)
Definition of_array (a:Numbers.BinNums.Z -> integer) (f:Numbers.BinNums.Z)
    (l:Numbers.BinNums.Z) : us_t :=
  us_t'mk a (mk f l).

(* Why3 assumption *)
Definition first1 (a:us_t) : Numbers.BinNums.Z := to_rep (first (rt a)).

(* Why3 assumption *)
Definition last1 (a:us_t) : Numbers.BinNums.Z := to_rep (last (rt a)).

Parameter length: us_t -> Numbers.BinNums.Z.

Axiom length'def :
  forall (a:us_t),
  (((first1 a) <= (last1 a))%Z ->
   ((length a) = (((last1 a) - (first1 a))%Z + 1%Z)%Z)) /\ (* non escaped EOL for cpp parsing *)
  (~ ((first1 a) <= (last1 a))%Z -> ((length a) = 0%Z)).

Parameter value__size: Numbers.BinNums.Z.

Parameter object__size: Numbers.BinNums.Z.

Parameter component__size: Numbers.BinNums.Z.

Parameter alignment: Numbers.BinNums.Z.

Axiom value__size_axiom : (0%Z <= value__size)%Z.

Axiom object__size_axiom : (0%Z <= object__size)%Z.

Axiom component__size_axiom : (0%Z <= component__size)%Z.

Axiom alignment_axiom : (0%Z <= alignment)%Z.

(* Why3 assumption *)
Definition bool_eq3 (x:us_t) (y:us_t) : Init.Datatypes.bool :=
  bool_eq2 (elts x) (to_rep (first (rt x))) (to_rep (last (rt x))) (elts y)
  (to_rep (first (rt y))) (to_rep (last (rt y))).

Parameter user_eq1: us_t -> us_t -> Init.Datatypes.bool.

Parameter dummy1: us_t.

(* Why3 assumption *)
Definition arr_int_unconstrained := us_t.

(* Why3 assumption *)
Inductive arr_int_unconstrained__ref :=
  | arr_int_unconstrained__ref'mk : us_t -> arr_int_unconstrained__ref.
Axiom arr_int_unconstrained__ref_WhyType : WhyType arr_int_unconstrained__ref.
Existing Instance arr_int_unconstrained__ref_WhyType.

(* Why3 assumption *)
Definition arr_int_unconstrained__content (v:arr_int_unconstrained__ref) :
    us_t :=
  match v with
  | arr_int_unconstrained__ref'mk x => x
  end.

(* Why3 assumption *)
Definition arr_int_unconstrained__ref_arr_int_unconstrained__content__projection
    (a:arr_int_unconstrained__ref) : us_t :=
  arr_int_unconstrained__content a.

Parameter arr: us_t.

Parameter attr__ATTRIBUTE_ADDRESS: Numbers.BinNums.Z.

(* Why3 assumption *)
Definition dynamic_invariant (temp___expr_196:us_t)
    (temp___is_init_192:Init.Datatypes.bool)
    (temp___skip_constant_193:Init.Datatypes.bool)
    (temp___do_toplevel_194:Init.Datatypes.bool)
    (temp___do_typ_inv_195:Init.Datatypes.bool) : Prop :=
  ~ (temp___skip_constant_193 = Init.Datatypes.true) ->
  dynamic_property (-2147483648%Z)%Z 2147483647%Z (first1 temp___expr_196)
  (last1 temp___expr_196).

(* Why3 assumption *)
Definition dynamic_invariant1 (temp___expr_18:Numbers.BinNums.Z)
    (temp___is_init_14:Init.Datatypes.bool)
    (temp___skip_constant_15:Init.Datatypes.bool)
    (temp___do_toplevel_16:Init.Datatypes.bool)
    (temp___do_typ_inv_17:Init.Datatypes.bool) : Prop :=
  (temp___is_init_14 = Init.Datatypes.true) \/
  ((-2147483648%Z)%Z <= 2147483647%Z)%Z -> in_range1 temp___expr_18.

(* Why3 goal *)
Theorem def'vc :
  dynamic_invariant arr Init.Datatypes.true Init.Datatypes.false
  Init.Datatypes.true Init.Datatypes.true ->
  (forall (i:Numbers.BinNums.Z),
   ((first1 arr) <= i)%Z /\ (i <= (last1 arr))%Z ->
   ~ (i = (first1 arr)) /\ (* non escaped EOL for cpp parsing *)
   ((to_rep (to_array arr (i - 1%Z)%Z)) < (to_rep (to_array arr i)))%Z \/
   (i = (first1 arr))) ->
  forall (i:Numbers.BinNums.Z),
  ((first1 arr) <= i)%Z /\ (i <= (last1 arr))%Z ->
  forall (j:Numbers.BinNums.Z),
  ((first1 arr) <= j)%Z /\ (j <= (last1 arr))%Z -> (i < j)%Z ->
  ((to_rep (to_array arr i)) < (to_rep (to_array arr j)))%Z.
Proof.
intros h1 h2 i (h3,h4) j (h5,h6) h7.

Require Import SPARK.

assert (Htrans: Relation_Definitions.transitive _ (fun (x y: integer) => (to_rep x) < (to_rep y))).
{
  intro; intros.

  (* Apply transitivity to prove transitive property *)
  transitivity (to_rep y); auto.
}

eapply (generic_raising_order_minus _ _ Htrans _); eauto; intuition.
(* Conclude *)
destruct (h2 i0) as [Hconcl | H3]; eauto.
- destruct Hconcl; eauto.
- subst. contradict H. omega.

Qed.
