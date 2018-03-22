Inductive nat : Set :=
  | O : nat
  | S : nat -> nat.

Eval compute in O.
Eval compute in (S O).

Fixpoint add (a b : nat) : nat :=
  match a with
  | O => b
  | S n => S (add n b)
  end.

Theorem add_is_assiciative: forall (a b c : nat),
  (add a (add b c)) = (add (add a b) c).
Proof.
  intros a b c.
  induction a.
  simpl.
  reflexivity.
  simpl.
  rewrite IHa.
  reflexivity.
Qed.
