Fixpoint add (a b : nat) : nat :=
  match a with
  | O => b
  | S n => S (add n b)
  end.

Fixpoint pred (n : nat) : nat :=
  match n with
  | O => O
  | S n' => n'
  end.

Theorem zero_is_not_greater_than_zero : ~ 0 > 0.
Proof.
  unfold not.
  unfold gt.
  unfold lt.
  intros H.
  inversion H.
Qed.

Definition pred_arg_is_greater_than_zero (n : nat) : n > 0 -> nat :=
  match n with
  | O => fun pf => match (zero_is_not_greater_than_zero pf) with
    (* match on all constructors of false, which is none *) end
  | S n' => fun _ => n'
  end.


Theorem add_is_associative: forall (a b c : nat),
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

Theorem negate: ~ 1 = 2.
Proof.
  unfold not.
  intros H.
  inversion H.
Admitted.
