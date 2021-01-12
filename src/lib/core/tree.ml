
(** Location trees

    This represents a trees of locations relevant to represent in
    a structured way the sets of locations where inlining decisions
    can occur.

    There are two points where one can make decision about
    inlining:
    - during the definition of a function, some computation
      can take place to determine whether it should be inlined
      or not. And thus a decision can be made (note however that
      it is not required/mandatory to make a decision at that
      point).
      This is typically what Closure does.
      Flambda1 does not perform any computation at that point
      however.
    - at a function call, it must be decided whether to inline
      the call or not, so there will always be a decision.

    Additionally, in both cases, there is also a set of
    "sub"-decisions that can be associated:
    - in the case of a function declaration, the body of the
      function may contain decisions
    - in the case of a function call, **if** the call has been
      inlined, the inlined code can itself contain decision
      about inlining.

    To handle such cases, i.e. a set of decisions about
    a piece of code (by opposition to a single decision about
    a particular function call/declaration), we index the
    decisions by their debuginfo, resulting in a map from
    debuginfos to decisions.
*)

(* Type definitions *)
(* ************************************************************************* *)

type ('call, 'clos) node =
  | Call of {
      decision : 'call;
      inlined : ('call, 'clos) t option;
      specialized : ('call, 'clos) t option;
    }
  (**)
  | Closure of {
      decision : 'clos;
      body : ('call, 'clos) t;
    }
  (**)
(** A decision at a particular point in code
    (and the tree of sub-decision rooted at that point). *)

and ('call, 'clos) t = ('call, 'clos) node Debuginfo.Map.t
(** A set of decisions, indexed by debuginfo. *)


