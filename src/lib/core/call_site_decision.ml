
type attribute_causing_inlining =
  | Unroll
  | Always

type decision =
  | Environment_says_never_inline
  | Unrolling_depth_exceeded
  | Max_inlining_depth_exceeded
  | Recursion_depth_exceeded
  | Never_inline_attribute
  | Inline of {
      attribute : attribute_causing_inlining option;
      unroll_to : int option;
    }

type t =
  | Unknown_function
  | Non_inlinable_function of {
      code_id : Code_id.t;
    }
  | Inlinable_function of {
      code_id : Code_id.t;
      decision : decision;
    }


