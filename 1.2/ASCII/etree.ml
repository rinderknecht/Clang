type t =
  Or  of t * t
| And of t * t
| Eq  of t * t
| Neq of t * t
| Not of t
| True
| False
| Ident of string
