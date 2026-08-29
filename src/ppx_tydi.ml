open Base
open Ppxlib

let extension =
  let pattern
    : ( payload
        , (pattern * expression * value_constraint option) list
          -> expression
          -> expression
        , expression )
        Ast_pattern.t
    =
    let open Ast_pattern in
    single_expr_payload
      (pexp_let __ (many (pack3 (value_binding ~pat:__ ~expr:__ ~constraint_:__))) __)
    |> map2 ~f:(fun rec_flag bindings ->
      match rec_flag with
      | Recursive -> Location.raise_errorf "[let rec] is not supported."
      | Nonrecursive -> bindings)
  in
  Extension.declare "tydi" Expression pattern (fun ~loc ~path:_ bindings rhs ->
    let open (val Ast_builder.make loc) in
    let patterns, expressions =
      List.map bindings ~f:(fun (pat, expr, constraint_) ->
        (* [let%tydi PAT : TY = EXPR] relies on type-directed disambiguation to
           resolve PAT's record-field labels from TY.  In OCaml 5.x the binding's
           type annotation lives in [pvb_constraint], separate from the pattern, so
           we must re-attach it to the pattern as [(PAT : TY)] — otherwise the
           annotation is lost and fields like [item] are reported unbound. *)
        let pat =
          match constraint_ with
          | Some (Pvc_constraint { typ; locally_abstract_univars = _ }) ->
            ppat_constraint pat typ
          | Some (Pvc_coercion _) | None -> pat
        in
        pat, expr)
      |> List.unzip
    in
    pexp_match
      (pexp_tuple expressions)
      [ case ~lhs:(ppat_tuple patterns) ~guard:None ~rhs ])
;;

let () =
  Driver.register_transformation "tydi" ~rules:[ Context_free.Rule.extension extension ]
;;
