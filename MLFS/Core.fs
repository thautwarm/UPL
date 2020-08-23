module Core

open Common
open CamlCompat
// for type class resolution
// this record holds a variable(field 'exp') with the info of
// 1. pos: where it's defined(source code position)
// 2. t: what the type
// 3. isPruned:is the type pruned
// variable naming convention:
// evi, inst, ei, *_ei, *_evi, *_inst
type evidence_instance
    = { t   : HM.t;
        pos : pos;
        isPruned : bool
      }

// instance resolution context.
// it's local context.
// global evidence instances are stored somewhere
// else, whose type is 
//    '(classKind, evidence_instance array) map'
// , for the performance concern
type inst_resolv_ctx = evidence_instance list

let arrow_class = HM.TNom "arrow_class"
let general_class = HM.TNom "general_class"

// to speed up instance resolution
let rec get_type_head =
  function
  | HM.TNom _ as h -> h
  | HM.TApp(t, _)
  | HM.TForall(_, t)
  | HM.TArrow(HM.TImplicit _, t) ->
  get_type_head t
  | HM.TArrow(_, _) -> arrow_class
  | _ -> general_class

// type check global state
type global_st =
  { tctsate : HMUnification.tcstate
  // for global resolution
  ; global_implicits : (HM.t, evidence_instance darray) dict
  // for type holes and hints
  ; queries : (string * HM.t) darray
  // for gensym
  ; count : int ref
  // for gensym's uniqueness
  ; mutable current_module_name : string
  }

let empty_global_st () = 
  { tctsate = HMUnification.mk_tcstate <| darray()
  ; global_implicits = dict()
  ; queries = darray()
  ; count = ref 0
  ; current_module_name = "main"
  }


type local_st =
  { type_env : (symbol, HM.t) map
  // mapping user symbols to actual symbols
  ; symmap : (symbol, symbol) map
  // for local resolution
  ; local_implicits : inst_resolv_ctx
  // current position
  ; pos : pos
  }

let empty_local_st =
  { type_env = Map.empty
  ; symmap = Map.empty
  ; local_implicits = []
  ; pos = {line = 1; col = 1; filename = "<unknown>"}
  }  


let alphabeta = [|for i = 'a' to 'z' do yield i|]

let gensym (g: global_st) =
  let mutable i = !g.count + 1 in
  g.count := i + 1;
  let chars = darray() in
  while i <> 0 do
    let j = i % 26 in
    DArray.push chars <| alphabeta.[j]
    i <- i / 26
  done;
  // System.String.Concat : char iterable -> string
  System.String.Concat chars
