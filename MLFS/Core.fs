module Core
// position
// variable naming convention:
// pos, pos', position, *_pos, etc
type pos = { line : int; col : int}

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
