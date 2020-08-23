module Exceptions

open Common
type Signal =
| DuplicateInstanceError of Core.evidence_instance array
| InstanceNotFound of HM.t
| UnboundTypeVariable of symbol
| UnificationFail of HM.t * HM.t
exception InferError of pos * Signal