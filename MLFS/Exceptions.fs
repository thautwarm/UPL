module Exceptions

open Common
type Signal =
| DuplicateInstanceError of Core.evidence_instance array
| InstanceNotFound of HM.t
| UnboundTypeVariable of symbol
| UnboundVariable of symbol
| UnificationFail of HM.t * HM.t
| UnsolvedTypeVariables of HM.t
| UnboundVar of symbol
| UnusedAnnotation of symbol
| MalformedTypeClassKind of HM.t
| InvalidNamespaceType of HM.t
exception InferError of pos * Signal
