module Exceptions

open Common
type Signal =
| DuplicateInstanceError of Core.evidence_instance array
| InstanceNotFound of HM.t
| UnboundTypeVariable of symbol
| UnboundVariable of symbol
| UnificationFail of HM.t * HM.t
| UnsolvedTypeVariables of HM.t
| UnusedAnnotation of symbol
| MalformedTypeClassKind of HM.t
| InvalidNamespaceType of HM.t
| RecursiveDefinition of symbol
| UndefinedSymbolInIR of symbol
| ModuleNotProvided of this:symbol * requiredModule:symbol

with
  override this.ToString() =
    match this with
    | InstanceNotFound t ->
        sprintf "  Instance not found for %O" t
    | UnboundVariable s ->
        sprintf "  Unbound variable %s" s
    | UnboundTypeVariable s ->
        sprintf "  Unbound type variable %s" s
    | DuplicateInstanceError xs ->
        sprintf "  Duplicate instances found:\n%s"
        <| String.concat "\n" (Array.map (sprintf "  - %O") xs)
    | UnificationFail(lhs, rhs) ->
        sprintf "  Unification failed:\n      %O\n    ~\n      %O" lhs rhs
    | UnsolvedTypeVariables t ->
        sprintf "  Type variables unsolved:\n     %O\n    Try adding explicit annotations." t
    | UnusedAnnotation s ->
        sprintf "  Unused type annotations for %s" s
    | MalformedTypeClassKind t ->
        sprintf "  Malformed type class kind for\n:     %O" t
    | InvalidNamespaceType t ->
        sprintf "  Invalid namespace type:\n     %O" t
    | RecursiveDefinition s ->
        let i = s.LastIndexOf '.'
        let user_sym = s.[i..]
        sprintf "  Recursively define %s, check your definition again." user_sym
    | UndefinedSymbolInIR s ->
        let i = s.LastIndexOf '.'
        let user_sym = s.[i..]
        sprintf "  Undefined symbol %s in your ir files, did you provide all ir files required?" user_sym
    | ModuleNotProvided(thisModule, requiredModule) ->
        sprintf "%s is not provided for compiling %s" requiredModule thisModule
exception InferError of pos * Signal
