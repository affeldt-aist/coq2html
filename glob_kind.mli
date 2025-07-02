type kind =
  | Axiom (* Axiom, Parameter or Variable(s), Hypothes,es, Context outside any section *)
  | Definition
  | Coertion
  | Theorem
  | SubClass
  | CanonicalDeclaration
  | Example
  | Scheme
  | ClassDeclaration
  | ProjectionFromAStructure
  | Instance
  | ClassMethod
  | DefinitionalAssumption
  | LogicalAssumption
  | Primitive
  | SectionVariableReference (* Variable,s, Hypothes,es, Context *)
  | Inductive
  | InductiveVariant
  | Coinductive
  | Record
  | RecordVariant
  | CoinductiveRecord
  | Constructor
  | Notation
  | Binder
  | Require
  | ModuleReference (* Import, Module start / end *)
  | ModuleType
  | Other of string

type t = kind

val of_string : string -> kind
val to_string : kind -> string
