open Common

(* see: https://rocq-prover.org/doc/V8.20.0/api/coq-core/Dumpglob/index.html *)

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

let of_string = function
  | "ax"     -> Axiom
  | "def"    -> Definition
  | "coe"    -> Coertion
  | "thm"    -> Theorem
  | "subclass" -> SubClass
  | "canonstruc" -> CanonicalDeclaration
  | "ex"     -> Example
  | "scheme" -> Scheme
  | "class"  -> ClassDeclaration
  | "proj"   -> ProjectionFromAStructure
  | "inst"   -> Instance
  | "meth"   -> ClassMethod
  | "defax"  -> DefinitionalAssumption
  | "prfax"  -> LogicalAssumption
  | "prim"   -> Primitive
  | "var"    -> SectionVariableReference
  | "indrec" -> Inductive
  | "rec"    -> InductiveVariant
  | "corec"  -> Coinductive
  | "ind"    -> Record
  | "variant" -> RecordVariant
  | "coind"  -> CoinductiveRecord
  | "constr" -> Constructor
  | "not"    -> Notation
  | "binder" -> Binder
  | "lib"    -> Require
  | "mod"    -> ModuleReference
  | "modtype" -> ModuleType
  | "abbrev" -> Other "abbrev"
  | "sec" -> Other "sec"
  | "prf" -> Other "prf"
  | "abbrev" -> Other "abbrev"
  | "vardef" -> Other "vardef"
  | "vardefax" -> Other "vardefax"
  | other ->
     warn (!%"unknown kind: '%s'" other);
     Other other

let to_string = function
  | Axiom      -> "ax"
  | Definition -> "def"
  | Coertion   -> "coe"
  | Theorem    -> "thm"
  | SubClass   -> "subclass"
  | CanonicalDeclaration -> "canonstruc"
  | Example    -> "ex"
  | Scheme     -> "scheme"
  | ClassDeclaration -> "class"
  | ProjectionFromAStructure -> "proj"
  | Instance   -> "inst"
  | ClassMethod -> "meth"
  | DefinitionalAssumption -> "defax"
  | LogicalAssumption -> "prfax"
  | Primitive  -> "prim"
  | SectionVariableReference -> "var"
  | Inductive  -> "indrec"
  | InductiveVariant -> "rec"
  | Coinductive -> "corec"
  | Record     -> "ind"
  | RecordVariant -> "variant"
  | CoinductiveRecord -> "coind"
  | Constructor -> "constr"
  | Notation   -> "not"
  | Binder     -> "binder"
  | Require    -> "lib"
  | ModuleReference -> "mod"
  | ModuleType -> "modtype"
  | Other "sec" -> "sec"
  | Other "prf" -> "prf"
  | Other "abbrev" -> "abbrev"
  | Other "vardef" -> "vardef"
  | Other "vardefax" -> "vardefax"
  | Other other ->
     warn (!%"unknown kind: '%s'" other);
     other
