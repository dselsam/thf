module Export where

import qualified Statement as Statement
import Formula
--import Formula (Formula)

export_statements :: [Statement.Statement] -> [String]
export_statements statements = "import sledgehammer_prelude" : (map export_statement statements)

purify_filename :: String -> String
purify_filename filename = let repl '/' = '.'
                               repl c = c in
                           map repl filename

export_statement :: Statement.Statement -> String
export_statement s = case s of
  Statement.Comment -> ""
  Statement.Include filename _ -> "include " ++ purify_filename filename
  Statement.AnnotatedFormula name (Statement.Role role) formula
    | role == "lemma" || role == "theorem" -> "axiom " ++ " " ++ name ++ " : " ++ export_formula formula
    | role == "conjecture" -> "theorem " ++ name ++ " : " ++ export_formula formula ++ " := sorry" -- "by blast"
    | role == "type" -> "constant " ++ export_formula formula
    | True -> "<undefined-role: " ++ role ++ "> " ++ name ++ " : " ++ export_formula formula
--error("undefined role " ++ role)

export_formula :: Formula -> String
export_formula formula = case formula of
  OrFormula f1 f2 -> "(or (" ++ export_formula f1 ++ ") (" ++ export_formula f2 ++ "))"
  AndFormula f1 f2 -> "(and (" ++ export_formula f1 ++ ") (" ++ export_formula f2 ++ "))"
  ApplyFormula f1 f2 -> "((" ++ export_formula f1 ++ ") (" ++ export_formula f2 ++ "))"
  QuantifiedFormula q vs f -> export_quantified_formula q vs f
  BinaryFormula f1 op f2 -> "(" ++ export_formula op ++ " (" ++ export_formula f1 ++ ") (" ++ export_formula f2 ++ "))"
  UnaryFormula op f -> export_formula op ++ " " ++ export_formula f
  Conditional c t e -> "(if (" ++ export_formula c ++ ") then (" ++ export_formula t ++ ") else (" ++ export_formula e ++ "))"
  TypeFormula f1 f2 -> export_formula f1 ++ " : " ++ export_formula f2
  Mapping f1 f2 -> "((" ++ export_formula f1 ++ ") -> (" ++ export_formula f2 ++ "))"
  Union f1 f2 -> "(sum (" ++ export_formula f1 ++ ") (" ++ export_formula f2 ++ "))"
  XProd f1 f2 -> "(prod (" ++ export_formula f1 ++ ") (" ++ export_formula f2 ++ "))" -- TODO is unicode in Lean
  Operator o -> export_operator o
  Constant s -> export_constant s
  Variable v -> export_var v
  Number r -> show(r)
  Subtype s1 s2 -> error("SUBTYPE")
  Object s -> error("Object")
  Functor s fs -> error("Functor")

export_quantified_formula :: Quantifier -> [Var] -> Formula -> String
export_quantified_formula q vs f = "(" ++ export_quantifier q ++ " " ++ export_vars vs ++ ", " ++ export_formula f ++ ")"

export_quantifier :: Quantifier -> String
export_quantifier q = case q of
  Q_Exclam -> "forall"
  Q_Question -> "exists"
  Q_Carrot -> "fun"
  Q_Pi -> "forall"
  Q_Sigma -> "sigma"
  Q_Mystery3 -> error("@+") -- indefinite description
  Q_Mystery4 -> error("@-") -- definite description

export_vars :: [Var] -> String
export_vars vs = concat (map export_binder vs)

export_var :: Var -> String
export_var v = case v of
 TypedVar s f -> s
 UntypedVar s -> s

export_binder :: Var -> String
export_binder v = case v of
 TypedVar s f -> "(" ++ s ++ " : " ++ export_formula f ++ ")"
 UntypedVar s -> "(" ++ s ++ ")"

export_constant :: String -> String
export_constant s
 | s == "$o" = "Prop"
 | s == "$tType" = "Type"
 | s == "$true" = "true"
 | s == "$false" = "false"
 | head s == '$' = tail s
 | True = s

export_operator :: OperatorType -> String
export_operator o = case o of
  IFF -> "iff"
  Implies -> "implies"
  NOT -> "not"
  EQQ -> "eq"
  NEQQ -> "neq"
  PI -> "forall"
  EX -> "exists"
  OR -> "or"
  AND -> "and"
  _ -> show o

export_role :: Statement.Role -> String
export_role (Statement.Role role)
 | role == "lemma" = "lemma"
 | role == "theorem" = "lemma"
 | role == "conjecture" = "theorem" -- We use the convention that theorems need to be proved by blast
 | True = error("undefined role " ++ role)
