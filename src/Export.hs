module Export where

import qualified Statement as Statement
import qualified Formula as THF

export_statement :: [Statement.Statement] -> String
export_statements statements = mapM export_statement statements

export_statement :: Statement.Statement -> String
export_statement s = case s of
  Statement.Include _ _ -> ""
  Statement.AnnotatedFormula name role formula -> export_formula formula -- TODO use name and role
  Statement.Comment -> ""
  
export_formula :: THF.Formula -> String
export_formula formula = "" -- TODO


{-
data Formula = OrFormula Formula Formula
             | AndFormula Formula Formula
             | ApplyFormula Formula Formula
             | QuantifiedFormula Quantifier [Var] Formula
             | BinaryFormula Formula Formula Formula
             | UnaryFormula Formula Formula
             | Conditional Formula Formula Formula
             | TypeFormula Formula Formula
             | Subtype String String
             | Mapping Formula Formula
             | Union Formula Formula
             | XProp Formula Formula
             | Operator OperatorType
             | Constant String
             | Variable Var
             | Number Rational
             | Object String
             | Functor String [Formula]
             deriving (Show)

data OperatorType = IFF | Implies | ImpliedBy | XOR | NOR | NAND | NOT | EQ | NEQ | PI | EX | OR | AND
                  deriving (Show)

data Quantifier = Q_Exclam
                | Q_Question
                | Q_Carrot
                | Q_Mystery1
                | Q_Mystery2
                | Q_Mystery3
                | Q_Mystery4
                deriving (Show)

data Var = TypedVar String Formula
         | UntypedVar String
         deriving (Show)
-}
