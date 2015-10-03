module Formula where

data Formula = OrFormula Formula Formula
             | AndFormula Formula Formula
             | ApplyFormula Formula Formula
             | QuantifiedFormula Quantifier [Var] Formula
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
             | Functor String [Formula]
             | TODO
             deriving (Show)

data OperatorType = IFF | Implies | ImpliedBy | XOR | NOR | NAND | NOT | EQ | NEQ
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
