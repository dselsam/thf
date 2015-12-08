module Formula where

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
             | XProd Formula Formula
             | Operator OperatorType
             | Constant String
             | Variable Var
             | Number Rational
             | Object String
             | Functor String [Formula]
             deriving (Show)

data OperatorType = IFF | Implies | ImpliedBy | XOR | NOR | NAND | NOT | EQQ | NEQQ | PI | EX | OR | AND
                  deriving (Show)

data Quantifier = Q_Exclam
                | Q_Question
                | Q_Carrot
                | Q_Pi
                | Q_Sigma
                | Q_Mystery3
                | Q_Mystery4
                deriving (Show)

data Var = TypedVar String Formula
         | UntypedVar String
         deriving (Show)
