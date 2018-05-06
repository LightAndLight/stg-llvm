module Syntax where

newtype Var = Var String deriving (Eq, Show)
newtype Constr = Constr String deriving (Eq, Show)

data Binding
  = Binding
  { bindingName :: Var
  , bindingBody :: Lambda
  }
  deriving (Eq, Show)

data UpdateFlag = U | N deriving (Eq, Show)

data Lambda
  = Lambda
  { lambdaFrees :: [Var]
  , lambdaUpdateFlag :: UpdateFlag
  , lambdaArgs :: [Var]
  , lambdaBody :: Expr
  }
  deriving (Eq, Show)

data Expr
  = Let [Binding] Expr
  | LetRec [Binding] Expr
  | Case Expr Alts
  | App Var [Atom]
  | Ctor Constr [Atom]
  | Prim Prim [Atom]
  | Literal Literal
  deriving (Eq, Show)

data Alts
  = Alts [Alt]
  | PAlts [PAlt]
  deriving (Eq, Show)

data Alt = Alt Constr [Var] Expr
  deriving (Eq, Show)

data PAlt = PAlt Literal Expr
  deriving (Eq, Show)

data Literal = LitInt Int deriving (Eq, Show)

data Atom = AtomVar Var | AtomLit Literal deriving (Eq, Show)

data Prim = PrimAdd | PrimSubtract | PrimMultiply deriving (Eq, Show)
