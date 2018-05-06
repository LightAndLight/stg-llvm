{-# language MultiParamTypeClasses, FlexibleContexts #-}
{-# language DeriveDataTypeable #-}
module Compile where

import Control.Exception (Exception, throw)
import Control.Monad.Reader (MonadReader)
import Data.Semigroup ((<>))
import LLVM.AST.AddrSpace (AddrSpace(..))
import LLVM.AST.Operand (Operand)
import LLVM.AST.Type (Type(..), ptr)
import LLVM.AST.Typed (typeOf)
import LLVM.IRBuilder.Constant (int32)
import LLVM.IRBuilder.Instruction (add, sub, load, gep, store)
import LLVM.IRBuilder.Monad (MonadIRBuilder)
import Type.Reflection (Typeable)

import qualified LLVM.AST.Type as LLVM

import Syntax

data TypeError
  = TypeError
  { functionName :: String
  , argument :: Int
  , expected :: String
  , actual :: String
  } deriving (Show, Typeable)

instance Exception TypeError

nodeRegType :: LLVM.Type
nodeRegType = ptr closureType

data Env
  = Env
  { nodeReg :: Operand
  , hpReg :: Operand
  , hlimitReg :: Operand
  , aStack :: Operand -- ptr (array A)
  , spA :: Operand -- ptr int32
  , bStack :: Operand -- ptr (array B)
  , spB :: Operand -- ptr int32
  } deriving (Eq, Show)

incr
  :: MonadIRBuilder m
  => Operand -- ^ : ptr int
  -> m Operand
incr addr =
  case typeOf addr of
    PointerType IntegerType{} _ -> do
      val <- load addr 0
      one <- int32 1
      val' <- add one val
      store addr 0 val'
      pure val'
    ty -> throw $ TypeError "incr" 1 "ptr(int)" (show ty)

decr
  :: MonadIRBuilder m
  => Operand -- ^ : ptr int
  -> m Operand
decr addr =
  case typeOf addr of
    PointerType IntegerType{} _ -> do
      val <- load addr 0
      one <- int32 1
      val' <- sub val one
      store addr 0 val'
      pure val'
    ty -> throw $ TypeError "decr" 1 "ptr(int)" (show ty)

-- | @peek sp stack@
-- Peeks at the value on the top of the stack
peekStack
  :: MonadIRBuilder m
  => Operand -- ^ ptr int
  -> Operand -- ^ ptr (array A)
  -> m Operand
peekStack sp stack =
  case typeOf sp of
    PointerType IntegerType{} _ ->
      case typeOf stack of
        PointerType ArrayType{} _ -> do
          val <- load sp 0
          zero <- int32 0
          addr <- gep stack [zero, val]
          load addr 0
        ty2 -> throw $ TypeError "peekStack" 2 "ptr(array(_))" (show ty2)
    ty1 -> throw $ TypeError "peekStack" 1 "ptr(int)" (show ty1)

-- | @push val sp stack@
-- Pushes @val@ onto @stack@, given that @sp@ points to the top of the stack
pushStack
  :: MonadIRBuilder m
  => Operand -- ^ A
  -> Operand -- ^ ptr int
  -> Operand -- ^ ptr (array A)
  -> m ()
pushStack val sp stack = do
  case typeOf sp of
    PointerType IntegerType{} _ ->
      case typeOf stack of
        PointerType (ArrayType _ itemTy) _
          | itemTy == typeOf val -> do
            sp' <- incr sp
            zero <- int32 0
            addr <- gep stack [zero, sp']
            store addr 0 val
          | otherwise ->
              throw $
              TypeError "pushStack" 2
                ("ptr(array(" <> show (typeOf val) <> "))")
                ("ptr(array(" <> show itemTy <> "))")
        ty2 -> throw $ TypeError "pushStack" 2 "ptr(array(_))" (show ty2)
    ty1 -> throw $ TypeError "pushStack" 1 "ptr(int)" (show ty1)

-- | @pop sp stack@
-- Pops the top value of the stack
popStack
  :: MonadIRBuilder m
  => Operand -- ^ ptr int
  -> Operand -- ^ ptr (array A)
  -> m Operand
popStack sp stack =
  case typeOf sp of
    PointerType IntegerType{} _ ->
      case typeOf stack of
        PointerType ArrayType{} _ -> do
          val <- peekStack sp stack
          _ <- decr sp
          pure val
        ty2 -> throw $ TypeError "popStack" 2 "ptr(array(_))" (show ty2)
    ty1 -> throw $ TypeError "popStack" 2 "ptr(int)" (show ty1)

infoTableType :: LLVM.Type
infoTableType =
  StructureType False
  [ LabelType -- entry code
  , LabelType -- evacuation code
  , LabelType -- scavenge code
  ]

closureType :: LLVM.Type
closureType =
  StructureType False
  [ ptr infoTableType
  , undefined -- pointer block
  , undefined -- non-pointer block
  ]

cgProgram
  :: ( MonadReader Env m
     , MonadIRBuilder m
     )
  => [Binding]
  -> m ()
cgProgram = undefined

cgExpr
  :: ( MonadReader Env m
     , MonadIRBuilder m
     )
  => Expr
  -> m ()
cgExpr (App f vs) = do
  undefined

