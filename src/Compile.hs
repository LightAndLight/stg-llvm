{-# language MultiParamTypeClasses, FlexibleContexts #-}
{-# language DeriveDataTypeable #-}
module Compile where

import Control.Exception (Exception, throw)
import Control.Monad.Reader (MonadReader, asks)
import Data.Foldable (for_, toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup ((<>))
import Data.Word (Word32)
import LLVM.AST.AddrSpace (AddrSpace(..))
import LLVM.AST.Constant (Constant(Undef))
import LLVM.AST.Instruction (Instruction(..), TailCallKind(..))
import LLVM.AST.Operand (Operand(..))
import LLVM.AST.ParameterAttribute (ParameterAttribute)
import LLVM.AST.Type (Type(..), ptr, void)
import LLVM.AST.Typed (typeOf)
import LLVM.IRBuilder.Constant (int32)
import LLVM.IRBuilder.Instruction (add, sub, load, gep, store, retVoid)
import LLVM.IRBuilder.Monad (MonadIRBuilder, emitInstr, emitInstrVoid)
import Type.Reflection (Typeable)

import qualified Data.List.NonEmpty as NonEmpty
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.CallingConvention as CC

import Syntax

data TypeError
  = TypeError
  { functionName :: String
  , argument :: Int
  , expected :: String
  , actual :: String
  } deriving (Show, Typeable)

instance Exception TypeError

tailcall :: MonadIRBuilder m => Operand -> [(Operand, [ParameterAttribute])] -> m Operand
tailcall fun as = do
  let instr = Call {
    tailCallKind = Just MustTail
  , callingConvention = CC.C
  , returnAttributes = []
  , function = Right fun
  , arguments = as
  , functionAttributes = []
  , metadata = []
  }
  case typeOf fun of
      FunctionType r _ _ -> case r of
        VoidType -> emitInstrVoid instr >> (pure (ConstantOperand (Undef void)))
        _        -> emitInstr r instr
      PointerType (FunctionType r _ _) _ -> case r of
        VoidType -> emitInstrVoid instr >> (pure (ConstantOperand (Undef void)))
        _        -> emitInstr r instr
      _ -> error "Cannot call non-function (Malformed AST)."

nodeRegType :: LLVM.Type
nodeRegType = ptr (ptr closureType)

data Env
  = Env
  { nodeReg :: Operand -- ptr (ptr closure)
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

incrMany
  :: MonadIRBuilder m
  => Word32
  -> Operand -- ^ : ptr int
  -> m Operand
incrMany amount addr =
  case typeOf addr of
    PointerType IntegerType{} _ -> do
      val <- load addr 0
      some <- int32 $ fromIntegral amount
      val' <- add some val
      store addr 0 val'
      pure val'
    ty -> throw $ TypeError "incr" 2 "ptr(int)" (show ty)

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

-- | @push vals sp stack@
-- Pushes @vals@ onto @stack@ in order
pushManyStack
  :: MonadIRBuilder m
  => NonEmpty Operand -- ^ [A]
  -> Operand -- ^ ptr int
  -> Operand -- ^ ptr (array A)
  -> m ()
pushManyStack vals sp stack = do
  case typeOf sp of
    PointerType IntegerType{} _ ->
      case typeOf stack of
        PointerType (ArrayType _ itemTy) _
          | all (\val -> itemTy == typeOf val) vals -> do
            spVal <- load sp 0
            _ <- incrMany (fromIntegral $ length vals) sp
            zero <- int32 0
            for_ (zip [0..] $ toList vals) $ \(n, val) -> do
              offset <- int32 n
              sp' <- add offset spVal
              addr <- gep stack [zero, sp']
              store addr 0 val
          | otherwise ->
              throw $
              TypeError "pushStack" 2
                ("ptr(array(" <> show (head $ NonEmpty.filter (itemTy /=) $ fmap typeOf vals) <> "))")
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

enter
  :: (MonadReader Env m, MonadIRBuilder m)
  => Operand -- ^ ptr closure
  -> m ()
enter closureAddr = do
  node <- asks nodeReg
  store node 0 closureAddr
  zero <- int32 0
  code <-
    gep closureAddr
    [ zero -- deref closure pointer
    , zero -- first field of closure
    , zero -- deref info table pointer
    , zero -- first field of info table pointer
    ]
  _ <- tailcall code []
  retVoid

thunkType :: LLVM.Type
thunkType = PointerType (FunctionType void [] False) (AddrSpace 0) 

infoTableType :: LLVM.Type
infoTableType =
  StructureType False
  [ thunkType -- entry code
  , thunkType -- evacuation code
  , thunkType -- scavenge code
  ]

closureType :: LLVM.Type
closureType =
  StructureType False
  [ ptr infoTableType
  , undefined -- pointer block
  , undefined -- non-pointer block
  ]

data Closure

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
