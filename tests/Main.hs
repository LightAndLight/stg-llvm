{-# language OverloadedStrings #-}
module Main where

import Hedgehog
  ((===), Property, MonadGen, property, check, forAll, forAllWith, success,
   annotate)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (($>))
import Data.Word (Word32)
import Data.Semigroup ((<>))
import Data.Text.Lazy (unpack)
import Foreign.Ptr (FunPtr, castFunPtr)
import LLVM.AST (Module, Operand)
import LLVM.AST.Type (Type(ArrayType, IntegerType), i32, void)
import LLVM.AST.Constant (Constant(Int))
import LLVM.ExecutionEngine (withMCJIT, getFunction, withModuleInEngine)
import LLVM.IRBuilder.Constant (array, int32)
import LLVM.IRBuilder.Instruction (gep, load, store, alloca, ret)
import LLVM.IRBuilder.Module
  (MonadModuleBuilder, ModuleBuilder, buildModule, function)
import LLVM.IRBuilder.Monad
  (MonadIRBuilder, IRBuilder, IRBuilderT, runIRBuilder, emptyIRBuilder)
import LLVM.Internal.Context (withContext)
import LLVM.Module (File(..), withModuleFromAST, writeBitcodeToFile)
import LLVM.Pretty (ppllvm)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import qualified LLVM.AST as LLVM (Type)

import Compile

tmpfile :: FilePath
tmpfile = "test.ll"

foreign import ccall "dynamic" reify :: FunPtr (IO Word32) -> IO Word32

lli :: IRBuilderT ModuleBuilder () -> IO Word32
lli code =
  withContext $ \ctxt ->
  withMCJIT ctxt (Just 0) Nothing Nothing Nothing $ \ee ->
  let
    mod = function "main" [] i32 (\_ -> code)
  in
  withModuleFromAST ctxt (buildModule "testModule" mod) $ \output ->
  withModuleInEngine ee output $ \exmodule ->
    getFunction exmodule "main" >>=
    maybe (error "main not found") (\fn -> reify (castFunPtr fn))

compileBitcode
  :: FilePath -> IRBuilderT ModuleBuilder () -> IO ()
compileBitcode fp code =
  withContext $ \ctxt ->
  let
    mod = function "main" [] i32 (\_ -> code)
  in
    withModuleFromAST
      ctxt
      (buildModule "testModule" mod)
      (writeBitcodeToFile (File fp))

genArrayPtr :: (MonadGen m, MonadIRBuilder n) => m ([Word32], n Operand)
genArrayPtr = do
  n <- Gen.integral (Range.constant 1 99)
  cs <- replicateM n $ Gen.word32 (Range.constant 0 (2^32))
  pure
    ( cs
    , do
        addr <- alloca (ArrayType 100 i32) Nothing 0
        val <- array $ Int 32 . fromIntegral <$> cs
        store addr 0 val
        pure addr
    )

genIntPtr :: (MonadGen m, MonadIRBuilder n) => m (Word32, n Operand)
genIntPtr = do
  n <- Gen.word32 (Range.constant 0 (2^32))
  pure
    ( n
    , do
        addr <- alloca (IntegerType 32) Nothing 0
        val <- int32 $ fromIntegral n
        store addr 0 val
        pure addr
    )

showIRBuilder :: IRBuilderT ModuleBuilder a -> String
showIRBuilder code =
  unpack .
  ppllvm .
  buildModule "rendering_debug_code" $
  function "rendering_debug_code" [] void $ \_ -> code $> ()

prop_incr_1 :: Property
prop_incr_1 =
  property $ do
    (num, initNum) <-
      forAllWith
        (\(a, b) -> "(" <> show a <> ",\n\n" <> showIRBuilder b <> ")")
        genIntPtr
    let
      code =
        do
          addr <- initNum
          res <- incr addr
          ret res

    res <- liftIO $ lli code
    res === num + 1

prop_incr_2 :: Property
prop_incr_2 =
  property $ do
    (num, initNum) <-
      forAllWith
        (\(a, b) -> "(" <> show a <> ",\n\n" <> showIRBuilder b <> ")")
        genIntPtr
    let
      code =
        do
          addr <- initNum
          _ <- incr addr
          res <- load addr 0
          ret res

    res <- liftIO $ lli code
    res === num + 1

prop_decr_1 :: Property
prop_decr_1 =
  property $ do
    (num, initNum) <-
      forAllWith
        (\(a, b) -> "(" <> show a <> ",\n\n" <> showIRBuilder b <> ")")
        genIntPtr
    let
      code =
        do
          addr <- initNum
          res <- decr addr
          ret res

    res <- liftIO $ lli code
    res === num - 1

showArrayPtr :: ([Word32], IRBuilderT ModuleBuilder a) -> String
showArrayPtr (a, b) = "(" <> show a <> ",\n\n" <> showIRBuilder b <> ")"

prop_decr_2 :: Property
prop_decr_2 =
  property $ do
    (num, initNum) <- forAllWith (show . fst) genIntPtr
    let
      code =
        do
          addr <- initNum
          _ <- decr addr
          res <- load addr 0
          ret res

    res <- liftIO $ lli code
    res === num - 1

prop_stack_peek :: Property
prop_stack_peek =
  property $ do
    (arrayItems, initArray) <- forAllWith showArrayPtr genArrayPtr
    let
      code = do
        arrayStart <- initArray

        sp <- alloca i32 Nothing 0
        offset <- int32 $ fromIntegral (length arrayItems - 1)
        store sp 0 offset

        val <- peekStack sp arrayStart
        ret val

    res <- liftIO $ lli code
    res === last arrayItems

prop_stack_push :: Property
prop_stack_push =
  property $ do
    (arrayItems, initArray) <- forAllWith showArrayPtr genArrayPtr
    newVal <- forAll $ Gen.word32 (Range.constant 0 (2^32))
    let
      code = do
        arrayStart <- initArray

        sp <- alloca i32 Nothing 0
        offset <- int32 $ fromIntegral (length arrayItems - 1)
        store sp 0 offset

        val <- int32 $ fromIntegral newVal
        pushStack val sp arrayStart
        val' <- peekStack sp arrayStart
        ret val'

    res <- liftIO $ lli code
    res === newVal

prop_stack_push_pop :: Property
prop_stack_push_pop =
  property $ do
    (arrayItems, initArray) <- forAllWith showArrayPtr genArrayPtr
    newVal <- forAll $ Gen.word32 (Range.constant 0 (2^32))
    let
      code = do
        arrayStart <- initArray

        sp <- alloca i32 Nothing 0
        offset <- int32 $ fromIntegral (length arrayItems - 1)
        store sp 0 offset

        val <- int32 $ fromIntegral newVal
        pushStack val sp arrayStart
        _ <- popStack sp arrayStart
        val' <- peekStack sp arrayStart
        ret val'

    res <- liftIO $ lli code
    res === last arrayItems

main :: IO Bool
main = do
  check prop_incr_1
  check prop_incr_2
  check prop_decr_1
  check prop_decr_2
  check prop_stack_peek
  check prop_stack_push
  check prop_stack_push_pop
