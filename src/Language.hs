{-# LANGUAGE DeriveGeneric, GADTs #-}
module Language where
import GHC.Generics (Generic)
import Math.Qeatem
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.HashSet as H

data HighlightToken = HighlightToken Int Int SyntaxToken
type Highlight = M.Map (Int, Int) (SourceRange, SyntaxToken)
data SyntaxToken = InfixTok String
                 | VariableTok Name
                 | ParameterTok Name
                 | FunctionTok String
                 | ConstantTok Double

data SourceRange = SourceRange Int Int Int -- line column length
               deriving (Eq, Ord, Show, Generic)

data Suffix = Tag String | SuffixIndex Int
            deriving (Show, Eq, Ord, Generic)
data Name = Name String [Suffix]
          deriving (Show, Eq, Ord, Generic)

type DepExp = DependendExpr Expression Var

type Expression = Expr Param
data Var = Var Name
         deriving (Show, Eq, Ord, Generic)
data Param = ParamName Name
           | ParamConst SourceRange
           deriving (Show, Eq, Ord, Generic)

instance Hashable SourceRange
instance Hashable Name
instance Hashable Var
instance Hashable Suffix
instance Hashable Param

data Pair = Pair {
  pair_x :: DepExp,
  pair_y :: DepExp}

data Transform = Transform {
  xform_xx :: DepExp,
  xform_xy :: DepExp,
  xform_tx :: DepExp,
  xform_yx :: DepExp,
  xform_yy :: DepExp,
  xform_ty :: DepExp }

{- parsing:
stream -> (Highlight, Map Sourcepos Double, Dependencies, PairVars, TransformVars, Paths, PathOperations, Params)
-}

data EMetaPath = OpenMetaPath [(Expression, Expression, EMetaJoin)] (Expression, Expression)
               | CyclicMetaPath [(Expression, Expression, EMetaJoin)]

data ETension = ETension SourceRange String Expression
              | ETensionAtLeast String Expression

data EMetaTension =
  ETensionBoth ETension ETension |
  ETensionOne  ETension |
  ETensionDefault Bool 

data EMetaNodeType = Open
                   | Curl String Expression
                   | Direction String Expression Expression

data EMetaJoin = ExprMetaJoin EMetaNodeType EMetaTension EMetaNodeType
data TypedVal where
  TypedVal :: Type a -> a -> TypedVal

data Infix = Infix Int TypedVal

data ExportVals = ExportVals {
  localVars :: HM.HashMap Name TypedVal,
  exportVars :: HM.HashMap Name TypedVal,
  pathOperations :: [PathOperation] }

data Type a where
  NumType :: Type DepExp
  PairType :: Type Pair
  TransformType :: Type Transform
  ExportType :: Type ExportVals
  SuffixType :: Type [Suffix]
  FunType :: Type a -> Type b -> Type (a -> b)

fromType :: Type a -> TypedVal -> Maybe a
fromType NumType (TypedVal NumType v) = Just v
fromType PairType (TypedVal PairType v) = Just v
fromType TransformType (TypedVal TransformType v) = Just v
fromType ExportType (TypedVal ExportType v) = Just v
fromType SuffixType (TypedVal SuffixType v) = Just v
fromType _ _ = Nothing

typeStr :: Type a -> String
typeStr NumType = "<numeric expression>"
typeStr PairType = "<pair expression>"
typeStr TransformType = "<transform expression>"
typeStr SuffixType = "<suffix>"
typeStr ExportType = "<export list>"
typeStr (FunType a@(FunType _ _) b) = "(" ++ typeStr a ++ ") -> " ++ typeStr b
typeStr (FunType a b) = typeStr a ++ " -> " ++ typeStr b


data PathOperation =
  DrawPath EMetaPath Expression |
  FillPath EMetaPath |
  FillDrawPath EMetaPath Expression |
  UnDrawPath EMetaPath Expression |
  UnFillPath EMetaPath |
  UnFillDrawPath EMetaPath Expression

data GlyphData = GlyphData {
  glyphParams :: H.HashSet Name,
  glyphVars :: HM.HashMap Name TypedVal,
  glyphHighlight :: Highlight,
  glyphDeps :: Dependencies Param Var,
  glyphPaths :: [EMetaPath],
  glyphPathOperations :: [PathOperation],
  glyphEmptyVars :: [Name]
}
