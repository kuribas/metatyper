{-# LANGUAGE DeriveGeneric, FlexibleInstances, GADTs #-}
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
type Deps = Dependencies Expression Var

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
  xform_x :: DepExp,
  xform_y :: DepExp,
  xform_xx :: DepExp,
  xform_xy :: DepExp,
  xform_yx :: DepExp,
  xform_yy :: DepExp }

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
  PathType :: Type EMetaPath
  FunType :: Type a -> Type b -> Type (a -> b)

class ToType a where
  toType :: a -> Type a

instance ToType DepExp where
  toType _ = NumType

instance ToType Pair where
  toType _ = PairType

instance ToType Transform where
  toType _ = TransformType

instance ToType ExportVals where
  toType _ = ExportType

instance ToType [Suffix] where
  toType _ = SuffixType

instance ToType EMetaPath where
  toType _ = PathType

instance (ToType a, ToType b) => ToType (a -> b) where
  toType _ = FunType (toType (undefined::a)) (toType (undefined::b))

addType :: ToType a => a -> TypedVal
addType a = TypedVal (toType a) a

fromType :: Type a -> TypedVal -> Maybe a
fromType NumType (TypedVal NumType v) = Just v
fromType PairType (TypedVal PairType v) = Just v
fromType TransformType (TypedVal TransformType v) = Just v
fromType ExportType (TypedVal ExportType v) = Just v
fromType SuffixType (TypedVal SuffixType v) = Just v
fromType PathType (TypedVal PathType v) = Just v
fromType _ _ = Nothing

typeStr :: Type a -> String
typeStr NumType = "<numeric expression>"
typeStr PairType = "<pair expression>"
typeStr TransformType = "<transform expression>"
typeStr SuffixType = "<suffix>"
typeStr ExportType = "<export list>"
typeStr PathType = "<path>"
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
  glyphPathOperations :: [PathOperation],
  glyphEmptyVars :: [Name]
}

type ExprFun2 = DepExp -> DepExp -> DepExp
type ExprFun1 = DepExp -> DepExp

operators :: HM.HashMap String (Int, [TypedVal])
operators = HM.fromList [
  ("+", (4, [addType ((+) :: ExprFun2),
             addType $ \(Pair x y) (Pair x2 y2) ->
             Pair (x+x2) (y+y2)])),
  ("-", (4, [addType ((-) :: ExprFun2),
             addType $ \(Pair x y) (Pair x2 y2) ->
             Pair (x-x2) (y-y2)])),
  ("*", (5, [addType ((*) :: ExprFun2),
             addType $ \(Pair x y) a -> Pair (x*a) (y*a),
             addType $ \a (Pair x y) -> Pair (x*a) (y*a)])),
  ("/", (5, [addType ((/) :: ExprFun2),
             addType $ \(Pair x y) a -> Pair (x/a) (y/a)])),
  ("++", (5, [addType ((\x y -> sqrt (x**2 + y**2)) :: ExprFun2)])),
  ("+-+", (5, [addType ((\x y -> sqrt (x**2 - y**2)) :: ExprFun2)])),
  ("**", (6, [addType ((**) :: ExprFun2)]))]

-- infixFunctions = HM.fromList [
--   ("rotated", [addType rotated]),
--   ("shifted", [addType shifted]),
--   ("slanted", [addType slanted]),
--   ("transformed", [addType transformed]),
--   ("xscaled", [addType xscaled]),
--   ("yscaled", [addType yscaled]),
--   ("zscaled", [addType zscaled]),
--   ("dotprod", [addType $ \(Pair a b) (Pair c d) -> Pair (a*c) + (b*d)])]
                 
functions :: HM.HashMap String [TypedVal]
functions = HM.fromList [
  ("sqrt", [addType (sqrt :: ExprFun1)]),
  ("sind", [addType (sin . (* (pi/180 :: DepExp)))]),
  ("cosd", [addType (cos . (* (pi/180 :: DepExp)))]),
  ("log", [addType (log :: ExprFun1)]),
  ("exp", [addType (exp :: ExprFun1)]),
  ("angle", [addType $ \x y -> atan (y/x) * 180/pi :: DepExp]),
  ("dir", [addType $ \a -> Pair (cos (a*pi/180)) (sin (a*pi/180))]),
  ("xpart", [addType pair_x,
             addType xform_x]),
  ("ypart", [addType pair_y,
             addType xform_y]),
  ("xxpart", [addType xform_xx]),
  ("xypart", [addType xform_xy]),
  ("yxpart", [addType xform_yx]),
  ("yypart", [addType xform_yy]),
  ("z", [addType $ \s -> Pair (makeVariable $ Var $ Name "x" s :: DepExp)
                         (makeVariable $ Var $ Name "y" s :: DepExp)])]
