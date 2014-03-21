{-# LANGUAGE GADTs #-}

module Parselang where
import Math.Qeatem
import Language
import Data.Char
import Data.Maybe
import Control.Monad
import Text.Parsec
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as H
import qualified Data.Map as M

type MetaParser a = Parsec String GlyphParseState a

data GlyphParseState = GlyphParseState {
  stateGlyphData :: GlyphData,
  stateFunctions :: HM.HashMap String [TypedVal],
  stateExprParser :: MetaParser TypedVal,
  stateInfixFuns :: HM.HashMap String [TypedVal],
  stateInfixOps :: HM.HashMap String (Int, [TypedVal])}

modifyGlyphData :: (GlyphData -> GlyphData) ->  GlyphParseState -> GlyphParseState
modifyGlyphData f state =
  state {stateGlyphData = f (stateGlyphData state)}

prependHighlight :: SyntaxToken -> SourcePos -> SourcePos -> GlyphParseState -> GlyphParseState
prependHighlight syntax sp sp2 =
  let line = sourceLine sp
      col  = sourceColumn sp
      len = sourceColumn sp2 - col
  in modifyGlyphData $
     \g -> g {glyphHighlight = M.insert (line, col)
                               (SourceRange line col len, syntax)
                               (glyphHighlight g)}
  
setSyntax :: SourcePos -> SyntaxToken -> MetaParser ()
setSyntax pos syntax = do
  sp <- getPosition
  void $ modifyState $ prependHighlight syntax pos sp

someSpace :: MetaParser ()
someSpace = void $ many $ oneOf " \t"

spacedAfter :: MetaParser a -> MetaParser a
spacedAfter p = do
  r <- p
  someSpace
  return r

pointDecimal :: MetaParser Double
pointDecimal = do
  void $ char '.' 
  s <- many1 digit
  return $ foldr (\d t -> t/10+fromIntegral (digitToInt d)) 0 s / 10

decimal :: MetaParser Int
decimal = do
  s <- many1 digit
  return $ foldl (\t d -> t*10+fromIntegral (digitToInt d)) 0 s

number :: MetaParser Double
number = 
  pointDecimal <|> do
    a <- decimal
    b <- option 0 pointDecimal
    return (fromIntegral a + b)

numericLit :: MetaParser Double
numericLit = spacedAfter $ do
  pos <- getPosition
  minus <- option 1 $ char '-' >> someSpace >> return (-1)
  a <- number
  b <- option 1 $ try $ someSpace >> char '/' >> someSpace >> number
  setSyntax pos (ConstantTok $ minus * a/b)
  return (minus * a/b)

index :: MetaParser Int
index = between (char '[') (char ']') decimal

castType :: Type a -> MetaParser TypedVal -> MetaParser a
castType t p =
 do e <- p
    case fromType t e of
      Nothing -> fail ""
      Just v -> return v
 <?> typeStr t

numericExpr :: MetaParser DepExp
numericExpr = castType NumType expression 

interpolate :: MetaParser (DepExp -> TypedVal)
interpolate = spacedAfter $ do
  char '[' >> someSpace
  a <- expression
  someSpace >> char ',' >> someSpace
  b <- expression
  void $ someSpace >> char ']'
  case (a, b) of
    (TypedVal NumType dlA, TypedVal NumType dlB) ->
      return $ \x -> TypedVal NumType $ (1-x)*dlA + x*dlB
    (TypedVal PairType (Pair dlAx dlAy), TypedVal PairType (Pair dlBx dlBy)) ->
      return $ \x -> TypedVal PairType $ Pair ((1-x)*dlAx + x*dlBx) ((1-x)*dlAy + x*dlBy)
    _ -> fail "Can only interpolate between numeric or pair"

suffix :: MetaParser Suffix
suffix = (Tag `liftM` many1 letter) <|>
         (SuffixIndex `liftM` decimal)

varOrApp :: MetaParser TypedVal
varOrApp = spacedAfter $ do
  state <- getState
  pos <- getPosition
  a <- try $ many1 letter
  case HM.lookup a (stateFunctions state) of
    Just [] -> fail "Unexpected: Empty function."
    Just f -> do
      setSyntax pos (FunctionTok a)
      foldr1 (<|>) $ map (try . parseFun) f
    Nothing
      | isJust (HM.lookup a $ stateInfixFuns state) -> fail ""
      | otherwise -> do
        name <- Name a `liftM` sepBy suffix (optional $ char '.')
        if H.member name (glyphParams $ stateGlyphData state)
           -- parameter
          then do setSyntax pos (ParameterTok name)
                  return $ TypedVal NumType $ makeConstant $
                    Parameter $ ParamName name

           -- variable
          else do setSyntax pos (VariableTok name)
                  return $ fromMaybe
                    (TypedVal NumType $ makeVariable $ Var name)
                    (HM.lookup name $ glyphVars $ stateGlyphData state)

parseFun :: TypedVal -> MetaParser TypedVal

-- a suffix argument can be used without parens, for example penpos1(0, 90)
parseFun fun@(TypedVal (FunType SuffixType a) f) =
  do s <- optional (char '.') >>
          sepBy1 suffix (optional $ char '.')
     someSpace
     case a of
       FunType _ _ ->
         char '(' >> someSpace >>
         (parseArgList $ TypedVal a $ f s)
       _ -> return $ TypedVal a $ f s
  <|> case a of
    FunType _ _ ->
      someSpace >> char '(' >>
      someSpace >> parseArgList fun
    _ -> fail ""
  <?> "Suffix argument."

parseFun fun@(TypedVal (FunType a b) f) = do
  someSpace
  case b of
    FunType _ _ ->
      char '(' >> someSpace >>
      parseArgList fun
    _ -> liftM (TypedVal b . f) $ castType a term

parseFun val = return val

parseArgList :: TypedVal -> MetaParser TypedVal
parseArgList (TypedVal (FunType a b) f) =
  do e <- castType a term
     someSpace
     case b of
       FunType _ _ -> char ',' >> someSpace
       _ -> return ()
     parseArgList $ TypedVal b $ f e

parseArgList v =
  (char ')' >> someSpace >> return v) <|> fail "to many arguments."

term :: MetaParser TypedVal
term = spacedAfter $ do
  t <- simpleTerm
  case t of
    TypedVal NumType dl ->
      ($dl) `liftM` option (TypedVal NumType) interpolate
    _ -> return t

optNegate :: MetaParser TypedVal -> MetaParser TypedVal
optNegate p = try $ do
  minus <- optionMaybe $ char '-' >> someSpace
  case minus of
    Nothing -> p
    Just _ -> do
      n <- p
      (case n of
          TypedVal NumType m ->
            return $ TypedVal NumType (-m)
          TypedVal PairType (Pair x y) ->
            return $ TypedVal PairType (Pair (-x) (-y))
          _ -> fail ""
        ) <?> "numeric of pair expression."

simpleTerm :: MetaParser TypedVal
simpleTerm = optNegate varOrApp <|> optNegate subExpr <|> do
  a <- numericLit
  b <- optionMaybe (subExpr <|> varOrApp)
  case b of
    Nothing ->
      return $ TypedVal NumType $ makeConstant $ Number a
    Just (TypedVal NumType e) ->
      return $ TypedVal NumType $ (makeConstant $ Number a)*e
    Just (TypedVal PairType (Pair x y)) ->
      return $ TypedVal PairType $
      Pair ((makeConstant $ Number a)*x) ((makeConstant $ Number a)*y)
    _ ->
      fail "Can only multiply numeric literal with numeric or pair expression."
  
subExpr :: MetaParser TypedVal
subExpr = spacedAfter $ do
  char '(' >> someSpace
  TypedVal t e <- expression
  someSpace
  case t of
    NumType ->
      (char ')' >> someSpace >> return (TypedVal t e))
      <|> do char ',' >> someSpace
             e2 <- numericExpr
             void $ someSpace >> char ')'
             return $ TypedVal PairType $ Pair e e2
    _ -> someSpace >> char ')' >> return (TypedVal t e)

infixTerm :: MetaParser TypedVal
infixTerm = term >>= infixApp

infixFuns :: MetaParser [TypedVal]
infixFuns = do
  pos <- getPosition
  state <- getState
  a <- many1 letter
  case HM.lookup a (stateInfixFuns state) of
    Just l -> setSyntax pos (InfixTok a) >> return l
    Nothing -> fail ""

infixApp :: TypedVal -> MetaParser TypedVal
infixApp t = do
  (do funs <- try infixFuns
      when (null funs) (fail "")
      (foldr1 (<|>) $ map (try . appInfix t) funs)
        >>= infixApp
    ) <|> return t

appInfix :: TypedVal -> TypedVal -> MetaParser TypedVal
appInfix t (TypedVal (FunType a b) f) = do
  case fromType a t of
    Just v -> parseFun $ TypedVal b $ f v
    Nothing -> fail $ "Infix function expects " ++ typeStr a

appInfix _ _ = fail "Invalid function"

-- find the next operator and check fixity
operator :: Int -> MetaParser (Int, String, [TypedVal])
operator n = spacedAfter $ do
  state <- getState
  pos <- getPosition
  s <- many1 $ oneOf "<=>:|+-/*\\!?#&@^~"
  setSyntax pos (InfixTok s)
  case HM.lookup s (stateInfixOps state) of
    Nothing -> fail ""
    Just (m, f)
      | m < n -> fail ""
      | otherwise -> return (m, s, f)

applyInfixOp :: TypedVal -> TypedVal -> TypedVal -> MetaParser TypedVal
applyInfixOp v1 v2 (TypedVal (FunType t1 (FunType t2 t3)) f) =
  case (fromType t1 v1, fromType t2 v2) of
    (Just r1, Just r2) -> return $ TypedVal t3 $ f r1 r2
    _ -> fail ""

applyInfixOp _ _ _ = fail "Illegal function"

getInfixOp :: Int -> TypedVal -> MetaParser TypedVal
getInfixOp n v@(TypedVal t1 _)  =
  (do (m, s, funs) <- try $ operator n
      next <- term
      v2@(TypedVal t2 _) <- getInfixOp m next
      res <- foldr (<|>)
             (fail $ typeStr t1 ++ " " ++ s ++ " " ++
              typeStr t2 ++ " not defined.") $
             map (applyInfixOp v v2) funs
      getInfixOp n res
  ) <|> return v

expression :: MetaParser TypedVal
expression = infixTerm >>= getInfixOp 0
