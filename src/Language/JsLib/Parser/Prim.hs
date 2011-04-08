module Language.JsLib.Parser.Prim where

import Text.ParserCombinators.Parsec hiding (many,(<|>))
import Text.ParserCombinators.Parsec.Pos
import Language.JsLib.Parser.Tokens
import Language.JsLib.AST
import Control.Applicative

type JsParser a = GenParser Token UserState a

data UserState = UserState

-------------------------------------------------------------------------------
-- Primary parsers
-------------------------------------------------------------------------------

pReserved :: String -> JsParser String
pReserved v = token pretty position m
  where
    pretty t = show v
    m (Reserved v' _) | v == v' = Just v
    m _               = Nothing

pValToken :: ValTokenType -> JsParser String
pValToken ty = token pretty position m
  where
    pretty t = show ty
    m (ValToken ty' v _) | ty == ty' = Just v
    m _                  = Nothing

-------------------------------------------------------------------------------
-- Utility parsers
-------------------------------------------------------------------------------

pIdent :: JsParser String
pIdent = pValToken TkIdent

pCommaList :: JsParser a -> JsParser [a]
pCommaList p = sepBy p (pReserved ",")

pChoice :: [JsParser a] -> JsParser a
pChoice = foldr (<|>) pzero

pOp :: (a,String) -> JsParser a
pOp (sem,op) = sem <$ pReserved op

anyOp :: [(a,String)] -> JsParser a
anyOp = pChoice . map pOp

pPack :: String -> JsParser a -> String -> JsParser a
pPack o p c = pReserved o *> p <* pReserved c

(<??>) :: JsParser a -> JsParser (a -> a) -> JsParser a
p <??> q = p <**> option id q

pMaybe :: JsParser a -> JsParser (Maybe a)
pMaybe = optionMaybe
