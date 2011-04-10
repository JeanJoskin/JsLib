--
-- Copyright (c) 2011, Jean Joskin
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of Jean Joskin nor the
--       names of its contributors may be used to endorse or promote products
--       derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL JEAN JOSKIN BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

module Language.JsLib.Parser.Tokens (Token (..), ValTokenType (..), position, errToken, ppTokens) where

import Text.ParserCombinators.Parsec.Pos
import Text.PrettyPrint.HughesPJ

data Token = Reserved !String !SourcePos
           | ValToken !ValTokenType String !SourcePos
                 
data ValTokenType
  = TkComment
  | TkIdent
  | TkNumeric
  | TkString
  | TkRegExp
  | TkError
  deriving (Eq, Ord)

errToken :: String -> SourcePos -> Token
errToken = ValToken TkError

position :: Token -> SourcePos
position (Reserved _ p) = p
position (ValToken _ _ p) = p

-------------------------------------------------------------------------------
-- Show instance
-------------------------------------------------------------------------------

instance Show Token where
  showsPrec _ token
    = showString
       (case token of
         Reserved key      pos -> "symbol "      ++ key
         ValToken tp val   pos -> show tp ++ " " ++ val
       )

instance Show ValTokenType where
 show tp = case tp of
  TkComment    -> "comment"
  TkIdent      -> "identifier"
  TkNumeric    -> "number"
  TkString     -> "string"
  TkRegExp     -> "regular expression"
  TkError      -> "error in scanner:"

-------------------------------------------------------------------------------
-- Pretty printer
-------------------------------------------------------------------------------

ppTokens :: [Token] -> Doc
ppTokens = vcat . map ppToken

ppToken :: Token -> Doc
ppToken (Reserved str pos)    = text "keyword" <+> text str <+> text "at" <+> text (show pos)
ppToken (ValToken tp val pos) = text (show tp) <+> text val <+> text "at" <+> text (show pos)
