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

module Language.EcmaScript.Parser.Tokens (Token (..), ValTokenType (..), errToken, ppTokens) where

import UU.Scanner.GenToken
import UU.Scanner.Position
import UU.Pretty

type Token = GenToken String ValTokenType String

data ValTokenType
  = TkComment
  | TkIdent
  | TkNumeric
  | TkString
  | TkError
  deriving (Eq, Ord)

errToken :: String -> Pos -> Token
errToken = ValToken TkError

-------------------------------------------------------------------------------
-- Show instance
-------------------------------------------------------------------------------

instance Show Token where
  showsPrec _ token
    = showString
       (case token of
         Reserved key      pos -> "symbol "      ++ key ++ maybeshow pos
         ValToken tp val   pos -> show tp ++ " " ++ val ++ maybeshow pos
       )

instance Show ValTokenType where
 show tp = case tp of
  TkComment    -> "comment"
  TkIdent      -> "identifier"
  TkNumeric    -> "number"
  TkString     -> "string"
  TkError      -> "error in scanner:"
  
maybeshow :: Pos -> String
maybeshow (Pos l c fn) | l <= 0 || c <= 0 =  ""
                       | otherwise        =  " at line " ++ show l
                                          ++ ", column " ++ show c
                                          ++ " of file " ++ show fn

-------------------------------------------------------------------------------
-- Pretty printer
-------------------------------------------------------------------------------

ppTokens :: [Token] -> PP_Doc
ppTokens = vlist . map ppToken

ppToken :: Token -> PP_Doc
ppToken (Reserved str pos)    = "keyword " >|< show str >|< " at " >|< ppPos pos
ppToken (ValToken tp val pos) = show tp >|< " " >|< val >|< " at " >|< ppPos pos

ppPos :: Pos -> PP_Doc
ppPos (Pos l c _) = show l >|< "," >|< show c
