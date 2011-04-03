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
{
module Language.EcmaScript.Parser.Scanner (scan) where

import UU.Scanner.GenToken
import UU.Scanner.Position
import Language.EcmaScript.Parser.Tokens
}
-------------------------------------------------------------------------------
-- Character groups
-------------------------------------------------------------------------------
-- TODO: support Unicode

$letter = [A-Za-z]
$combiningMark = []
$digit = [0-9]
$connectorPunctuation = [_]
$lineTerminator = [\n\r]
$hexDigit = [0-9a-fA-F]

-------------------------------------------------------------------------------
-- Macro's
-------------------------------------------------------------------------------

@unicodeEscapeSequence = u $hexDigit{4}

-- Identifier
@identifierStart = $letter | [\$\_] | \\ @unicodeEscapeSequence
@identifierPart = @identifierStart | $combiningMark | $digit | $connectorPunctuation
@identifier = @identifierStart @identifierPart+ | @identifierPart+

-- Keywords
@keyword = 
    break|do|instanceof|typeof|case|else|new|var|catch|finally|return|void|
    continue|for|switch|while|debugger|function|this|with|default|if|throw|
    delete|in|try

@futureKeyword = class|enum|extends|super|const|export|import|implements|
                 let|private|public|yield|interface|package|protected|static

-- Punctuators
@punctuator =
    "[" | "]" | "(" | ")" | "{" | "}" |
    ">=" | "<" | ">" | "," | ";" | "." |
    "!==" | "===" | "!=" | "==" | "<=" |
    "--" | "++" | "%" | "*" | "-" | "+" |
    "^" | "|" | "&" | ">>>" | "<<" | ">>" |
    ":" | "?" | "||" | "&&" | "~" | "!" |
    ">>=" | "%=" | "*=" | "-=" | "+=" | "=" |
    "^=" | "|=" | "&="  | ">>>=" | "<<=" |
    "/" | "/="

-- Comment
@multiLineComment = \/\* ([^\*] | \*+[^\/\*] | \*\n | \n)* \*+\/
@singleLineComment = \/\/ ~$lineTerminator*
@comment = @singleLineComment | @multiLineComment

-- String
@stringEscape  = [^0-9XxUu] | @unicodeEscapeSequence | [xX] $hexDigit{2}
@stringDoubleQ  =  [^\"\\] | \\ @stringEscape
@stringSingleQ  =  [^\'\\] | \\ @stringEscape

-- Numbers
@decimalLiteral = [0-9]+ (\.[0-9]+)? ([eE] [\+\-]? [0-9]+)? |
                  \.[0-9]+ ([eE] [\+\-]? [0-9]+)?

@hexLiteral = 0[xX] $hexDigit+

-------------------------------------------------------------------------------
-- Token definitions
-------------------------------------------------------------------------------

tokens :-
  <0>     \" @stringDoubleQ* \"             { ValToken TkString }
  <0>     \' @stringSingleQ* \'             { ValToken TkString }
  <0>     @keyword | @futureKeyword         { Reserved }
  <0>     @punctuator                       { Reserved }
  <0>     @comment                          ;
  <0>     true | false                      { Reserved }
  <0>     null                              { Reserved }
  <0>     @decimalLiteral                   { ValToken TkNumeric }
  <0>     @hexLiteral                       { ValToken TkNumeric }
  <0>     @identifier                       { ValToken TkIdent }
  <0>     $white+                           ;

{
type AlexInput = (Pos, String)
type StateStack = [Int]

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (_, [])   = Nothing
alexGetChar (p, c:cs) = Just (c, (adv p c, cs))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "alexInputPrevChar: should not be used."

scan :: FilePath -> String -> [Token]
scan f s = scan' (initPos f,s)

scan' :: AlexInput -> [Token]
scan' i@(pos,str) =
  case (alexScan i 0) of
    (AlexEOF)                 -> []
    (AlexError _)            -> let (Just (c, i')) = alexGetChar i
                                 in  errToken [c] pos : scan' i'
    (AlexSkip i' len)         -> scan' i'
    (AlexToken i' len action) -> let token = action (take len str) pos
                                 in  token : scan' i'
}
