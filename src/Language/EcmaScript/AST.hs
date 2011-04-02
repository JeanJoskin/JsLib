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

module Language.EcmaScript.AST where

import UU.Scanner.Position

type Ident = String
type Number = Double

type FunctionBody = [SourceElement]

emptyIdent :: Ident
emptyIdent = ""

data SourceElement
  = SEStatement Statement
  | SEFunctionDecl Ident [Ident] [SourceElement]
  deriving Show

data PropertyAssignment
  = PAExpr PropertyName Expression
  | PAGet PropertyName FunctionBody
  | PASet PropertyName Ident FunctionBody
  deriving Show

data PropertyName
  = PNIdent Ident
  | PNString String
  | PNNumeric Number
  deriving Show

data Expression
  = EExpression Expression
  | EThis
  | EIdent Ident
  | ENull
  | EBool Bool
  | ENumeric Number
  | EString String
  | EArray [Expression]
  | EObject [PropertyAssignment]
  | EFunction Ident [Ident] FunctionBody
  | EIndex Expression Expression
  | EDot Expression Ident
  | ENew Expression [Expression]
  | ECall Expression [Expression]
  | EComma Expression Expression
  | EPostPlusPlus Expression
  | EPostMinMin Expression
  | EDelete Expression
  | EVoid Expression
  | ETypeOf Expression
  | EPreInc Expression
  | EPreDec Expression
  | EUnaryPlus Expression
  | EUnaryMin Expression
  | EBitNot Expression
  | ELogicNot Expression
  | EMultiply Expression Expression
  | EDivide Expression Expression
  | EModulus Expression Expression
  | EAdd Expression Expression
  | ESubtract Expression Expression
  | ESignedShiftLeft Expression Expression
  | ESignedShiftRight Expression Expression
  | EUnsignedShiftRight Expression Expression
  | ELess Expression Expression
  | EGreater Expression Expression
  | ELessEqual Expression Expression
  | EGreaterEqual Expression Expression
  | EInstanceof Expression Expression
  | EIn Expression Expression
  | EEqual Expression Expression
  | ENotEqual Expression Expression
  | EStrictEqual Expression Expression
  | EStrictNotEqual Expression Expression
  | EBitAND Expression Expression
  | EBitXOR Expression Expression
  | EBitOR Expression Expression
  | ELogicAND Expression Expression
  | ELogicOR Expression Expression
  | EConditional Expression Expression Expression
  | EAssign Expression Expression
  | EAssignMultiply Expression Expression
  | EAssignDivide Expression Expression
  | EAssignAdd Expression Expression
  | EAssignSubtract Expression Expression
  | EAssignModulus Expression Expression
  | EAssignSignedShiftLeft Expression Expression
  | EAssignSignedShiftRight Expression Expression
  | EAssignUnsignedShiftRight Expression Expression
  | EAssignBitAND Expression Expression
  | EAssignBitXOR Expression Expression
  | EAssignBitOR Expression Expression
  deriving Show

data ForClause
  = FCExprExprExpr (Maybe Expression) (Maybe Expression) (Maybe Expression)
  | FCVarExprExpr [(Ident,Maybe Expression)] (Maybe Expression) (Maybe Expression)
  | FCLhsIn Expression Expression
  | FCVarIn (Ident,Maybe Expression) Expression
  deriving Show

data CaseClause
  = CCCase Expression [Statement]
  | CCDefault [Statement]
  deriving Show

data Statement
  = SEmpty
  | SBlock [Statement]
  | SVariable [(Ident,Maybe Expression)]
  | SExpression Expression
  | SIf Expression Statement (Maybe Statement)
  | SDoWhile Statement Expression
  | SWhile Expression Statement
  | SFor ForClause Statement
  | SContinue (Maybe Ident)
  | SBreak (Maybe Ident)
  | SReturn (Maybe Expression)
  | SWith Expression Statement
  | SSwitch Expression [CaseClause]
  | SLabel Ident Statement
  | SThrow Expression
  | STry Statement (Maybe (Ident,Statement)) (Maybe Statement)
  | SDebugger
  deriving Show
