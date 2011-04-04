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

module Language.EcmaScript.Parser.Parser (parse) where

import GHC.Prim
import UU.Parsing hiding (parse)
import qualified UU.Parsing (parse)
import UU.Scanner.Position
import UU.Scanner.GenToken
import UU.Scanner.GenTokenOrd
import UU.Scanner.GenTokenSymbol
import Text.Printf
import Language.EcmaScript.Parser.Tokens
import Language.EcmaScript.AST

type JsParser a = Parser Token a

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

pReserved :: String -> JsParser String
pReserved key = let tok = Reserved key noPos
                in  key <$ pSym tok

pValToken :: ValTokenType -> String -> JsParser String
pValToken tp val = let tok = ValToken tp val noPos
                       f (ValToken _ x _) = x
                   in  f <$> pSym tok
{-
pReservedIdent :: String -> JsParser String
pReservedIdent key = let tok = (ValToken TkIdent key noPos) :: Token
                         f (ValToken _ x _) = x == key
                         f _                = False
                     in  key <$ pSatisfy f (Insertion (show tok) tok 5) 
-}
maybePostfix :: a -> Maybe (a -> a) -> a
maybePostfix = flip (maybe id ($))

maybePostfixC :: (a -> a) -> Maybe (a -> a) -> (a -> a)
maybePostfixC = flip (maybe id (.))

pIdent :: JsParser String
pIdent = pValToken TkIdent "<identifier>"

pCommaList :: JsParser a -> JsParser [a]
pCommaList = pListSep (pReserved ",")

pChoice :: [JsParser a] -> JsParser a
pChoice = foldr (<|>) pFail

pOp :: (a,String) -> JsParser a
pOp (sem,op) = sem <$ pReserved op

anyOp :: [(a,String)] -> JsParser a
anyOp = pChoice . map pOp

pPack :: String -> JsParser a -> String -> JsParser a
pPack o p c = pReserved o *> p <* pReserved c

pMaybe :: JsParser a -> JsParser (Maybe a)
pMaybe p = opt (Just <$> p) Nothing

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

pArguments :: JsParser [Expression]
pArguments = pPack "(" (pCommaList pAssignmentExpression) ")"

pFunctionBody :: JsParser [SourceElement]
pFunctionBody = pList pSourceElement

-- Literal (7.8)
pLiteral :: JsParser Expression
pLiteral = pENull <|> pEBool <|> pENumeric <|> pEString <?> "literal"

pENull = ENull <$ pReserved "null"
pEBool = EBool <$> anyOp [(True,"true"),(False,"false")]
pENumeric = ENumeric <$> (read <$> pValToken TkNumeric "<number>")
pEString = EString <$> (read <$> pValToken TkString "<string>")

-- PrimaryExpression (11.1)
pPrimaryExpression :: JsParser Expression
pPrimaryExpression = pEThis <|> pEIdent <|> pLiteral <|> pEObject <|> pEArray <|> pEExpression <?> "primary expression"

pEThis = EThis <$ pReserved "this"
pEIdent = EIdent <$> pValToken TkIdent "<identifier>"
pEObject = EObject <$> pPack "{" (pCommaList pPropertyAssignment) "}"
pEArray = EArray <$> pPack "[" (pCommaList pAssignmentExpression) "]"
pEExpression = EExpression <$> pPack "(" pExpression ")"

-- PropertyName (11.1.5)
pPropertyName :: JsParser PropertyName
pPropertyName = pPNIdent <|> pPNString <|> pPNNumeric <?> "property name"

pPNIdent = PNIdent <$> pValToken TkIdent "<identifier>"
pPNString = PNString <$> pValToken TkString "<string>"
pPNNumeric = PNNumeric . read <$> pValToken TkNumeric "<number>"

-- PropertyAssignment (11.1.5)
pPropertyAssignment :: JsParser PropertyAssignment
pPropertyAssignment = pPAExpr <|> pPAGet <|> pPASet <?> "property assignment"

pPAExpr = PAExpr <$> pPropertyName <* pReserved ":" <*> pAssignmentExpression
pPAGet = PAGet <$ pReserved "get" <*> pPropertyName <* pReserved "(" <*
                  pReserved ")" <* pReserved "{" <*> pFunctionBody <*
                  pReserved "}"
pPASet = PASet <$ pReserved "set" <*> pPropertyName <* pReserved "(" <*>
                  pIdent <* pReserved ")" <* pReserved "{" <*> pFunctionBody <*
                  pReserved "}"

-- MemberExpression (11.2)
pMemberExpression :: JsParser Expression
pMemberExpression = maybePostfix <$> pPrimaryExpression <*> pMaybe pMemberExpressionPost <|>
                    maybePostfix <$> pEFunction <*> pMaybe pMemberExpressionPost <|>
                    maybePostfix <$> pENew <*> pMaybe pMemberExpressionPost

pMemberExpressionPost :: JsParser (Expression -> Expression)
pMemberExpressionPost = maybePostfixC <$> pEIndex <*> pMaybe pMemberExpressionPost <|>
                          maybePostfixC <$> pEDot <*> pMaybe pMemberExpressionPost

pEFunction = EFunction <$ pReserved "function" <*> opt pIdent emptyIdent
                 <*> pPack "(" (pCommaList pIdent) ")"
                 <*> pPack "{" pFunctionBody "}" <?> "function definition"
pENew = ENew <$ pReserved "new" <*> pMemberExpression <*> pArguments <?> "new"
pEIndex = flip EIndex <$> pPack "[" pExpression "]" <?> "index"
pEDot = flip EDot <$ pReserved "." <*> pIdent

-- NewExpression (11.2) (modified)
pNewExpression :: JsParser Expression
pNewExpression = pENewSimple

pENewSimple = (\x -> ENew x []) <$ pReserved "new" <*> pNewExpression <?> "new"

-- CallExpression (11.2) (modified)
pCallExpression :: JsParser Expression
pCallExpression = maybePostfix <$> pCECallMemberExpr <*> pMaybe pCallExpressionPost

pCallExpressionPost :: JsParser (Expression -> Expression)
pCallExpressionPost = maybePostfixC <$> pCECall <*> pMaybe pCallExpressionPost <|>
                        maybePostfixC <$> pCEIndex <*> pMaybe pCallExpressionPost <|>
                        maybePostfixC <$> pCEDot <*> pMaybe pCallExpressionPost

pCECallMemberExpr = maybePostfix <$> pMemberExpression <*> pMaybe (flip ECall <$> pArguments)

pCECall = flip ECall <$> pArguments
pCEIndex = flip EIndex <$> pPack "[" pExpression "]"
pCEDot = flip EDot <$ pReserved "." <*> pIdent

-- LeftHandSideExpression (11.2)
pLeftHandSideExpression :: JsParser Expression
pLeftHandSideExpression = pNewExpression <|> pCallExpression

-- PostFixExpression (11.3)
postfixOps = anyOp [(EPostPlusPlus,"++"),(EPostMinMin,"--")] <?> "postfix operator"

pPostFixExpression :: JsParser Expression
pPostFixExpression = maybePostfix <$> pLeftHandSideExpression <*> (pMaybe postfixOps)

-- UnaryExpression (11.4)
unaryOps = anyOp [ (EDelete,"delete"),(EVoid,"void"),(ETypeOf,"typeof"),
             (EPreInc,"++"),(EPreDec,"--"),(EUnaryPlus,"+"),(EUnaryMin,"-"),
             (EBitNot,"~"),(ELogicNot,"!") ] <?> "unary operator"

pUnaryExpression :: JsParser Expression
pUnaryExpression = (unaryOps <*> pUnaryExpression) <|> pPostFixExpression

-- Infix Operator Expressions (11.5 - 11.11)
infixOpList = [ (ELogicOR,"||"),(ELogicAND,"&&"),(EBitOR,"|"),(EBitXOR,"^"),
                   (EBitAND,"&"),(EEqual,"=="),(ENotEqual,"!="), (EStrictEqual,"==="),
                   (EStrictNotEqual,"!=="),(ELess,"<"), (EGreater,">"),
                   (ELessEqual,"<="),(EGreaterEqual,">="),(EInstanceof,"instanceof"),
                   (EIn,"in"),(ESignedShiftLeft,"<<"),(ESignedShiftRight,">>"),
                   (EUnsignedShiftRight,">>>"),(EAdd,"+"),(ESubtract,"-"),
                   (EModulus,"%"),(EDivide,"/"),(EMultiply,"*") ]

infixOpListNoIn = filter ((/=) "in" . snd) infixOpList

infixOps = anyOp infixOpList <?> "infix operator"
infixOpsNoIn = anyOp infixOpListNoIn <?> "infix operator (excluding in)"

pInfixOpExpression :: Bool -> JsParser Expression
pInfixOpExpression i | i         = infixOps `pChainl` pUnaryExpression
                     | otherwise = infixOpsNoIn `pChainl` pUnaryExpression

-- ConditionalExpression (11.12)
pConditionalExpression :: Bool -> JsParser Expression
pConditionalExpression i = maybePostfix <$> pInfixOpExpression i <*> pMaybe (pEConditional i)

pEConditional i = (\a b p -> EConditional p a b) <$ pReserved "?" <*>
                    pAssignmentExpression' i <* pReserved ":" <*>
                    pAssignmentExpression' i <?> "conditional"

-- AssignmentExpression (11.13)
assignOps = anyOp [(EAssign,"="),(EAssignMultiply,"*="),(EAssignDivide,"/="),(EAssignModulus,"%="),
             (EAssignAdd,"+="),(EAssignSubtract,"-="),(EAssignSignedShiftLeft,"<<="),
             (EAssignSignedShiftRight,">>="),(EAssignUnsignedShiftRight,">>>="),
             (EAssignBitAND,"&="),(EAssignBitXOR,"^="),(EAssignBitOR,"|=")]
             <?> "assignment operator"

pAssignmentExpression' :: Bool -> JsParser Expression
pAssignmentExpression' i = pConditionalExpression i <|> pEAssignment i

pAssignmentExpression = pAssignmentExpression' True

pEAssignment i = (\lhs op expr -> op lhs expr) <$> pLeftHandSideExpression <*>
                   assignOps <*> pAssignmentExpression' i <?> "assignment"

-- Expression (11.14)
pExpression' :: Bool -> JsParser Expression
pExpression' i = pOp (EComma,",") `pChainr` pAssignmentExpression' i

pExpression = pExpression' True

-- Statement (12)
pStatement :: JsParser Statement
pStatement = pBlock <|> pVariableStatement <|> pEmptyStatement <|> pExpressionStatement <|>
               pIfStatement <|> pIterationStatement <|> pContinueStatement <|>
               pContinueStatement <|> pBreakStatement <|> pReturnStatement <|>
               pWithStatement <|> pSwitchStatement <|> pLabelledStatement <|>
               pThrow <|> pTry <|> pDebugger

-- Block (12.1)
pBlock :: JsParser Statement
pBlock = SBlock <$> pPack "{" (pList pStatement) "}"

-- VariableStatement (12.2)
pVariableStatement' :: Bool -> JsParser Statement
pVariableStatement' i = SVariable <$ pReserved "var" <*> pCommaList (pVariableDeclaration i) <* pReserved ";"

pVariableStatement = pVariableStatement' True

-- VariableDeclaration (12.2)
pVariableDeclaration :: Bool -> JsParser (Ident,Maybe Expression)
pVariableDeclaration i = (,) <$> pIdent <*> (pMaybe pInitializer)
  where
    pInitializer = pReserved "=" *> pAssignmentExpression' i

-- EmptyStatement (12.3)
pEmptyStatement :: JsParser Statement
pEmptyStatement = SEmpty <$ pReserved ";"

-- ExpressionStatement (12.4)
pExpressionStatement :: JsParser Statement
pExpressionStatement = SExpression <$> pExpression <* pReserved ";"

-- IfStatement (12.5)
pIfStatement :: JsParser Statement
pIfStatement = SIf <$ pReserved "if" <*> pPack "(" pExpression ")" <*> 
	             pStatement <*> pMaybe (pReserved "else" *> pStatement)

-- IterationStatement (12.6)
pIterationStatement :: JsParser Statement
pIterationStatement = pSDoWhile <|> pSWhile <|> pSFor

pSDoWhile = SDoWhile <$ pReserved "do" <*> pStatement <* pReserved "while" <*>
              pPack "(" pExpression ")"
pSWhile = SWhile <$ pReserved "while" <*> pPack "(" pExpression ")" <*>
              pStatement
pSFor = SFor <$ pReserved "for" <*> pPack "(" pForClause ")" <*> pStatement


pForClause :: JsParser ForClause
pForClause = pFCExprExprExpr <|> pFCVarExprExpr <|> pFCLhsIn <|> pFCVarIn

pFCExprExprExpr = FCExprExprExpr <$> pMaybe (pExpression' False) <* pReserved ";" <*>
                    pMaybe pExpression <* pReserved ";" <*> pMaybe pExpression
pFCVarExprExpr = FCVarExprExpr <$ pReserved "var" <*> pCommaList (pVariableDeclaration False) <*
                   pReserved ";" <*> pMaybe pExpression <* pReserved ";" <*>
                   pMaybe pExpression
pFCLhsIn = FCLhsIn <$> pLeftHandSideExpression <* pReserved "in" <*> pExpression
pFCVarIn = FCVarIn <$ pReserved "var" <*> pVariableDeclaration False <*
             pReserved "in" <*> pExpression

-- ContinueStatement (12.7)
pContinueStatement :: JsParser Statement
pContinueStatement = SContinue <$ pReserved "continue" <*> pMaybe pIdent <* pReserved ";"

-- BreakStatement (12.8)
pBreakStatement :: JsParser Statement
pBreakStatement = SBreak <$ pReserved "break" <*> pMaybe pIdent <* pReserved ";"

-- ReturnStatement (12.9)
pReturnStatement :: JsParser Statement
pReturnStatement = SReturn <$ pReserved "return" <*> pMaybe pExpression <* pReserved ";"

-- WithStatement (12.10)
pWithStatement :: JsParser Statement
pWithStatement = SWith <$ pReserved "with" <*> pPack "(" pExpression ")" <*> pStatement

-- SwitchStatement (12.11)
pSwitchStatement :: JsParser Statement
pSwitchStatement = SSwitch <$ pReserved "switch" <*> pPack "(" pExpression ")" <*>
                       pPack "{" (pList pCaseClause) "}"

pCaseClause :: JsParser CaseClause
pCaseClause = pCCCase <|> pCCDefault

pCCCase = CCCase <$ pReserved "case" <*> pExpression <* pReserved ":" <*> pList pStatement
pCCDefault = CCDefault <$ pReserved "default" <* pReserved ":" <*> pList pStatement

-- LabelledStatement (12.12)
pLabelledStatement :: JsParser Statement
pLabelledStatement = SLabel <$> pIdent <* pReserved ":" <*> pStatement

-- ThrowStatement (12.13)
pThrow :: JsParser Statement
pThrow = SThrow <$ pReserved "throw" <*> pExpression <* pReserved ";"

-- TryStatement (12.14)
pTry :: JsParser Statement
pTry = STry <$ pReserved "try" <*> pBlock <*> pMaybe pCatch <*> pMaybe pFinally

pCatch = (,) <$ pReserved "catch" <*> pPack "(" pIdent ")" <*> pBlock
pFinally = pReserved "finally" *> pBlock

-- DebuggerStatement (12.15)
pDebugger :: JsParser Statement
pDebugger = SDebugger <$ pReserved "debugger" <* pReserved ";"

-- FunctionDeclaration (13)
pFunctionDeclaration :: JsParser SourceElement
pFunctionDeclaration = SEFunctionDecl <$ pReserved "function" <*> pIdent <*>
                         pPack "(" (pCommaList pIdent) ")" <*>
                         pPack "{" (pList pSourceElement) "}"

-- Program (14)
pProgram :: JsParser Program
pProgram = Program <$> pList pSourceElement

-- SourceElement (14)
pSourceElement :: JsParser SourceElement
pSourceElement = pSEStatement <|> pFunctionDeclaration

pSEStatement = SEStatement <$> pStatement

-------------------------------------------------------------------------------
-- Interface
-------------------------------------------------------------------------------

parse :: FilePath -> [Token] -> Either String Program
parse f tks
  = parseTokens (initPos f) pProgram tks

parseTokens :: Pos -> JsParser a -> [Token] -> Either String a
parseTokens pos p tks
  = if null msgs
    then final `seq` Right v
    else Left $ toError pos $ head msgs
  where
    steps = UU.Parsing.parse p tks
    msgs  = getMsgs steps
    (Pair v final) = evalSteps steps

toError :: Pos -> Message Token (Maybe Token) -> String
toError pos (Msg exp mtok _)
  = show p ++ ": " ++ m ++ ". Expecting " ++ show exp
  where
    p = case mtok of
          Nothing -> pos
          Just t  -> position t
    m = case mtok of
          Nothing -> "end of file"
          Just tok -> case tok of
                        Reserved str _         -> "symbol " ++ show str
                        ValToken TkError val _ -> "unrecognized token " ++ show val
                        ValToken tp val _      -> let descr = show tp ++ " "
                                                  in descr ++ show val
