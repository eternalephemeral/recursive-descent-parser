{-
	CMPT 383 - Assignment 2
	Recursive Descent Parser for Pablo Language
	Patrick Wang
-}

data PabloE = All(Int) | Var(String) | And(PabloE, PabloE) | Or(PabloE, PabloE) | Xor(PabloE, PabloE)
               | Not(PabloE) | Advance(PabloE, Int) | MatchStar(PabloE, PabloE)
   deriving Show

data PabloS = Assign (String, PabloE) |  If (PabloE, [PabloS], [PabloS])| While (PabloE, [PabloS])
   deriving Show

data Token = BAD_TOKEN | EQUALS | IF | COLON | ELSE | PERIOD | WHILE | OR | XOR | AND |
			 ALL Int | L_BRACKET | R_BRACKET | NOT | ADVANCE | MATCHSTAR | VAR String | INT String
	deriving Show

---------------------
-----Helper Functions
---------------------

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust other = True

isGoodToken :: Token -> Bool
isGoodToken BAD_TOKEN = False
isGoodToken other = True

isDigit :: Char -> Bool
isDigit c = elem c ['0'..'9']

isAlpha :: Char -> Bool
isAlpha c = elem c (['A'..'Z']++['a'..'z'])

--Returns true if token = [0-9]*
isInt :: [Char] -> Bool
isInt [] = True
isInt x
	| isDigit(head x) = isInt(tail x)
	| otherwise = False

--Returns true if token = [A-Za-z_][A-Za-z_0-9]*
--([Char], Bool) -> Bool
--          |
--      Has first character not been traversed yet
isVar :: ([Char], Bool) -> Bool
isVar ([], _) = True
isVar (x,True)
	| isAlpha(head x) = isVar(tail x, False)
	| (head x) == '_' = isVar(tail x, False)
	| otherwise = False	
isVar (x,False)
	| isAlpha(head x) = isVar(tail x, False)
	| (head x) == '_' = isVar(tail x, False)
	| isDigit(head x) = isVar(tail x, False)
	| otherwise = False

--------------------------
-----Pablo Lexer Functions
--------------------------

--Converts string tokens to their corresponding Token type 
convertToToken :: [Char] -> Token
convertToToken x
	| x == "" = BAD_TOKEN
	| x == "=" = EQUALS
	| x == "if" = IF
	| x == ":" = COLON
	| x == "else" = ELSE
	| x == "." = PERIOD
	| x == "while" = WHILE
	| x == "|" = OR
	| x == "^" = XOR
	| x == "&" = AND
	| x == "000..." = ALL 0
	| x == "111..." = ALL 1
	| x == "(" = L_BRACKET
	| x == ")" = R_BRACKET
	| x == "~" = NOT
	| x == "Advance" = ADVANCE
	| x == "MatchStar" = MATCHSTAR
	| isVar(x, True) = VAR x
	| isInt(x) = INT x
	| otherwise = BAD_TOKEN

--Splits [Char] into the first word and the remaining chars
splitWord :: [Char] -> ([Char], [Char])
splitWord [] = ([], [])
splitWord (' ':xs) = ([], xs)
splitWord (x:[]) = ([x],[])
splitWord (x:xs)
	| head xs == ' ' = ([x], tail xs)
	| otherwise = let (r1,r2) = (splitWord (tail xs)) in (x:[head xs]++r1,r2) 

--Returns the largest token possible (reading left to right) from [Char] that is not a BadToken
--or a BadToken if no GoodToken exists
findLargestToken :: [Char] -> Int -> Int -> (Token, [Char])
findLargestToken x i largest
	| i > length x = (convertToToken(take largest x), if largest == 0 then "" else drop largest x)
	| isGoodToken(convertToToken(take i x)) = findLargestToken x (i+1) i
	| otherwise = findLargestToken x (i+1) largest

--Converts a pablo word into token list
wordLexer :: [Char] -> [Token]
wordLexer x
	| null x = []
	| otherwise = token:wordLexer rest
	where {token = fst result;
		  rest = snd result;
		  result = findLargestToken x 1 0}

--Converts pablo character list into token list
--splits character list into words separated by delimeter ' ' and runs wordLexer on each word
pabloLexer :: [Char] -> [Token]
pabloLexer [] = []
pabloLexer w = (wordLexer word)++(pabloLexer rest)
	where {word = fst result;
		   rest = snd result;
		   result = splitWord w}

---------------------------
-----Pablo Parser Functions
---------------------------

-- <factor> ::= "000..." | "111..." | <var> | "(" <expr> ")" | "~" <expr> | "Advance" <expr> <int> | "MatchStar" <expr> <expr>
parseFactor :: [Token] -> Maybe ([Token], PabloE)
parseFactor (ALL r0:more) = Just (more, All (r0))
parseFactor (VAR r0:more) = Just (more, Var (r0))
parseFactor (L_BRACKET:more) = case parseExpr more of
								Just (more, expr1) -> case more of
														(R_BRACKET:more) -> Just (more, expr1)
														_ -> Nothing
								_ -> Nothing
parseFactor (NOT:more) = case parseExpr more of
						 Just (more, expr1) -> Just (more, Not (expr1))
						 _ -> Nothing
parseFactor (ADVANCE:more) = case parseExpr more of
							 Just (more, expr1) -> case more of
							 						(INT i:more) -> Just (more, Advance (expr1, read i :: Int))
							 						_ -> Nothing
							 _ -> Nothing
parseFactor (MATCHSTAR:more) = case parseExpr more of
								Just (more, expr1) -> case parseExpr more of
														Just (more, expr2) -> Just (more, MatchStar (expr1, expr2))
														_ -> Nothing
								_ -> Nothing
parseFactor _ = Nothing

-- <term> ::= <factor> | <term> "&" <factor>
parseTerm :: [Token] -> Maybe ([Token], PabloE)
parseTerm r0 = case parseFactor r0 of
                Just (more, factor1) -> extendTerm more factor1
                _ -> Nothing

extendTerm :: [Token] -> PabloE -> Maybe (([Token], PabloE))
extendTerm (AND:more) expr1 =
        case parseFactor more  of
                Just (more, term1) -> extendTerm more (And (expr1, term1))
                _ -> Nothing
extendTerm terms expr1 = Just (terms, expr1)

-- <expr> ::= <term> | <expr> "|" <term> | <expr> "^" <term>
parseExpr :: [Token] -> Maybe ([Token], PabloE)
parseExpr r0 = case parseTerm r0 of
				Just (more, term1) -> extendExpr more term1
				_ -> Nothing

extendExpr :: [Token] -> PabloE -> Maybe (([Token], PabloE))
extendExpr (OR:more) term1 =
        case parseTerm more  of
                Just (more, expr1) -> extendExpr more (Or (term1, expr1))
                _ -> Nothing
extendExpr (XOR:more) term1 =
        case parseTerm more  of
                Just (more, expr1) -> extendExpr more (Xor (term1, expr1))
                _ -> Nothing
extendExpr exprs term1 = Just (exprs, term1)

-- <while> ::= "while" <expr> ":" {<stmt>} "."
parseWhile :: [Token] -> Maybe ([Token], PabloS)
parseWhile (WHILE:more)
	| isJust(c1) = let Just (more,expr1) = c1
				   in case more of
				   (COLON:more)
				   		| isJust(c2) -> let Just (more, stmts1) = c2 -- 1 or more stmts
				   						in case more of
				   						(PERIOD:more) -> Just (more, While (expr1, stmts1))
					   				  	_ -> Nothing
				   		| otherwise -> case more of -- 0 stmts
				   					   (PERIOD:more) -> Just (more, While (expr1, []))
					   				   _ -> Nothing
				   		where c2 = (parseStmts more [])
	where c1 = parseExpr more
parseWhile _ = Nothing

-- ["else:" {<stmt>}]
parseElse :: [Token] -> Maybe ([Token], [PabloS])
parseElse (ELSE:COLON:more)
	| isJust(c1) = c1
	where c1 = (parseStmts more [])
parseElse _ = Nothing

-- <if> ::= "if" <expr> ":" {<stmt>} ["else:" {<stmt>}] "."
parseIf :: [Token] -> Maybe ([Token], PabloS)
parseIf (IF:more)
	| isJust(c1) = let Just (more,expr1) = c1 
				   in case more of
				   (COLON:more)
						| isJust(c2) -> let Just (more,stmts1) = c2 -- 1 or more "then stmts"
					   					in case parseElse(more) of
					   				  	Just (more, stmts2) -> case more of
					   				  						 (PERIOD:more) -> Just (more, If (expr1, stmts1, stmts2))
					   				  						 _ -> Nothing
					   				  	Nothing -> case more of
					   				  			   (PERIOD:more) -> Just (more, If (expr1, stmts1, []))
					   				  			   _ -> Nothing
					   	| otherwise -> case parseElse(more) of -- 0 "then stmts"
					   				  	Just (more, stmts2) -> case more of
					   				  						 (PERIOD:more) -> Just (more, If (expr1, [], stmts2))
					   				  						 _ -> Nothing
					   				  	Nothing -> case more of
					   				  				(PERIOD:more) -> Just (more, If (expr1, [], []))
					   				  				_ -> Nothing
						where c2 = (parseStmts more [])
				   _ -> Nothing
	where c1 = parseExpr more
parseIf _ = Nothing

-- <assign> ::= <var> "=" <expr>
parseAssign :: [Token] -> Maybe ([Token], PabloS)
parseAssign (VAR v1:EQUALS:more)
	| isJust(c1) = let Just (more,expr1) = c1
				   in Just (more, Assign (v1, expr1))
	where c1 = parseExpr more
parseAssign _ = Nothing

-- <stmt> ::= <assign> | <if> | <while>
parseStmt :: [Token] -> Maybe ([Token], PabloS)
parseStmt r0
	| isJust(c1) = c1
	| isJust(c2) = c2
    | isJust(c3) = c3
    where {c1 = parseAssign r0;
		   c2 = parseIf r0;
		   c3 = parseWhile r0}
parseStmt _ = Nothing

-- {<stmt>}
parseStmts :: [Token] -> [PabloS] -> Maybe ([Token], [PabloS])
parseStmts r0 [] = case parseStmt r0 of
					Just (more, stmt1) -> parseStmts more [stmt1]
					_ -> Nothing					
parseStmts r0 stmts = case parseStmt r0 of
					Just (more, stmt1) -> parseStmts more (stmts++[stmt1])
					_ -> Just (r0, stmts)



parsePablo :: [Char] -> Maybe [PabloS]
parsePablo x = case (parseStmts (pabloLexer x) []) of
				Just ([], ss) -> Just ss
				_ -> Nothing
