##Recursive Descent Parser for Pablo Language

###To Run:
```
ghci
:l pablo.hs
parsePablo "<Input char stream>"
```


####Pablo Grammar (in BNF):
```
<stmt> ::= <assign> | <if> | <while>
<assign> ::= <var> "=" <expr>
<if> ::= "if" <expr> ":" {<stmt>} ["else:" {<stmt>}] "."
<while> ::= "while" <expr> ":" {<stmt>} "."

<expr> ::= <term> | <expr> "|" <term> | <expr> "^" <term>
<term> ::= <factor> | <term> "&" <factor>
<factor> ::= "000..." | "111..." | <var> | "(" <expr> ")" | "~" <expr> 
             | "Advance" <expr> <int> | "MatchStar" <expr> <expr>
```
####Variable & Integer tokens as Regular Expressions:
```
<var> ::= [A-Za-z_][A-Za-z_0-9]*
<int> ::= [0-9]+
```
