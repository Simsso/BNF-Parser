# BNF-Parser
Haskell server that parses [Backus-Naur form (BNF)](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form) expressions. Only the syntax is being validated, semantics are not regarded. It converts (for instance) the following BNF
```
 <a> ::= <b>'.'
 <b> ::= "b" | 'b'<b>
 ```
 into this JSON representation:
 ```json
 {
    "rules": [
        {
            "name": "a",
            "expr": [
                [
                    { "ref": "b" },
                    { "l": "." }
                ]
            ]
        },
        {
            "name": "b",
            "expr": [
                [
                    { "l": "b" }
                ],
                [
                    { "l": "b" },
                    { "ref": "b" }
                ]
            ]
        }
    ]
}
```
The JSON version is obviously more verbose, but might be easier to handle when it comes to further processing.
 
## Run
You can either run this app with Haskell and Cabal installed or start the Docker container. The latter can be accomplished with the following commands:
```
git clone https://github.com/Simsso/BNF-Parser bnf-parser
cd bnf-parser
docker build . --tag=bnf-parser
docker run -p 80:3000 bnf-parser
```
which will make the API accessible at http://localhost:80. The Swagger specification can be found at `/meta-data/api-spec`.

## BNF Definition
The parser accepts BNFs which satisfy the following definition (taken from the [BNF Wikipedia article](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form)).
```
 <syntax>         ::= <rule> | <rule> <syntax>
 <rule>           ::= <opt-whitespace> "<" <rule-name> ">" <opt-whitespace> "::=" <opt-whitespace> <expression> <line-end>
 <opt-whitespace> ::= " " <opt-whitespace> | ""
 <expression>     ::= <list> | <list> <opt-whitespace> "|" <opt-whitespace> <expression>
 <line-end>       ::= <opt-whitespace> <EOL> | <line-end> <line-end>
 <list>           ::= <term> | <term> <opt-whitespace> <list>
 <term>           ::= <literal> | "<" <rule-name> ">"
 <literal>        ::= '"' <text1> '"' | "'" <text2> "'"
 <text1>          ::= "" | <character1> <text1>
 <text2>          ::= "" | <character2> <text2>
 <character>      ::= <letter> | <digit> | <symbol>
 <letter>         ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
 <digit>          ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
 <symbol>         ::=  "|" | " " | "!" | "#" | "$" | "%" | "&" | "(" | ")" | "*" | "+" | "," | "-" | "." | "/" | ":" | ";" | ">" | "=" | "<" | "?" | "@" | "[" | "\" | "]" | "^" | "_" | "`" | "{" | "}" | "~"
 <character1>     ::= <character> | "'"
 <character2>     ::= <character> | '"'
 <rule-name>      ::= <letter> | <rule-name> <rule-char>
 <rule-char>      ::= <letter> | <digit> | "-"
```