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
 
