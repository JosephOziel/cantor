USING: accessors assocs combinators command-line deques io
io.encodings.binary io.files io.streams.string kernel math
math.parser multiline namespaces peg peg.ebnf prettyprint sequences
sequences.deep strings ;
IN: cantor

<PRIVATE

DEFER: parse-cantor

! current is a number, stacks is a hashtable
TUPLE: stacks current stacks ;

: <stacks> ( -- stacks )
    0 H{ } clone stacks boa ;

: get-stack ( stacks -- stacks value )
    dup [ current>> ] [ stacks>> ] bi at V{ } or ;

: set-stack ( stacks value -- stacks )
    over [ current>> ] [ stacks>> ] bi set-at ;

: push-str ( stacks str -- stacks ) 
    swap get-stack rot prefix set-stack ;

:: (#) ( stacks n -- stacks ) 
    stacks get-stack unclip [ set-stack ] dip over [ current>> n + ] [ stacks>> ] bi at V{ } or swap prefix over [ current>> n + ] [ stacks>> ] bi set-at ;

:: (=) ( stacks n -- stacks )
    stacks get-stack dup first [ set-stack ] dip over [ current>> n + ] [ stacks>> ] bi at V{ } or swap prefix over [ current>> n + ] [ stacks>> ] bi set-at ;

: (>) ( stacks n -- stacks )
    '[ _ + ] change-current ;

: (!) ( stacks -- stacks ) 
    get-stack unclip [ set-stack ] dip parse-cantor call( x -- x ) ;

: (+) ( stacks -- stacks ) 
    get-stack unclip swap unclip swapd append prefix set-stack ;

: (^) ( stacks -- stacks ) 
    get-stack unclip write flush set-stack ;

: compose-all ( seq -- quot )
    [ ] [ compose ] reduce ;

! TODO: fix comments. they not working
EBNF: parse-cantor [=[

    spaces = [ \t\n\r]* => [[ drop ignore ]]
    comment = "(" [^()]* ")" => [[ drop ignore ]]
    string2 = "[" ([^[\]]+ | string2)* "]" => [[ flatten [ { { "[" [ CHAR: [ ] } { "]" [ CHAR: ] ] } [ ] } case ] map ]]
    string = ("["~ ([^[\]]+ | string2)* "]"~) => [[ { } [ append ] reduce flatten >string '[ _ push-str ] ]]
    int = ("-"? [0-9]+) => [[ concat string>number ]]
    move = "#"~ spaces int => [[ '[ _ (#) ] ]]
    copy = "="~ spaces int => [[ '[ _ (=) ] ]]
    change = ">"~ spaces int => [[ '[ _ (>) ] ]]
    eval = "!"~ => [[ [ (!) ] ]]
    concat = "+"~ => [[ [ (+) ] ]]
    out = "^"~ => [[ [ (^) ] ]]

    prog = ((string | move | copy | change | eval | concat | out | comment) spaces)* => [[ compose-all ]]
]=]

PRIVATE>

MACRO: run-cantor ( code -- quot )
    parse-cantor '[ <stacks> @ drop ] ;

: get-cantor ( code -- result )
    [ run-cantor ] with-string-writer ; inline

<PRIVATE

: (run-cantor) ( code -- )
    [ <stacks> ] dip parse-cantor call( x -- x ) drop ;

PRIVATE>

: cantor-main ( -- )
    command-line get [
        read-contents (run-cantor)
    ] [
        [ binary file-contents (run-cantor) ] each
    ] if-empty ;

MAIN: cantor-main