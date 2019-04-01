module A4TermParser where

    import Control.Applicative
    
    -- More imports as you need.
    
    import A4Term
    import ParserLib
    
    -- This can help testing by reading from a file so you can test multi-line input
    -- and also have little hassle with \
    parseFile :: String -> IO (Maybe Term)
    parseFile filename = do
        inp <- readFile filename
        let ans = runParser termParser inp
        return ans
    
    termParser :: Parser Term
    termParser = do
        whitespaces
        t <- block
        eof
        return t
    
    block = cond <|> lambda <|> local <|> binary
    -- local is for let-in; binary is for infix.
    
    cond = do
        terminal "if"
        myTest <- block
        terminal "then"
        myThen <- block
        terminal "else"
        myElse <- block
        return (Cond myTest myThen myElse)
    
    lambda = do
        terminal "\\"
        v <- var
        terminal "->"
        b <- block
        return (Lambda v b)
    
    local = do
        terminal "let"
        eqns <- between (terminal "{") (terminal "}") (many equation)
        terminal "in"
        body <- block
        return (Let eqns body)
    
    equation = do
        v <- var
        terminal "="
        b <- block
        terminal ";"
        return (v, b)
    
    binary = chainr1 test boolop
    
    boolop = fmap Prim2 (terminal "&&" <|> terminal "||")
    
    test = do
        a <- arith
        (do c <- cmp
            b <- arith
            return (Prim2 c a b))
         <|> return a
    
    cmp = terminal "==" <|> terminal "/=" <|> terminal "<=" <|> terminal "<"
    
    arith = chainl1 addend addop
    
    addop = fmap Prim2 (terminal "+" <|> terminal "-")
    
    addend = chainl1 factor mulop
    
    mulop = fmap Prim2 (terminal "*" <|> terminal "/")
    
    factor = fmap (foldl1 App) (some atom)
    
    atom = between (terminal "(") (terminal ")") block
           <|> literal
           <|> fmap Var var
    
    literal = fmap Num integer <|> fmap Bln boolean
    
    boolean = (do terminal "True"
                  return True)
              <|>
              (do terminal "False"
                  return False)
    
    var = identifier ["if", "then", "else", "let", "in", "True", "False"]