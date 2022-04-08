// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy (fun x -> System.Char.IsWhiteSpace x)
    let pletter        = satisfy (fun x -> System.Char.IsLetter x)
    let palphanumeric  = satisfy (fun x -> System.Char.IsLetterOrDigit x)

    let spaces         = many whitespaceChar
    let spaces1        = many1 whitespaceChar

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 .>> spaces >>. p2

    let parenthesise p = pchar('(') >*>. p .>*> pchar(')')

    let curlyBrackets p = pchar('{') >*>. p .>*> pchar('}')
    let pid = pletter <|> pchar('_') .>>. many (palphanumeric <|> pchar('_')) |>> fun (hd, tl) -> hd :: tl |> Array.ofList |> System.String.Concat 
    let unop op a = op .>> many whitespaceChar >>. a
    let binop op a b = (a .>*> op) .>*>. b


    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cref = createParserForwardedToRef<cExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    
    let NegParse = unop(pchar '-') AtomParse |>> (fun x -> Mul(N -1, x) ) <?> "Negation"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse; NegParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let VParse = pid |>> V <?> "String"
    let CITParse = pCharToInt >*>. parenthesise CharParse |>> CharToInt <?> "CharToInt"
    
    let PVParse = pPointValue >*>. ParParse |>> PV <?> "PV"
    do aref := choice [NParse; CITParse; ParParse; PVParse; VParse]     

    let AexpParse = TermParse 

    let CParse = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "Char"
    
    let CVParse = pstring "charValue" >*>. parenthesise TermParse |>> CV <?> "CV"
    
    let TLParse = pstring "ToLower" >*>. parenthesise CParse |>> ToLower <?> "ToLower"
    let TUParse = pstring "ToUpper" >*>. parenthesise CParse |>> ToUpper <?> "ToUpper"
    
    let ITCParse = pstring "IntToChar" >*>. parenthesise TermParse |>> IntToChar <?> "IntToChar"
    do cref := choice[ITCParse; TLParse; TUParse; CVParse; CParse]
    
    let CexpParse = CharParse

    let BexpParse = pstring "not implemented"

    let stmParse = pstring "not implemented"

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
