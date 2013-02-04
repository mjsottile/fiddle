--
-- simplec
--

module SimpleC where

import Text.PrettyPrint.HughesPJ

csl :: [Doc] -> Doc
csl ds = hsep $ punctuate comma ds

ssl :: [Doc] -> Doc
ssl ds = hsep $ punctuate semi ds

type Symbol = String

pSym :: Symbol -> Doc
pSym s = text s

data ConstExpr =
    SConst String
  | IConst Int
  | FConst Float
  | CConst Char
  deriving (Show, Eq)

pConst :: ConstExpr -> Doc
pConst (SConst s) = doubleQuotes $ text s
pConst (IConst i) = text $ show i
pConst (FConst f) = text $ show f
pConst (CConst c) = quotes $ text [c]

data Expr =
    FunCall Symbol [Expr] -- foo(a,b,c)
  | VarRef Symbol         -- a
  | Constant ConstExpr    -- 14
  | Incr Expr             -- foo++
  | Decr Expr             -- foo--
  | Dot Expr Expr         -- foo.bar
  | Arrow Expr Expr       -- foo->bar
  | Index Expr Expr       -- foo[bar]
  | BinOp BOp Expr Expr   -- a+b
  | UnOp UOp Expr         -- -a
  | Assign AOp Expr Expr  -- a = b
  deriving (Show, Eq)

pExpr :: Expr -> Doc
pExpr (FunCall fsym args) = (pSym fsym)<>(parens $ csl $ map pExpr args)
pExpr (VarRef vsym)       = pSym vsym
pExpr (Constant c)        = pConst c
pExpr (Incr e)            = (pExpr e)<>(text "++")
pExpr (Decr e)            = (pExpr e)<>(text "--")
pExpr (Dot e1 e2)         = (pExpr e1)<>(text ".")<>(pExpr e2)
pExpr (Arrow e1 e2)       = (pExpr e1)<>(text "->")<>(pExpr e2)
pExpr (Index e1 e2)       = (pExpr e1)<>(brackets $ pExpr e2)
pExpr (BinOp bop e1 e2)   = parens $ (pExpr e1)<>(pBOp bop)<>(pExpr e2)
pExpr (UnOp uop e)        = parens $ (pUOp uop)<>(pExpr e)
pExpr (Assign aop e1 e2)  = (pExpr e1)<>(pAOp aop)<>(pExpr e2)

data BOp = 
    BPlus | BMinus | BTimes | BDivide  -- + - * /
  | BAnd | BOr | BXor                  -- && || ^
  | BRem | BLShift | BRShift           -- % << >>
  | BGT | BLT | BGE | BLE | BNE | BEQ  -- > < >= <= != ==
  deriving (Show, Eq)

pBOp :: BOp -> Doc
pBOp op = text $
    case op of
      BPlus   -> "+"
      BMinus  -> "-"
      BTimes  -> "*"
      BDivide -> "/"
      BAnd    -> "&&"
      BOr     -> "||"
      BXor    -> "^"
      BRem    -> "%"
      BLShift -> "<<"
      BRShift -> ">>"
      BGT     -> ">"
      BLT     -> "<"
      BGE     -> ">="
      BLE     -> "<="
      BNE     -> "!="
      BEQ     -> "=="
      --otherwise -> error "Impossible happened in pBOP"

data AOp =
    AEqual | APlus | AMinus | ATimes | ADivide   -- = += -= *= /=
  | AAnd | AOr | AXor | ALShift | ARShift | ARem -- &= |= ^= <<= >>= %=
  deriving (Show, Eq)

pAOp :: AOp -> Doc
pAOp op = text $
    case op of
      AEqual   -> "="
      APlus    -> "+="
      AMinus   -> "-="
      ATimes   -> "*="
      ADivide  -> "/="
      AAnd     -> "&="
      AOr      -> "|="
      AXor     -> "^="
      ALShift  -> "<<="
      ARShift  -> ">>="
      ARem     -> "%="
      --otherwise -> error "Impossible happened in pAOp"

data UOp = UAnd | UStar | UPlus | UMinus | UNot | UBNot
  deriving (Show, Eq)

pUOp :: UOp -> Doc
pUOp op = text $
    case op of
      UAnd   -> "&"
      UStar  -> "*"
      UPlus  -> "+"
      UMinus -> "-"
      UNot   -> "!"
      UBNot  -> "~"
      --otherwise -> error "Impossible happened in pUOp"

data Label =
    LDefault
  | LCase ConstExpr
  deriving (Show, Eq)

pLabel :: Label -> Doc
pLabel LDefault  = text "default:"
pLabel (LCase c) = (text "case")<+>(pConst c)<>(text ":")

data CType =
    TInt
  | TFloat
  | TDouble
  | TChar
  | TBool
  deriving (Show, Eq)

pType :: CType -> Doc
pType TInt = text "int"
pType TFloat = text "float"
pType TDouble = text "double"
pType TChar = text "char"
pType TBool = text "int"

data Stmt =
    CompoundStmt [Stmt]
  | DeclSet CType [(Symbol, Expr)]
  | ExprStmt Expr
  | IfStmt Expr Stmt (Maybe Stmt)
  | Switch Expr [(Label, Stmt)]
  | While Expr Stmt
  | Do Stmt Expr
  | For Expr Expr Expr Stmt
  | Continue
  | Break
  | Return Expr
  deriving (Show, Eq)

pStmt :: Stmt -> Doc
pStmt (CompoundStmt stmts) = (ssl $ map pStmt stmts) <> semi
pStmt (DeclSet t vars) = (ssl $
  map (\(sym,ini) -> pType t<+>(pSym sym)<+>(text "=")<+>(pExpr ini)) vars)
  <> semi
pStmt (ExprStmt e) = (pExpr e) <> semi
pStmt (IfStmt e sthen selse) = 
    (text "if")<+>(parens $ pExpr e)
  <+>(braces $ pStmt sthen)
  <+>(case selse of
       Nothing -> empty
       Just j  -> text "else" <+> (braces $ pStmt j))
pStmt (Switch e ss) = 
    (text "switch")<+>(parens $ pExpr e)<+>
    (braces $ hcat $ 
        map (\(l,s) -> (pLabel l)<+> 
                       (pStmt s)) 
            ss)
pStmt (Do body cond) =
    (text "do")<+>
    (braces $ pStmt body)<+>
    (parens $ pExpr cond)<>
    semi 
pStmt (For ini tst inc body) =
    (text "for")<+>
    (parens $ ssl [pExpr ini, pExpr tst, pExpr inc])<+>
    (braces $ pStmt body)
pStmt (Continue) = text "continue"<>semi
pStmt (Break) = text "break"<>semi
pStmt (Return e) = text "return" <+> (parens $ pExpr e) <> semi

