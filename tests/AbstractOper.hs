data Oper =
    Loop LoBound HiBound Incr Body
  | FieldAccess Field [Index]
  | Funtion Name [Args]
  | Scalar Value
  | ArithExpr
  | VarRef Name
  | BinAlt Test Case1 Case2
  | ManyAlt Test [Case]
  | Update [Destruct/Nondestruct] Variable Oper

data Declaration = 
    Declare Name DType Location

data DType =
    DType CType 

data 