module Parser where


import CommonParserUtil
import Token
import Expr

import ParserTime

-- | Utility
rule prodRule action              = (prodRule, action, Nothing  )
ruleWithPrec prodRule action prec = (prodRule, action, Just prec)

--
parserSpec :: ParserSpec Token AST IO ()
parserSpec = ParserSpec
  {
    startSymbol = "Program'",

    tokenPrecAssoc = [],

    parserSpecList =
    [
      rule "Program' -> Program"
        (\rhs -> return . get rhs $ 1),
      
      rule "Program -> Decls Comms"
        (\rhs -> return . ASTProg $ Prog (fromASTDecls . get rhs $ 1)
                                         (fromASTComms . get rhs $ 2) ),

      rule "Decls ->"
        (\rhs -> return $ ASTDecls []),

      rule "Decls -> Decl"
        (\rhs -> return $ ASTDecls [  fromASTDecl . get rhs $ 1 ]),

      rule "Decls -> Decl ; Decls"
        (\rhs -> return $ ASTDecls
                            (  (fromASTDecl . get rhs $ 1)
                                 : (fromASTDecls . get rhs $ 3) ) ),

      rule "Decl -> Type TkIdentifier"
        (\rhs -> return . ASTDecl $ (fromASTType . get rhs $ 1, getText rhs 2)),

      rule "Type -> int"
        (\rhs -> return . ASTType $ TyInt),

      rule "Type -> bool"
        (\rhs -> return . ASTType $ TyBool),

      rule "Comms ->"
        (\rhs -> return . ASTComms $ [] ),

      rule "Comms -> Comm"
        (\rhs -> return . ASTComms $ [ fromASTComm . get rhs $ 1 ]),
      
      rule "Comms -> Comm ; Comms"
        (\rhs -> return $ ASTComms
                            ( (fromASTComm . get rhs $ 1)
                                : (fromASTComms . get rhs $ 3) ) ),

      rule "Comm -> skip"
        (\rhs -> return . ASTComm $ CSkip ),
      
      rule "Comm -> { Comms }"
        (\rhs -> return . ASTComm . commsToComm . fromASTComms . get rhs $ 2 ),

      rule "Comm -> TkIdentifier = Expr"
        (\rhs -> return . ASTComm $ CAssign (getText rhs 1) (fromASTExpr . get rhs $ 3) ),
      
      rule "Comm -> read ( TkIdentifier )"
        (\rhs -> return . ASTComm . CRead . getText rhs $ 3),
      
      rule "Comm -> write ( Expr )"
        (\rhs -> return . ASTComm . CWrite . fromASTExpr . get rhs $ 3),
      
      rule "Comm -> if Expr then Comm else Comm"
        (\rhs -> return . ASTComm $ CIf
                                      (fromASTExpr . get rhs $ 2)
                                      (fromASTComm . get rhs $ 4)
                                      (fromASTComm . get rhs $ 6) ),
      
      rule "Comm -> while Expr Comm"
        (\rhs -> return . ASTComm $ CWhile
                                      (fromASTExpr . get rhs $ 2)
                                      (fromASTComm . get rhs $ 3) ),
      
      rule "Comm -> assert Expr"
        (\rhs -> return . ASTComm $ CAssert
                                      (fromASTExpr . get rhs $ 2) ),

      rule "Expr -> OrExpr"
        (\rhs -> return . get rhs $ 1),

      rule "OrExpr -> OrExpr || AndExpr"
        (\rhs -> return . ASTExpr $ EBinOp OpOr
                                     (fromASTExpr . get rhs $ 1)
                                     (fromASTExpr . get rhs $ 3) ),

      rule "OrExpr -> AndExpr"
        (\rhs -> return . get rhs $ 1),

      rule "AndExpr -> AndExpr && EqNeqExpr"
        (\rhs -> return . ASTExpr $ EBinOp OpAnd
                                     (fromASTExpr . get rhs $ 1)
                                     (fromASTExpr . get rhs $ 3) ),

      rule "AndExpr -> EqNeqExpr"
        (\rhs -> return . get rhs $ 1),

      rule "EqNeqExpr -> EqNeqExpr == CompExpr"
        (\rhs -> return . ASTExpr $ EBinOp OpEqual
                                     (fromASTExpr . get rhs $ 1)
                                     (fromASTExpr . get rhs $ 3) ),

      rule "EqNeqExpr -> EqNeqExpr != CompExpr"
        (\rhs -> return . ASTExpr . EUnaryOp OpNot $ EBinOp OpEqual
                                     (fromASTExpr . get rhs $ 1)
                                     (fromASTExpr . get rhs $ 3) ),

      rule "EqNeqExpr -> CompExpr"
        (\rhs -> return . get rhs $ 1),

      rule "CompExpr -> CompExpr < AdditiveExpr"
        (\rhs ->
            let op1 = fromASTExpr . get rhs $ 1
                op2 = fromASTExpr . get rhs $ 3
            in 
            return . ASTExpr $ EBinOp OpLessThan op1 op2),
      
      rule "CompExpr -> CompExpr <= AdditiveExpr"
        (\rhs ->
            let op1 = fromASTExpr . get rhs $ 1
                op2 = fromASTExpr . get rhs $ 3
            in 
            return . ASTExpr $ EBinOp OpOr
                                      (EBinOp OpLessThan op1 op2)
                                      (EBinOp OpEqual op1 op2)),

      rule "CompExpr -> CompExpr > AdditiveExpr"
        (\rhs ->
            let op1 = fromASTExpr . get rhs $ 1
                op2 = fromASTExpr . get rhs $ 3
            in 
            return . ASTExpr $ EBinOp OpLessThan op2 op1),

      rule "CompExpr -> CompExpr >= AdditiveExpr"
        (\rhs ->
            let op1 = fromASTExpr . get rhs $ 1
                op2 = fromASTExpr . get rhs $ 3
            in 
            return . ASTExpr $ EBinOp OpOr
                                      (EBinOp OpLessThan op2 op1)
                                      (EBinOp OpEqual op2 op1)),

      rule "CompExpr -> AdditiveExpr"
        (\rhs -> return . get rhs $ 1),

      rule "AdditiveExpr -> AdditiveExpr + MultiplicativeExpr"
        (\rhs -> return . ASTExpr $ EBinOp OpAdd
                                     (fromASTExpr . get rhs $ 1)
                                     (fromASTExpr . get rhs $ 3) ),

      rule "AdditiveExpr -> AdditiveExpr - MultiplicativeExpr"
        (\rhs -> return . ASTExpr $ EBinOp OpSub
                                     (fromASTExpr . get rhs $ 1)
                                     (fromASTExpr . get rhs $ 3) ),

      rule "AdditiveExpr -> MultiplicativeExpr"
        (\rhs -> return . get rhs $ 1),

      rule "MultiplicativeExpr -> MultiplicativeExpr * UnaryExpr"
        (\rhs -> return . ASTExpr $ EBinOp OpMul
                                     (fromASTExpr . get rhs $ 1)
                                     (fromASTExpr . get rhs $ 3) ),

      rule "MultiplicativeExpr -> MultiplicativeExpr / UnaryExpr"
        (\rhs -> return . ASTExpr $ EBinOp OpDiv
                                     (fromASTExpr . get rhs $ 1)
                                     (fromASTExpr . get rhs $ 3) ),

      rule "MultiplicativeExpr -> MultiplicativeExpr % UnaryExpr"
        (\rhs -> return . ASTExpr $ EBinOp OpMod
                                     (fromASTExpr . get rhs $ 1)
                                     (fromASTExpr . get rhs $ 3) ),
      
      rule "MultiplicativeExpr -> UnaryExpr"
        (\rhs -> return . get rhs $ 1),

      rule "UnaryExpr -> - Primary"
        (\rhs -> return . ASTExpr . EUnaryOp OpSub . fromASTExpr . get rhs $ 2),

      rule "UnaryExpr -> ! Primary"
        (\rhs -> return . ASTExpr . EUnaryOp OpNot . fromASTExpr . get rhs $ 2),
      
      rule "UnaryExpr -> Primary"
        (\rhs -> return . get rhs $ 1),

      rule "Primary -> TkCInt"
        (\rhs -> return . ASTExpr . ECst . CInt . read . getText rhs $ 1),

      rule "Primary -> TkCBool"
        (\rhs -> let bool_lit = case getText rhs $ 1 of
                                  "true"  -> "True"
                                  "false" -> "False"
                                  x       -> x
                 in  return . ASTExpr . ECst . CBool . read $ bool_lit),

      rule "Primary -> TkIdentifier"
        (\rhs -> return . ASTExpr . EVar . getText rhs $ 1),

      rule "Primary -> ( Expr )"
        (\rhs -> return . get rhs $ 2)
    ],
    
    baseDir        = "./",
    actionTblFile  = "action_table_whilelang.txt",
    gotoTblFile    = "goto_table_whilelang.txt",
    grammarFile    = "prod_rules_whilelang.txt",
    parserSpecFile = "mygrammar_whilelang.grm",
    genparserexe   = "yapb-exe",

    synCompSpec = Nothing,
    parserTime = ParserTime {
                   pa_startTime=startTime,
                   pa_finishTime=finishTime
                 }    
  }
