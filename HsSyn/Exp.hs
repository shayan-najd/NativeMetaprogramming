{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module Expr where

data Located a
data Id
data Name
data OccName
data Wrapper
data GlobalRdrEnv
data AmbiguousFieldOcc a
data FastString
data IPName
data OverLit a
data Lit
data LWcType a
data PostRn a b
data PostTc a b
data Fixity
data Boxity
data Type
data LocalBinds a
data ConLike
data LRecUpdField a
data LSigWcType a
data SourceText
data StringLiteral
data Tickish a
data RecFields a b
data Origin
data LPat a
data LType a
data LocalBindsLR a b
data SrcSpan
data LDecl a
data Group a
class OutputableBndr a


type LExpr id = Located (Expr id)

type PostTcExpr  = Expr Id

type PostTcTable = [(Name, PostTcExpr)]

data SyntaxExpr id = SyntaxExpr { syn_expr      :: Expr id
                                , syn_arg_wraps :: [Wrapper]
                                , syn_res_wrap  :: Wrapper }

type CmdSyntaxTable id = [(Name, Expr id)]

data UnboundVar
  = OutOfScope OccName GlobalRdrEnv
  | TrueExprHole OccName

data Expr id
  = Var     (Located id)

  | UnboundVar UnboundVar

  | RecFld (AmbiguousFieldOcc id)
  | OverLabel FastString
  | IPVar   IPName
  | OverLit (OverLit id)
  | Lit     Lit
  | Lam     (MatchGroup id (LExpr id))

  | LamCase (MatchGroup id (LExpr id))

  | App     (LExpr id) (LExpr id)
  | AppType (LExpr id) (LWcType id)

  | AppTypeOut (LExpr id) (LWcType Name)

  | OpApp       (LExpr id)
                (LExpr id)
                (PostRn id Fixity)
                (LExpr id)

  | NegApp      (LExpr id)
                (SyntaxExpr id)

  | Par       (LExpr id)
  | SectionL    (LExpr id)
                (LExpr id)
  | SectionR    (LExpr id)
                (LExpr id)

  | ExplicitTuple
        [LTupArg id]
        Boxity

  | Case      (LExpr id)
                (MatchGroup id (LExpr id))

  | If        (Maybe (SyntaxExpr id))

                (LExpr id)
                (LExpr id)
                (LExpr id)

  | MultiIf   (PostTc id Type) [LGRHS id (LExpr id)]

  | Let       (Located (LocalBinds id))
                (LExpr  id)

  | Do        (StmtContext Name)

                (Located [ExprLStmt id])
                (PostTc id Type)

  | ExplicitList
                (PostTc id Type)
                (Maybe (SyntaxExpr id))
                [LExpr id]

  | ExplicitPArr
                (PostTc id Type)
                [LExpr id]

  | RecordCon
      { rcon_con_name :: Located id
      , rcon_con_like :: PostTc id ConLike
      , rcon_con_expr :: PostTcExpr
      , rcon_flds     :: RecordBinds id }

  | RecordUpd
      { rupd_expr :: LExpr id
      , rupd_flds :: [LRecUpdField id]
      , rupd_cons :: PostTc id [ConLike]

      , rupd_in_tys  :: PostTc id [Type]
      , rupd_out_tys :: PostTc id [Type]

      , rupd_wrap :: PostTc id Wrapper
      }

  | ExprWithTySig
                (LExpr id)
                (LSigWcType id)
  | ExprWithTySigOut
                (LExpr id)
                (LSigWcType Name)

  | ArithSeq
                PostTcExpr
                (Maybe (SyntaxExpr id))
                (ArithSeqInfo id)

  | PArrSeq
                PostTcExpr
                (ArithSeqInfo id)

  | SCC       SourceText
                StringLiteral
                (LExpr id)

  | CoreAnn   SourceText
                StringLiteral
                (LExpr id)

  | Bracket    (Bracket id)

  | RnBracketOut
      (Bracket Name)
      [PendingRnSplice]
  | TcBracketOut
      (Bracket Name)
      [PendingTcSplice]

  | SpliceE  (Splice id)

  | Proc      (LPat id)
                (LCmdTop id)

  | Static    (LExpr id)

  | ArrApp
        (LExpr id)
        (LExpr id)
        (PostTc id Type)
        ArrAppType
        Bool

  | ArrForm
        (LExpr id)

        (Maybe Fixity)
        [LCmdTop id]

  | Tick
     (Tickish id)
     (LExpr id)
  | BinTick
     Int
     Int
     (LExpr id)

  | TickPragma
     SourceText
     (StringLiteral,(Int,Int),(Int,Int))
     ((SourceText,SourceText),(SourceText,SourceText))

     (LExpr id)

  | EWildPat

  | EAsPat      (Located id)
                (LExpr id)

  | EViewPat    (LExpr id)
                (LExpr id)

  | ELazyPat    (LExpr id)

  |  Wrap     Wrapper
                (Expr id)
type LTupArg id = Located (TupArg id)

data TupArg id
  = Present (LExpr id)
  | Missing (PostTc id Type)

data LWcTypeX = forall id. OutputableBndr id => LWcTypeX (LWcType id)

type LCmd id = Located (Cmd id)

data Cmd id
  = CmdArrApp
        (LExpr id)
        (LExpr id)
        (PostTc id Type)
        ArrAppType
        Bool

  | CmdArrForm
        (LExpr id)

        (Maybe Fixity)
        [LCmdTop id]
  | CmdApp    (LCmd id)
                (LExpr id)
  | CmdLam    (MatchGroup id (LCmd id))

  | CmdPar    (LCmd id)

  | CmdCase   (LExpr id)
                (MatchGroup id (LCmd id))

  | CmdIf     (Maybe (SyntaxExpr id))
                (LExpr id)
                (LCmd id)
                (LCmd id)

  | CmdLet    (Located (LocalBinds id))
                (LCmd  id)

  | CmdDo     (Located [CmdLStmt id])
                (PostTc id Type)

  | CmdWrap   Wrapper
                (Cmd id)

data ArrAppType = HigherOrderApp | FirstOrderApp

type LCmdTop id = Located (CmdTop id)

data CmdTop id
  = CmdTop (LCmd id)
             (PostTc id Type)
             (PostTc id Type)
             (CmdSyntaxTable id)

type RecordBinds id = RecFields id (LExpr id)

data MatchGroup id body
  = MG { mg_alts    :: Located [LMatch id body]
       , mg_arg_tys :: [PostTc id Type]
       , mg_res_ty  :: PostTc id Type
       , mg_origin  :: Origin }

type LMatch id body = Located (Match id body)

data Match id body
  = Match {
        m_fixity :: MatchFixity id,
        m_pats :: [LPat id],
        m_type :: (Maybe (LType id)),

        m_grhss :: (GRHSs id body)
  }

data MatchFixity id
  = NonFunBindMatch
  | FunBindMatch (Located id)
                 Bool

data GRHSs id body
  = GRHSs {
      grhssGRHSs :: [LGRHS id body],
      grhssLocalBinds :: Located (LocalBinds id)
    }

type LGRHS id body = Located (GRHS id body)

data GRHS id body = GRHS [GuardLStmt id]
                         body

type LStmt id body = Located (StmtLR id id body)
type LStmtLR idL idR body = Located (StmtLR idL idR body)
type Stmt id body = StmtLR id id body
type CmdLStmt   id = LStmt id (LCmd  id)
type CmdStmt    id = Stmt  id (LCmd  id)
type ExprLStmt  id = LStmt id (LExpr id)
type ExprStmt   id = Stmt  id (LExpr id)
type GuardLStmt id = LStmt id (LExpr id)
type GuardStmt  id = Stmt  id (LExpr id)
type GhciLStmt  id = LStmt id (LExpr id)
type GhciStmt   id = Stmt  id (LExpr id)

data StmtLR idL idR body
  = LastStmt

          body
          Bool
          (SyntaxExpr idR)

  | BindStmt (LPat idL)
             body
             (SyntaxExpr idR)
             (SyntaxExpr idR)

             (PostTc idR Type)

  | ApplicativeStmt
             [ ( SyntaxExpr idR
               , ApplicativeArg idL idR) ]
             (Maybe (SyntaxExpr idR))
             (PostTc idR Type)
  | BodyStmt body
             (SyntaxExpr idR)
             (SyntaxExpr idR)
             (PostTc idR Type)

  | LetStmt  (Located (LocalBindsLR idL idR))

  | ParStmt  [ParStmtBlock idL idR]
             (Expr idR)
             (SyntaxExpr idR)
             (PostTc idR Type)

  | TransStmt {
      trS_form  :: TransForm,
      trS_stmts :: [ExprLStmt idL],

      trS_bndrs :: [(idR, idR)],
      trS_using :: LExpr idR,
      trS_by :: Maybe (LExpr idR),

      trS_ret :: SyntaxExpr idR,
      trS_bind :: SyntaxExpr idR,
      trS_bind_arg_ty :: PostTc idR Type,
      trS_fmap :: Expr idR

    }

  | RecStmt
     { recS_stmts :: [LStmtLR idL idR body]

     , recS_later_ids :: [idR]

     , recS_rec_ids :: [idR]

     , recS_bind_fn :: SyntaxExpr idR
     , recS_ret_fn  :: SyntaxExpr idR
     , recS_mfix_fn :: SyntaxExpr idR
     , recS_bind_ty :: PostTc idR Type

     , recS_later_rets :: [PostTcExpr]
     , recS_rec_rets :: [PostTcExpr]

      , recS_ret_ty :: PostTc idR Type

      }

data TransForm
  = ThenForm
  | GroupForm

data ParStmtBlock idL idR
  = ParStmtBlock
        [ExprLStmt idL]
        [idR]
        (SyntaxExpr idR)

data ApplicativeArg idL idR
  = ApplicativeArgOne
      (LPat idL)
      (LExpr idL)
  | ApplicativeArgMany
      [ExprLStmt idL]
      (Expr idL)
      (LPat idL)

data Splice id
   = TypedSplice
        id
        (LExpr id)
   | UntypedSplice
        id
        (LExpr id)
   | QuasiQuote
        id
        id
        SrcSpan
        FastString

type SplicePointName = Name

data PendingRnSplice
  = PendingRnSplice UntypedSpliceFlavour SplicePointName (LExpr Name)

data UntypedSpliceFlavour
  = UntypedExpSplice
  | UntypedPatSplice
  | UntypedTypeSplice
  | UntypedDeclSplice

data PendingTcSplice
  = PendingTcSplice SplicePointName (LExpr Id)

data Bracket id = ExpBr (LExpr id)
                  | PatBr (LPat id)
                  | DecBrL [LDecl id]
                  | DecBrG (Group id)
                  | TypBr (LType id)
                  | VarBr Bool id
                  | TExpBr (LExpr id)

data ArithSeqInfo id
  = From            (LExpr id)
  | FromThen        (LExpr id)
                    (LExpr id)
  | FromTo          (LExpr id)
                    (LExpr id)
  | FromThenTo      (LExpr id)
                    (LExpr id)
                    (LExpr id)

data MatchContext id
  = FunRhs id
  | LambdaExpr
  | CaseAlt
  | IfAlt
  | ProcExpr
  | PatBindRhs
  | RecUpd

  | StmtCtxt (StmtContext id)

  | ThPatSplice
  | ThPatQuote
  | PatSyn

data StmtContext id
  = ListComp
  | MonadComp
  | PArrComp
  | DoExpr
  | MDoExpr
  | ArrowExpr
  | GhciStmtCtxt
  | PatGuard (MatchContext id)
  | ParStmtCtxt (StmtContext id)
  | TransStmtCtxt (StmtContext id)
