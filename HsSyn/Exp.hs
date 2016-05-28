{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module HsExpr where

data Located a
data Id
data Name
data OccName
data HsWrapper
data GlobalRdrEnv
data AmbiguousFieldOcc a
data FastString
data HsIPName
data HsOverLit a
data HsLit
data LHsWcType a
data PostRn a b
data PostTc a b
data Fixity
data Boxity
data Type
data HsLocalBinds a
data ConLike
data LHsRecUpdField a
data LHsSigWcType a
data SourceText
data StringLiteral
data Tickish a
data HsRecFields a b
data Origin
data LPat a
data LHsType a
data HsLocalBindsLR a b
data SrcSpan
data LHsDecl a
data HsGroup a
class OutputableBndr a


type LHsExpr id = Located (HsExpr id)

type PostTcExpr  = HsExpr Id

type PostTcTable = [(Name, PostTcExpr)]

data SyntaxExpr id = SyntaxExpr { syn_expr      :: HsExpr id
                                , syn_arg_wraps :: [HsWrapper]
                                , syn_res_wrap  :: HsWrapper }

type CmdSyntaxTable id = [(Name, HsExpr id)]

data UnboundVar
  = OutOfScope OccName GlobalRdrEnv
  | TrueExprHole OccName

data HsExpr id
  = HsVar     (Located id)

  | HsUnboundVar UnboundVar

  | HsRecFld (AmbiguousFieldOcc id)
  | HsOverLabel FastString
  | HsIPVar   HsIPName
  | HsOverLit (HsOverLit id)
  | HsLit     HsLit
  | HsLam     (MatchGroup id (LHsExpr id))

  | HsLamCase (MatchGroup id (LHsExpr id))

  | HsApp     (LHsExpr id) (LHsExpr id)
  | HsAppType (LHsExpr id) (LHsWcType id)

  | HsAppTypeOut (LHsExpr id) (LHsWcType Name)

  | OpApp       (LHsExpr id)
                (LHsExpr id)
                (PostRn id Fixity)
                (LHsExpr id)

  | NegApp      (LHsExpr id)
                (SyntaxExpr id)

  | HsPar       (LHsExpr id)
  | SectionL    (LHsExpr id)
                (LHsExpr id)
  | SectionR    (LHsExpr id)
                (LHsExpr id)

  | ExplicitTuple
        [LHsTupArg id]
        Boxity

  | HsCase      (LHsExpr id)
                (MatchGroup id (LHsExpr id))

  | HsIf        (Maybe (SyntaxExpr id))

                (LHsExpr id)
                (LHsExpr id)
                (LHsExpr id)

  | HsMultiIf   (PostTc id Type) [LGRHS id (LHsExpr id)]

  | HsLet       (Located (HsLocalBinds id))
                (LHsExpr  id)

  | HsDo        (HsStmtContext Name)

                (Located [ExprLStmt id])
                (PostTc id Type)

  | ExplicitList
                (PostTc id Type)
                (Maybe (SyntaxExpr id))
                [LHsExpr id]

  | ExplicitPArr
                (PostTc id Type)
                [LHsExpr id]

  | RecordCon
      { rcon_con_name :: Located id
      , rcon_con_like :: PostTc id ConLike
      , rcon_con_expr :: PostTcExpr
      , rcon_flds     :: HsRecordBinds id }

  | RecordUpd
      { rupd_expr :: LHsExpr id
      , rupd_flds :: [LHsRecUpdField id]
      , rupd_cons :: PostTc id [ConLike]

      , rupd_in_tys  :: PostTc id [Type]
      , rupd_out_tys :: PostTc id [Type]

      , rupd_wrap :: PostTc id HsWrapper
      }

  | ExprWithTySig
                (LHsExpr id)
                (LHsSigWcType id)
  | ExprWithTySigOut
                (LHsExpr id)
                (LHsSigWcType Name)

  | ArithSeq
                PostTcExpr
                (Maybe (SyntaxExpr id))
                (ArithSeqInfo id)

  | PArrSeq
                PostTcExpr
                (ArithSeqInfo id)

  | HsSCC       SourceText
                StringLiteral
                (LHsExpr id)

  | HsCoreAnn   SourceText
                StringLiteral
                (LHsExpr id)

  | HsBracket    (HsBracket id)

  | HsRnBracketOut
      (HsBracket Name)
      [PendingRnSplice]
  | HsTcBracketOut
      (HsBracket Name)
      [PendingTcSplice]

  | HsSpliceE  (HsSplice id)

  | HsProc      (LPat id)
                (LHsCmdTop id)

  | HsStatic    (LHsExpr id)

  | HsArrApp
        (LHsExpr id)
        (LHsExpr id)
        (PostTc id Type)
        HsArrAppType
        Bool

  | HsArrForm
        (LHsExpr id)

        (Maybe Fixity)
        [LHsCmdTop id]

  | HsTick
     (Tickish id)
     (LHsExpr id)
  | HsBinTick
     Int
     Int
     (LHsExpr id)

  | HsTickPragma
     SourceText
     (StringLiteral,(Int,Int),(Int,Int))
     ((SourceText,SourceText),(SourceText,SourceText))

     (LHsExpr id)

  | EWildPat

  | EAsPat      (Located id)
                (LHsExpr id)

  | EViewPat    (LHsExpr id)
                (LHsExpr id)

  | ELazyPat    (LHsExpr id)

  |  HsWrap     HsWrapper
                (HsExpr id)
type LHsTupArg id = Located (HsTupArg id)

data HsTupArg id
  = Present (LHsExpr id)
  | Missing (PostTc id Type)

data LHsWcTypeX = forall id. OutputableBndr id => LHsWcTypeX (LHsWcType id)

type LHsCmd id = Located (HsCmd id)

data HsCmd id
  = HsCmdArrApp
        (LHsExpr id)
        (LHsExpr id)
        (PostTc id Type)
        HsArrAppType
        Bool

  | HsCmdArrForm
        (LHsExpr id)

        (Maybe Fixity)
        [LHsCmdTop id]
  | HsCmdApp    (LHsCmd id)
                (LHsExpr id)
  | HsCmdLam    (MatchGroup id (LHsCmd id))

  | HsCmdPar    (LHsCmd id)

  | HsCmdCase   (LHsExpr id)
                (MatchGroup id (LHsCmd id))

  | HsCmdIf     (Maybe (SyntaxExpr id))
                (LHsExpr id)
                (LHsCmd id)
                (LHsCmd id)

  | HsCmdLet    (Located (HsLocalBinds id))
                (LHsCmd  id)

  | HsCmdDo     (Located [CmdLStmt id])
                (PostTc id Type)

  | HsCmdWrap   HsWrapper
                (HsCmd id)

data HsArrAppType = HsHigherOrderApp | HsFirstOrderApp

type LHsCmdTop id = Located (HsCmdTop id)

data HsCmdTop id
  = HsCmdTop (LHsCmd id)
             (PostTc id Type)
             (PostTc id Type)
             (CmdSyntaxTable id)

type HsRecordBinds id = HsRecFields id (LHsExpr id)

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
        m_type :: (Maybe (LHsType id)),

        m_grhss :: (GRHSs id body)
  }

data MatchFixity id
  = NonFunBindMatch
  | FunBindMatch (Located id)
                 Bool

data GRHSs id body
  = GRHSs {
      grhssGRHSs :: [LGRHS id body],
      grhssLocalBinds :: Located (HsLocalBinds id)
    }

type LGRHS id body = Located (GRHS id body)

data GRHS id body = GRHS [GuardLStmt id]
                         body

type LStmt id body = Located (StmtLR id id body)
type LStmtLR idL idR body = Located (StmtLR idL idR body)
type Stmt id body = StmtLR id id body
type CmdLStmt   id = LStmt id (LHsCmd  id)
type CmdStmt    id = Stmt  id (LHsCmd  id)
type ExprLStmt  id = LStmt id (LHsExpr id)
type ExprStmt   id = Stmt  id (LHsExpr id)
type GuardLStmt id = LStmt id (LHsExpr id)
type GuardStmt  id = Stmt  id (LHsExpr id)
type GhciLStmt  id = LStmt id (LHsExpr id)
type GhciStmt   id = Stmt  id (LHsExpr id)

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

  | LetStmt  (Located (HsLocalBindsLR idL idR))

  | ParStmt  [ParStmtBlock idL idR]
             (HsExpr idR)
             (SyntaxExpr idR)
             (PostTc idR Type)

  | TransStmt {
      trS_form  :: TransForm,
      trS_stmts :: [ExprLStmt idL],

      trS_bndrs :: [(idR, idR)],
      trS_using :: LHsExpr idR,
      trS_by :: Maybe (LHsExpr idR),

      trS_ret :: SyntaxExpr idR,
      trS_bind :: SyntaxExpr idR,
      trS_bind_arg_ty :: PostTc idR Type,
      trS_fmap :: HsExpr idR

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
      (LHsExpr idL)
  | ApplicativeArgMany
      [ExprLStmt idL]
      (HsExpr idL)
      (LPat idL)

data HsSplice id
   = HsTypedSplice
        id
        (LHsExpr id)
   | HsUntypedSplice
        id
        (LHsExpr id)
   | HsQuasiQuote
        id
        id
        SrcSpan
        FastString

type SplicePointName = Name

data PendingRnSplice
  = PendingRnSplice UntypedSpliceFlavour SplicePointName (LHsExpr Name)

data UntypedSpliceFlavour
  = UntypedExpSplice
  | UntypedPatSplice
  | UntypedTypeSplice
  | UntypedDeclSplice

data PendingTcSplice
  = PendingTcSplice SplicePointName (LHsExpr Id)

data HsBracket id = ExpBr (LHsExpr id)
                  | PatBr (LPat id)
                  | DecBrL [LHsDecl id]
                  | DecBrG (HsGroup id)
                  | TypBr (LHsType id)
                  | VarBr Bool id
                  | TExpBr (LHsExpr id)

data ArithSeqInfo id
  = From            (LHsExpr id)
  | FromThen        (LHsExpr id)
                    (LHsExpr id)
  | FromTo          (LHsExpr id)
                    (LHsExpr id)
  | FromThenTo      (LHsExpr id)
                    (LHsExpr id)
                    (LHsExpr id)

data HsMatchContext id
  = FunRhs id
  | LambdaExpr
  | CaseAlt
  | IfAlt
  | ProcExpr
  | PatBindRhs
  | RecUpd

  | StmtCtxt (HsStmtContext id)

  | ThPatSplice
  | ThPatQuote
  | PatSyn

data HsStmtContext id
  = ListComp
  | MonadComp
  | PArrComp
  | DoExpr
  | MDoExpr
  | ArrowExpr
  | GhciStmtCtxt
  | PatGuard (HsMatchContext id)
  | ParStmtCtxt (HsStmtContext id)
  | TransStmtCtxt (HsStmtContext id)
