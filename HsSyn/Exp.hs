{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module Exp where

data FractionalLit
data Located a
data Id
data Name
data OccName
data Wrapper
data GlobalRdrEnv
data AmbiguousFieldOcc a
data ByteString
data FastString
data IPName
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

data Lit
  = Char          SourceText Char
  | CharPrim      SourceText Char
  | String        SourceText FastString
  | StringPrim    SourceText ByteString
  | Int           SourceText Integer

  | IntPrim       SourceText Integer
  | WordPrim      SourceText Integer
  | Int64Prim     SourceText Integer
  | Word64Prim    SourceText Integer
  | Integer       SourceText Integer Type
  | Rat           FractionalLit Type
  | FloatPrim     FractionalLit
  | DoublePrim    FractionalLit

data OverLit id
  = OverLit' {
        ol_val :: OverLitVal,
        ol_rebindable :: PostRn id Bool,
        ol_witness :: Exp id,
        ol_type :: PostTc id Type }

data OverLitVal
  = Integral   !SourceText !Integer
  | Fractional !FractionalLit
  | IsString   !SourceText !FastString


type LExp id = Located (Exp id)

type PostTcExp  = Exp Id

type PostTcTable = [(Name, PostTcExp)]

data SyntaxExp id = SyntaxExp { syn_expr      :: Exp id
                                , syn_arg_wraps :: [Wrapper]
                                , syn_res_wrap  :: Wrapper }

type CmdSyntaxTable id = [(Name, Exp id)]

data UnboundVar
  = OutOfScope OccName GlobalRdrEnv
  | TrueExpHole OccName

data Exp id
  = Var     (Located id)

  | UnboundVar UnboundVar

  | RecFld (AmbiguousFieldOcc id)
  | OverLabel FastString
  | IPVar   IPName
  | OverLit (OverLit id)
  | Lit     Lit
  | Lam     (MatchGroup id (LExp id))

  | LamCase (MatchGroup id (LExp id))

  | App     (LExp id) (LExp id)
  | AppType (LExp id) (LWcType id)

  | AppTypeOut (LExp id) (LWcType Name)

  | OpApp       (LExp id)
                (LExp id)
                (PostRn id Fixity)
                (LExp id)

  | NegApp      (LExp id)
                (SyntaxExp id)

  | Par       (LExp id)
  | SectionL    (LExp id)
                (LExp id)
  | SectionR    (LExp id)
                (LExp id)

  | ExplicitTuple
        [LTupArg id]
        Boxity

  | Case      (LExp id)
                (MatchGroup id (LExp id))

  | If        (Maybe (SyntaxExp id))

                (LExp id)
                (LExp id)
                (LExp id)

  | MultiIf   (PostTc id Type) [LGRHS id (LExp id)]

  | Let       (Located (LocalBinds id))
                (LExp  id)

  | Do        (StmtContext Name)

                (Located [ExpLStmt id])
                (PostTc id Type)

  | ExplicitList
                (PostTc id Type)
                (Maybe (SyntaxExp id))
                [LExp id]

  | ExplicitPArr
                (PostTc id Type)
                [LExp id]

  | RecordCon
      { rcon_con_name :: Located id
      , rcon_con_like :: PostTc id ConLike
      , rcon_con_expr :: PostTcExp
      , rcon_flds     :: RecordBinds id }

  | RecordUpd
      { rupd_expr :: LExp id
      , rupd_flds :: [LRecUpdField id]
      , rupd_cons :: PostTc id [ConLike]

      , rupd_in_tys  :: PostTc id [Type]
      , rupd_out_tys :: PostTc id [Type]

      , rupd_wrap :: PostTc id Wrapper
      }

  | ExpWithTySig
                (LExp id)
                (LSigWcType id)
  | ExpWithTySigOut
                (LExp id)
                (LSigWcType Name)

  | ArithSeq
                PostTcExp
                (Maybe (SyntaxExp id))
                (ArithSeqInfo id)

  | PArrSeq
                PostTcExp
                (ArithSeqInfo id)

  | SCC       SourceText
                StringLiteral
                (LExp id)

  | CoreAnn   SourceText
                StringLiteral
                (LExp id)

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

  | Static    (LExp id)

  | ArrApp
        (LExp id)
        (LExp id)
        (PostTc id Type)
        ArrAppType
        Bool

  | ArrForm
        (LExp id)

        (Maybe Fixity)
        [LCmdTop id]

  | Tick
     (Tickish id)
     (LExp id)
  | BinTick
     Int
     Int
     (LExp id)

  | TickPragma
     SourceText
     (StringLiteral,(Int,Int),(Int,Int))
     ((SourceText,SourceText),(SourceText,SourceText))

     (LExp id)

  | EWildPat

  | EAsPat      (Located id)
                (LExp id)

  | EViewPat    (LExp id)
                (LExp id)

  | ELazyPat    (LExp id)

  |  Wrap     Wrapper
                (Exp id)
type LTupArg id = Located (TupArg id)

data TupArg id
  = Present (LExp id)
  | Missing (PostTc id Type)

data LWcTypeX = forall id. OutputableBndr id => LWcTypeX (LWcType id)

type LCmd id = Located (Cmd id)

data Cmd id
  = CmdArrApp
        (LExp id)
        (LExp id)
        (PostTc id Type)
        ArrAppType
        Bool

  | CmdArrForm
        (LExp id)

        (Maybe Fixity)
        [LCmdTop id]
  | CmdApp    (LCmd id)
                (LExp id)
  | CmdLam    (MatchGroup id (LCmd id))

  | CmdPar    (LCmd id)

  | CmdCase   (LExp id)
                (MatchGroup id (LCmd id))

  | CmdIf     (Maybe (SyntaxExp id))
                (LExp id)
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

type RecordBinds id = RecFields id (LExp id)

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
type ExpLStmt  id = LStmt id (LExp id)
type ExpStmt   id = Stmt  id (LExp id)
type GuardLStmt id = LStmt id (LExp id)
type GuardStmt  id = Stmt  id (LExp id)
type GhciLStmt  id = LStmt id (LExp id)
type GhciStmt   id = Stmt  id (LExp id)

data StmtLR idL idR body
  = LastStmt

          body
          Bool
          (SyntaxExp idR)

  | BindStmt (LPat idL)
             body
             (SyntaxExp idR)
             (SyntaxExp idR)

             (PostTc idR Type)

  | ApplicativeStmt
             [ ( SyntaxExp idR
               , ApplicativeArg idL idR) ]
             (Maybe (SyntaxExp idR))
             (PostTc idR Type)
  | BodyStmt body
             (SyntaxExp idR)
             (SyntaxExp idR)
             (PostTc idR Type)

  | LetStmt  (Located (LocalBindsLR idL idR))

  | ParStmt  [ParStmtBlock idL idR]
             (Exp idR)
             (SyntaxExp idR)
             (PostTc idR Type)

  | TransStmt {
      trS_form  :: TransForm,
      trS_stmts :: [ExpLStmt idL],

      trS_bndrs :: [(idR, idR)],
      trS_using :: LExp idR,
      trS_by :: Maybe (LExp idR),

      trS_ret :: SyntaxExp idR,
      trS_bind :: SyntaxExp idR,
      trS_bind_arg_ty :: PostTc idR Type,
      trS_fmap :: Exp idR

    }

  | RecStmt
     { recS_stmts :: [LStmtLR idL idR body]

     , recS_later_ids :: [idR]

     , recS_rec_ids :: [idR]

     , recS_bind_fn :: SyntaxExp idR
     , recS_ret_fn  :: SyntaxExp idR
     , recS_mfix_fn :: SyntaxExp idR
     , recS_bind_ty :: PostTc idR Type

     , recS_later_rets :: [PostTcExp]
     , recS_rec_rets :: [PostTcExp]

      , recS_ret_ty :: PostTc idR Type

      }

data TransForm
  = ThenForm
  | GroupForm

data ParStmtBlock idL idR
  = ParStmtBlock
        [ExpLStmt idL]
        [idR]
        (SyntaxExp idR)

data ApplicativeArg idL idR
  = ApplicativeArgOne
      (LPat idL)
      (LExp idL)
  | ApplicativeArgMany
      [ExpLStmt idL]
      (Exp idL)
      (LPat idL)

data Splice id
   = TypedSplice
        id
        (LExp id)
   | UntypedSplice
        id
        (LExp id)
   | QuasiQuote
        id
        id
        SrcSpan
        FastString

type SplicePointName = Name

data PendingRnSplice
  = PendingRnSplice UntypedSpliceFlavour SplicePointName (LExp Name)

data UntypedSpliceFlavour
  = UntypedExpSplice
  | UntypedPatSplice
  | UntypedTypeSplice
  | UntypedDeclSplice

data PendingTcSplice
  = PendingTcSplice SplicePointName (LExp Id)

data Bracket id = ExpBr (LExp id)
                  | PatBr (LPat id)
                  | DecBrL [LDecl id]
                  | DecBrG (Group id)
                  | TypBr (LType id)
                  | VarBr Bool id
                  | TExpBr (LExp id)

data ArithSeqInfo id
  = From            (LExp id)
  | FromThen        (LExp id)
                    (LExp id)
  | FromTo          (LExp id)
                    (LExp id)
  | FromThenTo      (LExp id)
                    (LExp id)
                    (LExp id)

data MatchContext id
  = FunRhs id
  | LambdaExp
  | CaseAlt
  | IfAlt
  | ProcExp
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
  | DoExp
  | MDoExp
  | ArrowExp
  | GhciStmtCtxt
  | PatGuard (MatchContext id)
  | ParStmtCtxt (StmtContext id)
  | TransStmtCtxt (StmtContext id)
