{-# LANGUAGE CPP, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module HsExpr where

import HsDecls
import HsPat
import HsLit
import PlaceHolder ( PostTc,PostRn,DataId )
import HsTypes
import HsBinds

import TcEvidence
import CoreSyn
import Var
import DynFlags ( gopt, GeneralFlag(Opt_PrintExplicitCoercions) )
import Name
import RdrName  ( GlobalRdrEnv )
import BasicTypes
import ConLike
import SrcLoc
import Util
import StaticFlags( opt_PprStyle_Debug )
import Outputable
import FastString
import Type

import Data.Data hiding (Fixity)
import Data.Maybe (isNothing)

type LHsExpr id = Located (HsExpr id)

type PostTcExpr  = HsExpr Id

type PostTcTable = [(Name, PostTcExpr)]
noPostTcExpr :: PostTcExpr
noPostTcExpr = HsLit (HsString "" (fsLit "noPostTcExpr"))
noPostTcTable :: PostTcTable
noPostTcTable = []

data SyntaxExpr id = SyntaxExpr { syn_expr      :: HsExpr id
                                , syn_arg_wraps :: [HsWrapper]
                                , syn_res_wrap  :: HsWrapper }
  deriving (Typeable)
deriving instance (DataId id) => Data (SyntaxExpr id)

noExpr :: HsExpr id
noExpr = HsLit (HsString "" (fsLit "noExpr"))
noSyntaxExpr :: SyntaxExpr id
noSyntaxExpr = SyntaxExpr { syn_expr      = HsLit (HsString "" (fsLit "noSyntaxExpr"))
                          , syn_arg_wraps = []
                          , syn_res_wrap  = WpHole }

mkRnSyntaxExpr :: Name -> SyntaxExpr Name
mkRnSyntaxExpr name = SyntaxExpr { syn_expr      = HsVar $ noLoc name
                                 , syn_arg_wraps = []
                                 , syn_res_wrap  = WpHole }

instance OutputableBndr id => Outputable (SyntaxExpr id) where
  ppr (SyntaxExpr { syn_expr      = expr
                  , syn_arg_wraps = arg_wraps
                  , syn_res_wrap  = res_wrap })
    = sdocWithDynFlags $ \ dflags ->
      getPprStyle $ \s ->
      if debugStyle s || gopt Opt_PrintExplicitCoercions dflags
      then ppr expr <> braces (pprWithCommas ppr arg_wraps)
                    <> braces (ppr res_wrap)
      else ppr expr
type CmdSyntaxTable id = [(Name, HsExpr id)]

data UnboundVar
  = OutOfScope OccName GlobalRdrEnv

  | TrueExprHole OccName
  deriving (Data, Typeable)
instance Outputable UnboundVar where
    ppr = ppr . unboundVarOcc
unboundVarOcc :: UnboundVar -> OccName
unboundVarOcc (OutOfScope occ _) = occ
unboundVarOcc (TrueExprHole occ) = occ

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
  deriving (Typeable)
deriving instance (DataId id) => Data (HsExpr id)

type LHsTupArg id = Located (HsTupArg id)

data HsTupArg id
  = Present (LHsExpr id)
  | Missing (PostTc id Type)
  deriving (Typeable)
deriving instance (DataId id) => Data (HsTupArg id)
tupArgPresent :: LHsTupArg id -> Bool
tupArgPresent (L _ (Present {})) = True
tupArgPresent (L _ (Missing {})) = False

instance OutputableBndr id => Outputable (HsExpr id) where
    ppr expr = pprExpr expr

pprLExpr :: OutputableBndr id => LHsExpr id -> SDoc
pprLExpr (L _ e) = pprExpr e
pprExpr :: OutputableBndr id => HsExpr id -> SDoc
pprExpr e | isAtomicHsExpr e || isQuietHsExpr e =            ppr_expr e
          | otherwise                           = pprDeeper (ppr_expr e)
isQuietHsExpr :: HsExpr id -> Bool

isQuietHsExpr (HsPar _)          = True
isQuietHsExpr (HsApp _ _)        = True
isQuietHsExpr (HsAppType _ _)    = True
isQuietHsExpr (HsAppTypeOut _ _) = True
isQuietHsExpr (OpApp _ _ _ _)    = True
isQuietHsExpr _ = False
pprBinds :: (OutputableBndr idL, OutputableBndr idR)
         => HsLocalBindsLR idL idR -> SDoc
pprBinds b = pprDeeper (ppr b)

ppr_lexpr :: OutputableBndr id => LHsExpr id -> SDoc
ppr_lexpr e = ppr_expr (unLoc e)
ppr_expr :: forall id. OutputableBndr id => HsExpr id -> SDoc
ppr_expr (HsVar (L _ v))  = pprPrefixOcc v
ppr_expr (HsUnboundVar uv)= pprPrefixOcc (unboundVarOcc uv)
ppr_expr (HsIPVar v)      = ppr v
ppr_expr (HsOverLabel l)  = char '#' <> ppr l
ppr_expr (HsLit lit)      = ppr lit
ppr_expr (HsOverLit lit)  = ppr lit
ppr_expr (HsPar e)        = parens (ppr_lexpr e)
ppr_expr (HsCoreAnn _ (StringLiteral _ s) e)
  = vcat [text "HsCoreAnn" <+> ftext s, ppr_lexpr e]
ppr_expr e@(HsApp {})        = ppr_apps e []
ppr_expr e@(HsAppType {})    = ppr_apps e []
ppr_expr e@(HsAppTypeOut {}) = ppr_apps e []
ppr_expr (OpApp e1 op _ e2)
  = case unLoc op of
      HsVar (L _ v) -> pp_infixly v
      HsRecFld f    -> pp_infixly f
      _             -> pp_prefixly
  where
    pp_e1 = pprDebugParendExpr e1
    pp_e2 = pprDebugParendExpr e2
    pp_prefixly
      = hang (ppr op) 2 (sep [pp_e1, pp_e2])
    pp_infixly v
      = sep [pp_e1, sep [pprInfixOcc v, nest 2 pp_e2]]
ppr_expr (NegApp e _) = char '-' <+> pprDebugParendExpr e
ppr_expr (SectionL expr op)
  = case unLoc op of
      HsVar (L _ v) -> pp_infixly v
      _             -> pp_prefixly
  where
    pp_expr = pprDebugParendExpr expr
    pp_prefixly = hang (hsep [text " \\ x_ ->", ppr op])
                       4 (hsep [pp_expr, text "x_ )"])
    pp_infixly v = (sep [pp_expr, pprInfixOcc v])
ppr_expr (SectionR op expr)
  = case unLoc op of
      HsVar (L _ v) -> pp_infixly v
      _             -> pp_prefixly
  where
    pp_expr = pprDebugParendExpr expr
    pp_prefixly = hang (hsep [text "( \\ x_ ->", ppr op, text "x_"])
                       4 (pp_expr <> rparen)
    pp_infixly v = sep [pprInfixOcc v, pp_expr]
ppr_expr (ExplicitTuple exprs boxity)
  = tupleParens (boxityTupleSort boxity) (fcat (ppr_tup_args $ map unLoc exprs))
  where
    ppr_tup_args []               = []
    ppr_tup_args (Present e : es) = (ppr_lexpr e <> punc es) : ppr_tup_args es
    ppr_tup_args (Missing _ : es) = punc es : ppr_tup_args es
    punc (Present {} : _) = comma <> space
    punc (Missing {} : _) = comma
    punc []               = empty
ppr_expr (HsLam matches)
  = pprMatches (LambdaExpr :: HsMatchContext id) matches
ppr_expr (HsLamCase matches)
  = sep [ sep [text "\\case {"],
          nest 2 (pprMatches (CaseAlt :: HsMatchContext id) matches <+> char '}') ]
ppr_expr (HsCase expr matches)
  = sep [ sep [text "case", nest 4 (ppr expr), ptext (sLit "of {")],
          nest 2 (pprMatches (CaseAlt :: HsMatchContext id) matches <+> char '}') ]
ppr_expr (HsIf _ e1 e2 e3)
  = sep [hsep [text "if", nest 2 (ppr e1), ptext (sLit "then")],
         nest 4 (ppr e2),
         text "else",
         nest 4 (ppr e3)]
ppr_expr (HsMultiIf _ alts)
  = sep $ text "if" : map ppr_alt alts
  where ppr_alt (L _ (GRHS guards expr)) =
          sep [ vbar <+> interpp'SP guards
              , text "->" <+> pprDeeper (ppr expr) ]

ppr_expr (HsLet (L _ binds) expr@(L _ (HsLet _ _)))
  = sep [hang (text "let") 2 (hsep [pprBinds binds, ptext (sLit "in")]),
         ppr_lexpr expr]
ppr_expr (HsLet (L _ binds) expr)
  = sep [hang (text "let") 2 (pprBinds binds),
         hang (text "in")  2 (ppr expr)]
ppr_expr (HsDo do_or_list_comp (L _ stmts) _) = pprDo do_or_list_comp stmts
ppr_expr (ExplicitList _ _ exprs)
  = brackets (pprDeeperList fsep (punctuate comma (map ppr_lexpr exprs)))
ppr_expr (ExplicitPArr _ exprs)
  = paBrackets (pprDeeperList fsep (punctuate comma (map ppr_lexpr exprs)))
ppr_expr (RecordCon { rcon_con_name = con_id, rcon_flds = rbinds })
  = hang (ppr con_id) 2 (ppr rbinds)
ppr_expr (RecordUpd { rupd_expr = L _ aexp, rupd_flds = rbinds })
  = hang (pprParendExpr aexp) 2 (braces (fsep (punctuate comma (map ppr rbinds))))
ppr_expr (ExprWithTySig expr sig)
  = hang (nest 2 (ppr_lexpr expr) <+> dcolon)
         4 (ppr sig)
ppr_expr (ExprWithTySigOut expr sig)
  = hang (nest 2 (ppr_lexpr expr) <+> dcolon)
         4 (ppr sig)
ppr_expr (ArithSeq _ _ info) = brackets (ppr info)
ppr_expr (PArrSeq  _ info) = paBrackets (ppr info)
ppr_expr EWildPat       = char '_'
ppr_expr (ELazyPat e)   = char '~' <> pprParendLExpr e
ppr_expr (EAsPat v e)   = ppr v <> char '@' <> pprParendLExpr e
ppr_expr (EViewPat p e) = ppr p <+> text "->" <+> ppr e
ppr_expr (HsSCC _ (StringLiteral _ lbl) expr)
  = sep [ text "{-# SCC" <+> doubleQuotes (ftext lbl) <+> ptext (sLit "#-}"),
          pprParendLExpr expr ]
ppr_expr (HsWrap co_fn e)
  = pprHsWrapper co_fn (\parens -> if parens then pprParendExpr e
                                             else pprExpr       e)
ppr_expr (HsSpliceE s)         = pprSplice s
ppr_expr (HsBracket b)         = pprHsBracket b
ppr_expr (HsRnBracketOut e []) = ppr e
ppr_expr (HsRnBracketOut e ps) = ppr e $$ text "pending(rn)" <+> ppr ps
ppr_expr (HsTcBracketOut e []) = ppr e
ppr_expr (HsTcBracketOut e ps) = ppr e $$ text "pending(tc)" <+> ppr ps
ppr_expr (HsProc pat (L _ (HsCmdTop cmd _ _ _)))
  = hsep [text "proc", ppr pat, ptext (sLit "->"), ppr cmd]
ppr_expr (HsStatic e)
  = hsep [text "static", pprParendLExpr e]
ppr_expr (HsTick tickish exp)
  = pprTicks (ppr exp) $
    ppr tickish <+> ppr_lexpr exp
ppr_expr (HsBinTick tickIdTrue tickIdFalse exp)
  = pprTicks (ppr exp) $
    hcat [text "bintick<",
          ppr tickIdTrue,
          text ",",
          ppr tickIdFalse,
          text ">(",
          ppr exp, text ")"]
ppr_expr (HsTickPragma _ externalSrcLoc _ exp)
  = pprTicks (ppr exp) $
    hcat [text "tickpragma<",
          pprExternalSrcLoc externalSrcLoc,
          text ">(",
          ppr exp,
          text ")"]
ppr_expr (HsArrApp arrow arg _ HsFirstOrderApp True)
  = hsep [ppr_lexpr arrow, larrowt, ppr_lexpr arg]
ppr_expr (HsArrApp arrow arg _ HsFirstOrderApp False)
  = hsep [ppr_lexpr arg, arrowt, ppr_lexpr arrow]
ppr_expr (HsArrApp arrow arg _ HsHigherOrderApp True)
  = hsep [ppr_lexpr arrow, larrowtt, ppr_lexpr arg]
ppr_expr (HsArrApp arrow arg _ HsHigherOrderApp False)
  = hsep [ppr_lexpr arg, arrowtt, ppr_lexpr arrow]
ppr_expr (HsArrForm (L _ (HsVar (L _ v))) (Just _) [arg1, arg2])
  = sep [pprCmdArg (unLoc arg1), hsep [pprInfixOcc v, pprCmdArg (unLoc arg2)]]
ppr_expr (HsArrForm op _ args)
  = hang (text "(|" <+> ppr_lexpr op)
         4 (sep (map (pprCmdArg.unLoc) args) <+> text "|)")
ppr_expr (HsRecFld f) = ppr f

data LHsWcTypeX = forall id. OutputableBndr id => LHsWcTypeX (LHsWcType id)
ppr_apps :: OutputableBndr id
         => HsExpr id
         -> [Either (LHsExpr id) LHsWcTypeX]
         -> SDoc
ppr_apps (HsApp (L _ fun) arg)        args
  = ppr_apps fun (Left arg : args)
ppr_apps (HsAppType (L _ fun) arg)    args
  = ppr_apps fun (Right (LHsWcTypeX arg) : args)
ppr_apps (HsAppTypeOut (L _ fun) arg) args
  = ppr_apps fun (Right (LHsWcTypeX arg) : args)
ppr_apps fun args = hang (ppr_expr fun) 2 (sep (map pp args))
  where
    pp (Left arg)                             = pprParendLExpr arg
    pp (Right (LHsWcTypeX (HsWC { hswc_body = L _ arg })))
      = char '@' <> pprParendHsType arg
pprExternalSrcLoc :: (StringLiteral,(Int,Int),(Int,Int)) -> SDoc
pprExternalSrcLoc (StringLiteral _ src,(n1,n2),(n3,n4))
  = ppr (src,(n1,n2),(n3,n4))

pprDebugParendExpr :: OutputableBndr id => LHsExpr id -> SDoc
pprDebugParendExpr expr
  = getPprStyle (\sty ->
    if debugStyle sty then pprParendLExpr expr
                      else pprLExpr      expr)
pprParendLExpr :: OutputableBndr id => LHsExpr id -> SDoc
pprParendLExpr (L _ e) = pprParendExpr e
pprParendExpr :: OutputableBndr id => HsExpr id -> SDoc
pprParendExpr expr
  | hsExprNeedsParens expr = parens (pprExpr expr)
  | otherwise              = pprExpr expr

hsExprNeedsParens :: HsExpr id -> Bool

hsExprNeedsParens (ArithSeq {})       = False
hsExprNeedsParens (PArrSeq {})        = False
hsExprNeedsParens (HsLit {})          = False
hsExprNeedsParens (HsOverLit {})      = False
hsExprNeedsParens (HsVar {})          = False
hsExprNeedsParens (HsUnboundVar {})   = False
hsExprNeedsParens (HsIPVar {})        = False
hsExprNeedsParens (HsOverLabel {})    = False
hsExprNeedsParens (ExplicitTuple {})  = False
hsExprNeedsParens (ExplicitList {})   = False
hsExprNeedsParens (ExplicitPArr {})   = False
hsExprNeedsParens (HsPar {})          = False
hsExprNeedsParens (HsBracket {})      = False
hsExprNeedsParens (HsRnBracketOut {}) = False
hsExprNeedsParens (HsTcBracketOut {}) = False
hsExprNeedsParens (HsDo sc _ _)
       | isListCompExpr sc            = False
hsExprNeedsParens (HsRecFld{})        = False
hsExprNeedsParens _ = True

isAtomicHsExpr :: HsExpr id -> Bool
isAtomicHsExpr (HsVar {})        = True
isAtomicHsExpr (HsLit {})        = True
isAtomicHsExpr (HsOverLit {})    = True
isAtomicHsExpr (HsIPVar {})      = True
isAtomicHsExpr (HsOverLabel {})  = True
isAtomicHsExpr (HsUnboundVar {}) = True
isAtomicHsExpr (HsWrap _ e)      = isAtomicHsExpr e
isAtomicHsExpr (HsPar e)         = isAtomicHsExpr (unLoc e)
isAtomicHsExpr (HsRecFld{})      = True
isAtomicHsExpr _                 = False

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

  deriving (Typeable)
deriving instance (DataId id) => Data (HsCmd id)
data HsArrAppType = HsHigherOrderApp | HsFirstOrderApp
  deriving (Data, Typeable)

type LHsCmdTop id = Located (HsCmdTop id)
data HsCmdTop id
  = HsCmdTop (LHsCmd id)
             (PostTc id Type)
             (PostTc id Type)
             (CmdSyntaxTable id)
  deriving (Typeable)
deriving instance (DataId id) => Data (HsCmdTop id)
instance OutputableBndr id => Outputable (HsCmd id) where
    ppr cmd = pprCmd cmd

pprLCmd :: OutputableBndr id => LHsCmd id -> SDoc
pprLCmd (L _ c) = pprCmd c
pprCmd :: OutputableBndr id => HsCmd id -> SDoc
pprCmd c | isQuietHsCmd c =            ppr_cmd c
         | otherwise      = pprDeeper (ppr_cmd c)
isQuietHsCmd :: HsCmd id -> Bool

isQuietHsCmd (HsCmdPar _) = True
isQuietHsCmd (HsCmdApp _ _) = True
isQuietHsCmd _ = False

ppr_lcmd :: OutputableBndr id => LHsCmd id -> SDoc
ppr_lcmd c = ppr_cmd (unLoc c)
ppr_cmd :: forall id. OutputableBndr id => HsCmd id -> SDoc
ppr_cmd (HsCmdPar c) = parens (ppr_lcmd c)
ppr_cmd (HsCmdApp c e)
  = let (fun, args) = collect_args c [e] in
    hang (ppr_lcmd fun) 2 (sep (map pprParendLExpr args))
  where
    collect_args (L _ (HsCmdApp fun arg)) args = collect_args fun (arg:args)
    collect_args fun args = (fun, args)
ppr_cmd (HsCmdLam matches)
  = pprMatches (LambdaExpr :: HsMatchContext id) matches
ppr_cmd (HsCmdCase expr matches)
  = sep [ sep [text "case", nest 4 (ppr expr), ptext (sLit "of {")],
          nest 2 (pprMatches (CaseAlt :: HsMatchContext id) matches <+> char '}') ]
ppr_cmd (HsCmdIf _ e ct ce)
  = sep [hsep [text "if", nest 2 (ppr e), ptext (sLit "then")],
         nest 4 (ppr ct),
         text "else",
         nest 4 (ppr ce)]

ppr_cmd (HsCmdLet (L _ binds) cmd@(L _ (HsCmdLet _ _)))
  = sep [hang (text "let") 2 (hsep [pprBinds binds, ptext (sLit "in")]),
         ppr_lcmd cmd]
ppr_cmd (HsCmdLet (L _ binds) cmd)
  = sep [hang (text "let") 2 (pprBinds binds),
         hang (text "in")  2 (ppr cmd)]
ppr_cmd (HsCmdDo (L _ stmts) _)  = pprDo ArrowExpr stmts
ppr_cmd (HsCmdWrap w cmd)
  = pprHsWrapper w (\_ -> parens (ppr_cmd cmd))
ppr_cmd (HsCmdArrApp arrow arg _ HsFirstOrderApp True)
  = hsep [ppr_lexpr arrow, larrowt, ppr_lexpr arg]
ppr_cmd (HsCmdArrApp arrow arg _ HsFirstOrderApp False)
  = hsep [ppr_lexpr arg, arrowt, ppr_lexpr arrow]
ppr_cmd (HsCmdArrApp arrow arg _ HsHigherOrderApp True)
  = hsep [ppr_lexpr arrow, larrowtt, ppr_lexpr arg]
ppr_cmd (HsCmdArrApp arrow arg _ HsHigherOrderApp False)
  = hsep [ppr_lexpr arg, arrowtt, ppr_lexpr arrow]
ppr_cmd (HsCmdArrForm (L _ (HsVar (L _ v))) (Just _) [arg1, arg2])
  = sep [pprCmdArg (unLoc arg1), hsep [pprInfixOcc v, pprCmdArg (unLoc arg2)]]
ppr_cmd (HsCmdArrForm op _ args)
  = hang (text "(|" <> ppr_lexpr op)
         4 (sep (map (pprCmdArg.unLoc) args) <> text "|)")
pprCmdArg :: OutputableBndr id => HsCmdTop id -> SDoc
pprCmdArg (HsCmdTop cmd@(L _ (HsCmdArrForm _ Nothing [])) _ _ _)
  = ppr_lcmd cmd
pprCmdArg (HsCmdTop cmd _ _ _)
  = parens (ppr_lcmd cmd)
instance OutputableBndr id => Outputable (HsCmdTop id) where
    ppr = pprCmdArg

type HsRecordBinds id = HsRecFields id (LHsExpr id)

data MatchGroup id body
  = MG { mg_alts    :: Located [LMatch id body]
       , mg_arg_tys :: [PostTc id Type]
       , mg_res_ty  :: PostTc id Type
       , mg_origin  :: Origin }

  deriving (Typeable)
deriving instance (Data body,DataId id) => Data (MatchGroup id body)
type LMatch id body = Located (Match id body)

data Match id body
  = Match {
        m_fixity :: MatchFixity id,
        m_pats :: [LPat id],
        m_type :: (Maybe (LHsType id)),

        m_grhss :: (GRHSs id body)
  } deriving (Typeable)
deriving instance (Data body,DataId id) => Data (Match id body)

data MatchFixity id
  = NonFunBindMatch
  | FunBindMatch (Located id)
                 Bool
  deriving (Typeable)
deriving instance (DataId id) => Data (MatchFixity id)
isInfixMatch :: Match id body -> Bool
isInfixMatch match = case m_fixity match of
  FunBindMatch _ True -> True
  _                   -> False
isEmptyMatchGroup :: MatchGroup id body -> Bool
isEmptyMatchGroup (MG { mg_alts = ms }) = null $ unLoc ms

isSingletonMatchGroup :: [LMatch id body] -> Bool
isSingletonMatchGroup matches
  | [L _ match] <- matches
  , Match { m_grhss = GRHSs { grhssGRHSs = [_] } } <- match
  = True
  | otherwise
  = False
matchGroupArity :: MatchGroup id body -> Arity

matchGroupArity (MG { mg_alts = alts })
  | L _ (alt1:_) <- alts = length (hsLMatchPats alt1)
  | otherwise        = panic "matchGroupArity"
hsLMatchPats :: LMatch id body -> [LPat id]
hsLMatchPats (L _ (Match _ pats _ _)) = pats

data GRHSs id body
  = GRHSs {
      grhssGRHSs :: [LGRHS id body],
      grhssLocalBinds :: Located (HsLocalBinds id)
    } deriving (Typeable)
deriving instance (Data body,DataId id) => Data (GRHSs id body)
type LGRHS id body = Located (GRHS id body)

data GRHS id body = GRHS [GuardLStmt id]
                         body
  deriving (Typeable)
deriving instance (Data body,DataId id) => Data (GRHS id body)

pprMatches :: (OutputableBndr idL, OutputableBndr idR, Outputable body)
           => HsMatchContext idL -> MatchGroup idR body -> SDoc
pprMatches ctxt (MG { mg_alts = matches })
    = vcat (map (pprMatch ctxt) (map unLoc (unLoc matches)))

pprFunBind :: (OutputableBndr idL, OutputableBndr idR, Outputable body)
           => idL -> MatchGroup idR body -> SDoc
pprFunBind fun matches = pprMatches (FunRhs fun) matches

pprPatBind :: forall bndr id body. (OutputableBndr bndr, OutputableBndr id, Outputable body)
           => LPat bndr -> GRHSs id body -> SDoc
pprPatBind pat (grhss)
 = sep [ppr pat, nest 2 (pprGRHSs (PatBindRhs :: HsMatchContext id) grhss)]
pprMatch :: (OutputableBndr idL, OutputableBndr idR, Outputable body)
         => HsMatchContext idL -> Match idR body -> SDoc
pprMatch ctxt match
  = sep [ sep (herald : map (nest 2 . pprParendLPat) other_pats)
        , nest 2 ppr_maybe_ty
        , nest 2 (pprGRHSs ctxt (m_grhss match)) ]
  where
    is_infix = isInfixMatch match
    (herald, other_pats)
        = case ctxt of
            FunRhs fun
                | not is_infix -> (pprPrefixOcc fun, m_pats match)

                | null pats2 -> (pp_infix, [])

                | otherwise -> (parens pp_infix, pats2)
                where
                  pp_infix = pprParendLPat pat1 <+> pprInfixOcc fun <+> pprParendLPat pat2
            LambdaExpr -> (char '\\', m_pats match)
            _  -> ASSERT( null pats1 )
                  (ppr pat1, [])
    (pat1:pats1) = m_pats match
    (pat2:pats2) = pats1
    ppr_maybe_ty = case m_type match of
                        Just ty -> dcolon <+> ppr ty
                        Nothing -> empty

pprGRHSs :: (OutputableBndr idR, Outputable body)
         => HsMatchContext idL -> GRHSs idR body -> SDoc
pprGRHSs ctxt (GRHSs grhss (L _ binds))
  = vcat (map (pprGRHS ctxt . unLoc) grhss)
 $$ ppUnless (isEmptyLocalBinds binds)
      (text "where" $$ nest 4 (pprBinds binds))
pprGRHS :: (OutputableBndr idR, Outputable body)
        => HsMatchContext idL -> GRHS idR body -> SDoc
pprGRHS ctxt (GRHS [] body)
 =  pp_rhs ctxt body
pprGRHS ctxt (GRHS guards body)
 = sep [vbar <+> interpp'SP guards, pp_rhs ctxt body]
pp_rhs :: Outputable body => HsMatchContext idL -> body -> SDoc
pp_rhs ctxt rhs = matchSeparator ctxt <+> pprDeeper (ppr rhs)

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
  deriving (Typeable)
deriving instance (Data body, DataId idL, DataId idR)
  => Data (StmtLR idL idR body)
data TransForm
  = ThenForm
  | GroupForm
  deriving (Data, Typeable)
data ParStmtBlock idL idR
  = ParStmtBlock
        [ExprLStmt idL]
        [idR]
        (SyntaxExpr idR)
  deriving( Typeable )
deriving instance (DataId idL, DataId idR) => Data (ParStmtBlock idL idR)
data ApplicativeArg idL idR
  = ApplicativeArgOne
      (LPat idL)
      (LHsExpr idL)
  | ApplicativeArgMany
      [ExprLStmt idL]
      (HsExpr idL)
      (LPat idL)
  deriving( Typeable )
deriving instance (DataId idL, DataId idR) => Data (ApplicativeArg idL idR)

instance (OutputableBndr idL)
      => Outputable (ParStmtBlock idL idR) where
  ppr (ParStmtBlock stmts _ _) = interpp'SP stmts
instance (OutputableBndr idL, OutputableBndr idR, Outputable body)
         => Outputable (StmtLR idL idR body) where
    ppr stmt = pprStmt stmt
pprStmt :: forall idL idR body . (OutputableBndr idL, OutputableBndr idR, Outputable body)
        => (StmtLR idL idR body) -> SDoc
pprStmt (LastStmt expr ret_stripped _)
  = ifPprDebug (text "[last]") <+>
       (if ret_stripped then text "return" else empty) <+>
       ppr expr
pprStmt (BindStmt pat expr _ _ _) = hsep [ppr pat, larrow, ppr expr]
pprStmt (LetStmt (L _ binds))     = hsep [text "let", pprBinds binds]
pprStmt (BodyStmt expr _ _ _)     = ppr expr
pprStmt (ParStmt stmtss _ _ _)    = sep (punctuate (text " | ") (map ppr stmtss))
pprStmt (TransStmt { trS_stmts = stmts, trS_by = by, trS_using = using, trS_form = form })
  = sep $ punctuate comma (map ppr stmts ++ [pprTransStmt by using form])
pprStmt (RecStmt { recS_stmts = segment, recS_rec_ids = rec_ids
                 , recS_later_ids = later_ids })
  = text "rec" <+>
    vcat [ ppr_do_stmts segment
         , ifPprDebug (vcat [ text "rec_ids=" <> ppr rec_ids
                            , text "later_ids=" <> ppr later_ids])]
pprStmt (ApplicativeStmt args mb_join _)
  = getPprStyle $ \style ->
      if userStyle style
         then pp_for_user
         else pp_debug
  where

   pp_for_user = vcat $ punctuate semi $ concatMap flattenArg args

   flattenStmt :: ExprLStmt idL -> [SDoc]
   flattenStmt (L _ (ApplicativeStmt args _ _)) = concatMap flattenArg args
   flattenStmt stmt = [ppr stmt]
   flattenArg (_, ApplicativeArgOne pat expr) =
     [ppr (BindStmt pat expr noSyntaxExpr noSyntaxExpr (panic "pprStmt")
             :: ExprStmt idL)]
   flattenArg (_, ApplicativeArgMany stmts _ _) =
     concatMap flattenStmt stmts
   pp_debug =
     let
         ap_expr = sep (punctuate (text " |") (map pp_arg args))
     in
       if isNothing mb_join
          then ap_expr
          else text "join" <+> parens ap_expr
   pp_arg (_, ApplicativeArgOne pat expr) =
     ppr (BindStmt pat expr noSyntaxExpr noSyntaxExpr (panic "pprStmt")
            :: ExprStmt idL)
   pp_arg (_, ApplicativeArgMany stmts return pat) =
     ppr pat <+>
     text "<-" <+>
     ppr (HsDo DoExpr (noLoc
                (stmts ++ [noLoc (LastStmt (noLoc return) False noSyntaxExpr)]))
           (error "pprStmt"))
pprTransformStmt :: OutputableBndr id => [id] -> LHsExpr id -> Maybe (LHsExpr id) -> SDoc
pprTransformStmt bndrs using by
  = sep [ text "then" <+> ifPprDebug (braces (ppr bndrs))
        , nest 2 (ppr using)
        , nest 2 (pprBy by)]
pprTransStmt :: Outputable body => Maybe body -> body -> TransForm -> SDoc
pprTransStmt by using ThenForm
  = sep [ text "then", nest 2 (ppr using), nest 2 (pprBy by)]
pprTransStmt by using GroupForm
  = sep [ text "then group", nest 2 (pprBy by), nest 2 (ptext (sLit "using") <+> ppr using)]
pprBy :: Outputable body => Maybe body -> SDoc
pprBy Nothing  = empty
pprBy (Just e) = text "by" <+> ppr e
pprDo :: (OutputableBndr id, Outputable body)
      => HsStmtContext any -> [LStmt id body] -> SDoc
pprDo DoExpr        stmts = text "do"  <+> ppr_do_stmts stmts
pprDo GhciStmtCtxt  stmts = text "do"  <+> ppr_do_stmts stmts
pprDo ArrowExpr     stmts = text "do"  <+> ppr_do_stmts stmts
pprDo MDoExpr       stmts = text "mdo" <+> ppr_do_stmts stmts
pprDo ListComp      stmts = brackets    $ pprComp stmts
pprDo PArrComp      stmts = paBrackets  $ pprComp stmts
pprDo MonadComp     stmts = brackets    $ pprComp stmts
pprDo _             _     = panic "pprDo"
ppr_do_stmts :: (OutputableBndr idL, OutputableBndr idR, Outputable body)
             => [LStmtLR idL idR body] -> SDoc

ppr_do_stmts stmts
  = lbrace <+> pprDeeperList vcat (punctuate semi (map ppr stmts))
           <+> rbrace
pprComp :: (OutputableBndr id, Outputable body)
        => [LStmt id body] -> SDoc
pprComp quals
  | not (null quals)
  , L _ (LastStmt body _ _) <- last quals
  = hang (ppr body <+> vbar) 2 (pprQuals (dropTail 1 quals))
  | otherwise
  = pprPanic "pprComp" (pprQuals quals)
pprQuals :: (OutputableBndr id, Outputable body)
        => [LStmt id body] -> SDoc
pprQuals quals = interpp'SP quals

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
  deriving (Typeable )
deriving instance (DataId id) => Data (HsSplice id)
isTypedSplice :: HsSplice id -> Bool
isTypedSplice (HsTypedSplice {}) = True
isTypedSplice _                  = False

type SplicePointName = Name
data PendingRnSplice
  = PendingRnSplice UntypedSpliceFlavour SplicePointName (LHsExpr Name)
  deriving (Data, Typeable)
data UntypedSpliceFlavour
  = UntypedExpSplice
  | UntypedPatSplice
  | UntypedTypeSplice
  | UntypedDeclSplice
  deriving( Data, Typeable )
data PendingTcSplice
  = PendingTcSplice SplicePointName (LHsExpr Id)
  deriving( Data, Typeable )

instance OutputableBndr id => Outputable (HsSplice id) where
  ppr s = pprSplice s
pprPendingSplice :: OutputableBndr id => SplicePointName -> LHsExpr id -> SDoc
pprPendingSplice n e = angleBrackets (ppr n <> comma <+> ppr e)
pprSplice :: OutputableBndr id => HsSplice id -> SDoc
pprSplice (HsTypedSplice   n e)  = ppr_splice (text "$$") n e
pprSplice (HsUntypedSplice n e)  = ppr_splice (text "$")  n e
pprSplice (HsQuasiQuote n q _ s) = ppr_quasi n q s
ppr_quasi :: OutputableBndr id => id -> id -> FastString -> SDoc
ppr_quasi n quoter quote = ifPprDebug (brackets (ppr n)) <>
                           char '[' <> ppr quoter <> vbar <>
                           ppr quote <> text "|]"
ppr_splice :: OutputableBndr id => SDoc -> id -> LHsExpr id -> SDoc
ppr_splice herald n e
    = herald <> ifPprDebug (brackets (ppr n)) <> eDoc
    where

          pp_as_was = pprLExpr e
          eDoc = case unLoc e of
                 HsPar _ -> pp_as_was
                 HsVar _ -> pp_as_was
                 _ -> parens pp_as_was
data HsBracket id = ExpBr (LHsExpr id)
                  | PatBr (LPat id)
                  | DecBrL [LHsDecl id]
                  | DecBrG (HsGroup id)
                  | TypBr (LHsType id)
                  | VarBr Bool id
                  | TExpBr (LHsExpr id)
  deriving (Typeable)
deriving instance (DataId id) => Data (HsBracket id)
isTypedBracket :: HsBracket id -> Bool
isTypedBracket (TExpBr {}) = True
isTypedBracket _           = False
instance OutputableBndr id => Outputable (HsBracket id) where
  ppr = pprHsBracket

pprHsBracket :: OutputableBndr id => HsBracket id -> SDoc
pprHsBracket (ExpBr e)   = thBrackets empty (ppr e)
pprHsBracket (PatBr p)   = thBrackets (char 'p') (ppr p)
pprHsBracket (DecBrG gp) = thBrackets (char 'd') (ppr gp)
pprHsBracket (DecBrL ds) = thBrackets (char 'd') (vcat (map ppr ds))
pprHsBracket (TypBr t)   = thBrackets (char 't') (ppr t)
pprHsBracket (VarBr True n)  = char '\''         <> ppr n
pprHsBracket (VarBr False n) = text "''" <> ppr n
pprHsBracket (TExpBr e)  = thTyBrackets (ppr e)
thBrackets :: SDoc -> SDoc -> SDoc
thBrackets pp_kind pp_body = char '[' <> pp_kind <> vbar <+>
                             pp_body <+> text "|]"
thTyBrackets :: SDoc -> SDoc
thTyBrackets pp_body = text "[||" <+> pp_body <+> ptext (sLit "||]")
instance Outputable PendingRnSplice where
  ppr (PendingRnSplice _ n e) = pprPendingSplice n e
instance Outputable PendingTcSplice where
  ppr (PendingTcSplice n e) = pprPendingSplice n e

data ArithSeqInfo id
  = From            (LHsExpr id)
  | FromThen        (LHsExpr id)
                    (LHsExpr id)
  | FromTo          (LHsExpr id)
                    (LHsExpr id)
  | FromThenTo      (LHsExpr id)
                    (LHsExpr id)
                    (LHsExpr id)
  deriving (Typeable)
deriving instance (DataId id) => Data (ArithSeqInfo id)
instance OutputableBndr id => Outputable (ArithSeqInfo id) where
    ppr (From e1)             = hcat [ppr e1, pp_dotdot]
    ppr (FromThen e1 e2)      = hcat [ppr e1, comma, space, ppr e2, pp_dotdot]
    ppr (FromTo e1 e3)        = hcat [ppr e1, pp_dotdot, ppr e3]
    ppr (FromThenTo e1 e2 e3)
      = hcat [ppr e1, comma, space, ppr e2, pp_dotdot, ppr e3]
pp_dotdot :: SDoc
pp_dotdot = text " .. "

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
  deriving (Data, Typeable)
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
  deriving (Data, Typeable)
isListCompExpr :: HsStmtContext id -> Bool
isListCompExpr ListComp          = True
isListCompExpr PArrComp          = True
isListCompExpr MonadComp         = True
isListCompExpr (ParStmtCtxt c)   = isListCompExpr c
isListCompExpr (TransStmtCtxt c) = isListCompExpr c
isListCompExpr _                 = False
isMonadCompExpr :: HsStmtContext id -> Bool
isMonadCompExpr MonadComp            = True
isMonadCompExpr (ParStmtCtxt ctxt)   = isMonadCompExpr ctxt
isMonadCompExpr (TransStmtCtxt ctxt) = isMonadCompExpr ctxt
isMonadCompExpr _                    = False
matchSeparator :: HsMatchContext id -> SDoc
matchSeparator (FunRhs {})  = text "="
matchSeparator CaseAlt      = text "->"
matchSeparator IfAlt        = text "->"
matchSeparator LambdaExpr   = text "->"
matchSeparator ProcExpr     = text "->"
matchSeparator PatBindRhs   = text "="
matchSeparator (StmtCtxt _) = text "<-"
matchSeparator RecUpd       = panic "unused"
matchSeparator ThPatSplice  = panic "unused"
matchSeparator ThPatQuote   = panic "unused"
matchSeparator PatSyn       = panic "unused"
pprMatchContext :: Outputable id => HsMatchContext id -> SDoc
pprMatchContext ctxt
  | want_an ctxt = text "an" <+> pprMatchContextNoun ctxt
  | otherwise    = text "a"  <+> pprMatchContextNoun ctxt
  where
    want_an (FunRhs {}) = True
    want_an ProcExpr    = True
    want_an _           = False
pprMatchContextNoun :: Outputable id => HsMatchContext id -> SDoc
pprMatchContextNoun (FunRhs fun)    = text "equation for"
                                      <+> quotes (ppr fun)
pprMatchContextNoun CaseAlt         = text "case alternative"
pprMatchContextNoun IfAlt           = text "multi-way if alternative"
pprMatchContextNoun RecUpd          = text "record-update construct"
pprMatchContextNoun ThPatSplice     = text "Template Haskell pattern splice"
pprMatchContextNoun ThPatQuote      = text "Template Haskell pattern quotation"
pprMatchContextNoun PatBindRhs      = text "pattern binding"
pprMatchContextNoun LambdaExpr      = text "lambda abstraction"
pprMatchContextNoun ProcExpr        = text "arrow abstraction"
pprMatchContextNoun (StmtCtxt ctxt) = text "pattern binding in"
                                      $$ pprStmtContext ctxt
pprMatchContextNoun PatSyn          = text "pattern synonym declaration"

pprAStmtContext, pprStmtContext :: Outputable id => HsStmtContext id -> SDoc
pprAStmtContext ctxt = article <+> pprStmtContext ctxt
  where
    pp_an = text "an"
    pp_a  = text "a"
    article = case ctxt of
                  MDoExpr       -> pp_an
                  PArrComp      -> pp_an
                  GhciStmtCtxt  -> pp_an
                  _             -> pp_a

pprStmtContext GhciStmtCtxt    = text "interactive GHCi command"
pprStmtContext DoExpr          = text "'do' block"
pprStmtContext MDoExpr         = text "'mdo' block"
pprStmtContext ArrowExpr       = text "'do' block in an arrow command"
pprStmtContext ListComp        = text "list comprehension"
pprStmtContext MonadComp       = text "monad comprehension"
pprStmtContext PArrComp        = text "array comprehension"
pprStmtContext (PatGuard ctxt) = text "pattern guard for" $$ pprMatchContext ctxt

pprStmtContext (ParStmtCtxt c)
 | opt_PprStyle_Debug = sep [text "parallel branch of", pprAStmtContext c]
 | otherwise          = pprStmtContext c
pprStmtContext (TransStmtCtxt c)
 | opt_PprStyle_Debug = sep [text "transformed branch of", pprAStmtContext c]
 | otherwise          = pprStmtContext c

matchContextErrString :: Outputable id => HsMatchContext id -> SDoc
matchContextErrString (FunRhs fun)         = text "function" <+> ppr fun
matchContextErrString CaseAlt              = text "case"
matchContextErrString IfAlt                = text "multi-way if"
matchContextErrString PatBindRhs           = text "pattern binding"
matchContextErrString RecUpd               = text "record update"
matchContextErrString LambdaExpr           = text "lambda"
matchContextErrString ProcExpr             = text "proc"
matchContextErrString ThPatSplice                = panic "matchContextErrString"
matchContextErrString ThPatQuote                 = panic "matchContextErrString"
matchContextErrString PatSyn                     = panic "matchContextErrString"
matchContextErrString (StmtCtxt (ParStmtCtxt c))   = matchContextErrString (StmtCtxt c)
matchContextErrString (StmtCtxt (TransStmtCtxt c)) = matchContextErrString (StmtCtxt c)
matchContextErrString (StmtCtxt (PatGuard _))      = text "pattern guard"
matchContextErrString (StmtCtxt GhciStmtCtxt)      = text "interactive GHCi command"
matchContextErrString (StmtCtxt DoExpr)            = text "'do' block"
matchContextErrString (StmtCtxt ArrowExpr)         = text "'do' block"
matchContextErrString (StmtCtxt MDoExpr)           = text "'mdo' block"
matchContextErrString (StmtCtxt ListComp)          = text "list comprehension"
matchContextErrString (StmtCtxt MonadComp)         = text "monad comprehension"
matchContextErrString (StmtCtxt PArrComp)          = text "array comprehension"
pprMatchInCtxt :: (OutputableBndr idL, OutputableBndr idR, Outputable body)
               => HsMatchContext idL -> Match idR body -> SDoc
pprMatchInCtxt ctxt match  = hang (text "In" <+> pprMatchContext ctxt <> colon)
                             4 (pprMatch ctxt match)
pprStmtInCtxt :: (OutputableBndr idL, OutputableBndr idR, Outputable body)
               => HsStmtContext idL -> StmtLR idL idR body -> SDoc
pprStmtInCtxt ctxt (LastStmt e _ _)
  | isListCompExpr ctxt
  = hang (text "In the expression:") 2 (ppr e)
pprStmtInCtxt ctxt stmt
  = hang (text "In a stmt of" <+> pprAStmtContext ctxt <> colon)
       2 (ppr_stmt stmt)
  where
    ppr_stmt (TransStmt { trS_by = by, trS_using = using
                        , trS_form = form }) = pprTransStmt by using form
    ppr_stmt stmt = pprStmt stmt
