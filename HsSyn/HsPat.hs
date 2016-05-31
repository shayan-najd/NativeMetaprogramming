{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module HsPat
       (Pat(..), InPat, OutPat, LPat, HsConPatDetails, hsConPatArgs,
        HsRecFields(..), HsRecField'(..), LHsRecField', HsRecField,
        LHsRecField, HsRecUpdField, LHsRecUpdField, hsRecFields,
        hsRecFieldSel, hsRecFieldId, hsRecFieldsArgs, hsRecUpdFieldId,
        hsRecUpdFieldOcc, hsRecUpdFieldRdr, mkPrefixConPat, mkCharLitPat,
        mkNilPat, isUnliftedHsBind, looksLazyPatBind, isUnliftedLPat,
        isBangedLPat, isBangedPatBind, hsPatNeedsParens,
        isIrrefutableHsPat, collectEvVarsPats, pprParendLPat, pprConArgs)
       where
import {-# SOURCE #-} HsExpr
       (SyntaxExpr, LHsExpr, HsSplice, pprLExpr, pprSplice)
import HsBinds
import HsLit
import PlaceHolder
import HsTypes
import TcEvidence
import BasicTypes
import PprCore ()
import TysWiredIn
import Var
import RdrName (RdrName)
import ConLike
import DataCon
import TyCon
import Outputable
import Type
import SrcLoc
import Bag
import DynFlags (gopt, GeneralFlag(..))
import Maybes
import Data.Data hiding (TyCon, Fixity)

type InPat id = LPat id

type OutPat id = LPat id

type LPat id = Located (Pat id)

data Pat id = WildPat (PostTc id Type)
            | VarPat (Located id)
            | LazyPat (LPat id)
            | AsPat (Located id) (LPat id)
            | ParPat (LPat id)
            | BangPat (LPat id)
            | ListPat [LPat id] (PostTc id Type)
                      (Maybe (PostTc id Type, SyntaxExpr id))
            | TuplePat [LPat id] Boxity [PostTc id Type]
            | PArrPat [LPat id] (PostTc id Type)
            | ConPatIn (Located id) (HsConPatDetails id)
            | ConPatOut{pat_con :: Located ConLike, pat_arg_tys :: [Type],
                        pat_tvs :: [TyVar], pat_dicts :: [EvVar], pat_binds :: TcEvBinds,
                        pat_args :: HsConPatDetails id, pat_wrap :: HsWrapper}
            | ViewPat (LHsExpr id) (LPat id) (PostTc id Type)
            | SplicePat (HsSplice id)
            | LitPat HsLit
            | NPat (Located (HsOverLit id)) (Maybe (SyntaxExpr id))
                   (SyntaxExpr id) (PostTc id Type)
            | NPlusKPat (Located id) (Located (HsOverLit id)) (HsOverLit id)
                        (SyntaxExpr id) (SyntaxExpr id) (PostTc id Type)
            | SigPatIn (LPat id) (LHsSigWcType id)
            | SigPatOut (LPat id) Type
            | CoPat HsWrapper (Pat id) Type
            deriving Typeable

deriving instance (DataId id) => Data (Pat id)

type HsConPatDetails id =
     HsConDetails (LPat id) (HsRecFields id (LPat id))

hsConPatArgs :: HsConPatDetails id -> [LPat id]
hsConPatArgs (PrefixCon ps) = ps
hsConPatArgs (RecCon fs)
  = map (hsRecFieldArg . unLoc) (rec_flds fs)
hsConPatArgs (InfixCon p1 p2) = [p1, p2]

data HsRecFields id arg = HsRecFields{rec_flds ::
                                      [LHsRecField id arg],
                                      rec_dotdot :: Maybe Int}
                        deriving (Typeable, Functor, Foldable, Traversable)

deriving instance (DataId id, Data arg) => Data
         (HsRecFields id arg)

type LHsRecField' id arg = Located (HsRecField' id arg)

type LHsRecField id arg = Located (HsRecField id arg)

type LHsRecUpdField id = Located (HsRecUpdField id)

type HsRecField id arg = HsRecField' (FieldOcc id) arg

type HsRecUpdField id =
     HsRecField' (AmbiguousFieldOcc id) (LHsExpr id)

data HsRecField' id arg = HsRecField{hsRecFieldLbl :: Located id,
                                     hsRecFieldArg :: arg, hsRecPun :: Bool}
                        deriving (Data, Typeable, Functor, Foldable, Traversable)

hsRecFields :: HsRecFields id arg -> [PostRn id id]
hsRecFields rbinds
  = map (unLoc . hsRecFieldSel . unLoc) (rec_flds rbinds)

hsRecFieldsArgs :: HsRecFields id arg -> [arg]
hsRecFieldsArgs rbinds
  = map (hsRecFieldArg . unLoc) (rec_flds rbinds)

hsRecFieldSel :: HsRecField name arg -> Located (PostRn name name)
hsRecFieldSel = fmap selectorFieldOcc . hsRecFieldLbl

hsRecFieldId :: HsRecField Id arg -> Located Id
hsRecFieldId = hsRecFieldSel

hsRecUpdFieldRdr :: HsRecUpdField id -> Located RdrName
hsRecUpdFieldRdr = fmap rdrNameAmbiguousFieldOcc . hsRecFieldLbl

hsRecUpdFieldId ::
                HsRecField' (AmbiguousFieldOcc Id) arg -> Located Id
hsRecUpdFieldId = fmap selectorFieldOcc . hsRecUpdFieldOcc

hsRecUpdFieldOcc ::
                 HsRecField' (AmbiguousFieldOcc Id) arg -> LFieldOcc Id
hsRecUpdFieldOcc = fmap unambiguousFieldOcc . hsRecFieldLbl

instance (OutputableBndr name) => Outputable (Pat name) where
        ppr = pprPat

pprPatBndr :: OutputableBndr name => name -> SDoc
pprPatBndr var
  = getPprStyle $
      \ sty ->
        if debugStyle sty then parens (pprBndr LambdaBind var) else
          pprPrefixOcc var

pprParendLPat :: (OutputableBndr name) => LPat name -> SDoc
pprParendLPat (L _ p) = pprParendPat p

pprParendPat :: (OutputableBndr name) => Pat name -> SDoc
pprParendPat p
  = sdocWithDynFlags $
      \ dflags ->
        if need_parens dflags p then parens (pprPat p) else pprPat p
  where need_parens dflags p
          | CoPat{} <- p = gopt Opt_PrintTypecheckerElaboration dflags
          | otherwise = hsPatNeedsParens p

pprPat :: (OutputableBndr name) => Pat name -> SDoc
pprPat (VarPat (L _ var)) = pprPatBndr var
pprPat (WildPat _) = char '_'
pprPat (LazyPat pat) = char '~' <> pprParendLPat pat
pprPat (BangPat pat) = char '!' <> pprParendLPat pat
pprPat (AsPat name pat)
  = hcat [pprPrefixOcc (unLoc name), char '@', pprParendLPat pat]
pprPat (ViewPat expr pat _)
  = hcat [pprLExpr expr, text " -> ", ppr pat]
pprPat (ParPat pat) = parens (ppr pat)
pprPat (LitPat s) = ppr s
pprPat (NPat l Nothing _ _) = ppr l
pprPat (NPat l (Just _) _ _) = char '-' <> ppr l
pprPat (NPlusKPat n k _ _ _ _) = hcat [ppr n, char '+', ppr k]
pprPat (SplicePat splice) = pprSplice splice
pprPat (CoPat co pat _)
  = pprHsWrapper co
      (\ parens -> if parens then pprParendPat pat else pprPat pat)
pprPat (SigPatIn pat ty) = ppr pat <+> dcolon <+> ppr ty
pprPat (SigPatOut pat ty) = ppr pat <+> dcolon <+> ppr ty
pprPat (ListPat pats _ _) = brackets (interpp'SP pats)
pprPat (PArrPat pats _) = paBrackets (interpp'SP pats)
pprPat (TuplePat pats bx _)
  = tupleParens (boxityTupleSort bx) (pprWithCommas ppr pats)
pprPat (ConPatIn con details) = pprUserCon (unLoc con) details
pprPat
  (ConPatOut{pat_con = con, pat_tvs = tvs, pat_dicts = dicts,
             pat_binds = binds, pat_args = details})
  = sdocWithDynFlags $
      \ dflags ->
        if gopt Opt_PrintTypecheckerElaboration dflags then
          ppr con <>
            braces (sep [hsep (map pprPatBndr (tvs ++ dicts)), ppr binds])
            <+> pprConArgs details
          else pprUserCon (unLoc con) details

pprUserCon ::
             (OutputableBndr con, OutputableBndr id) =>
             con -> HsConPatDetails id -> SDoc
pprUserCon c (InfixCon p1 p2) = ppr p1 <+> pprInfixOcc c <+> ppr p2
pprUserCon c details = pprPrefixOcc c <+> pprConArgs details

pprConArgs :: OutputableBndr id => HsConPatDetails id -> SDoc
pprConArgs (PrefixCon pats) = sep (map pprParendLPat pats)
pprConArgs (InfixCon p1 p2)
  = sep [pprParendLPat p1, pprParendLPat p2]
pprConArgs (RecCon rpats) = ppr rpats

instance (Outputable arg) => Outputable (HsRecFields id arg) where
        ppr (HsRecFields{rec_flds = flds, rec_dotdot = Nothing})
          = braces (fsep (punctuate comma (map ppr flds)))
        ppr (HsRecFields{rec_flds = flds, rec_dotdot = Just n})
          = braces
              (fsep (punctuate comma (map ppr (take n flds) ++ [dotdot])))
          where dotdot = text ".." <+> ifPprDebug (ppr (drop n flds))

instance (Outputable id, Outputable arg) => Outputable
         (HsRecField' id arg) where
        ppr
          (HsRecField{hsRecFieldLbl = f, hsRecFieldArg = arg,
                      hsRecPun = pun})
          = ppr f <+> (ppUnless pun $ equals <+> ppr arg)

mkPrefixConPat :: DataCon -> [OutPat id] -> [Type] -> OutPat id
mkPrefixConPat dc pats tys
  = noLoc $
      ConPatOut{pat_con = noLoc (RealDataCon dc), pat_tvs = [],
                pat_dicts = [], pat_binds = emptyTcEvBinds,
                pat_args = PrefixCon pats, pat_arg_tys = tys,
                pat_wrap = idHsWrapper}

mkNilPat :: Type -> OutPat id
mkNilPat ty = mkPrefixConPat nilDataCon [] [ty]

mkCharLitPat :: String -> Char -> OutPat id
mkCharLitPat src c
  = mkPrefixConPat charDataCon [noLoc $ LitPat (HsCharPrim src c)] []

isUnliftedLPat :: LPat id -> Bool
isUnliftedLPat (L _ (ParPat p)) = isUnliftedLPat p
isUnliftedLPat (L _ (TuplePat _ Unboxed _)) = True
isUnliftedLPat _ = False

isUnliftedHsBind :: HsBind id -> Bool
isUnliftedHsBind (PatBind{pat_lhs = p}) = isUnliftedLPat p
isUnliftedHsBind _ = False

isBangedPatBind :: HsBind id -> Bool
isBangedPatBind (PatBind{pat_lhs = pat}) = isBangedLPat pat
isBangedPatBind _ = False

isBangedLPat :: LPat id -> Bool
isBangedLPat (L _ (ParPat p)) = isBangedLPat p
isBangedLPat (L _ (BangPat{})) = True
isBangedLPat _ = False

looksLazyPatBind :: HsBind id -> Bool
looksLazyPatBind (PatBind{pat_lhs = p}) = looksLazyLPat p
looksLazyPatBind _ = False

looksLazyLPat :: LPat id -> Bool
looksLazyLPat (L _ (ParPat p)) = looksLazyLPat p
looksLazyLPat (L _ (AsPat _ p)) = looksLazyLPat p
looksLazyLPat (L _ (BangPat{})) = False
looksLazyLPat (L _ (TuplePat _ Unboxed _)) = False
looksLazyLPat (L _ (VarPat{})) = False
looksLazyLPat (L _ (WildPat{})) = False
looksLazyLPat _ = True

isIrrefutableHsPat :: OutputableBndr id => LPat id -> Bool
isIrrefutableHsPat pat = go pat
  where go (L _ pat) = go1 pat
        go1 (WildPat{}) = True
        go1 (VarPat{}) = True
        go1 (LazyPat{}) = True
        go1 (BangPat pat) = go pat
        go1 (CoPat _ pat _) = go1 pat
        go1 (ParPat pat) = go pat
        go1 (AsPat _ pat) = go pat
        go1 (ViewPat _ pat _) = go pat
        go1 (SigPatIn pat _) = go pat
        go1 (SigPatOut pat _) = go pat
        go1 (TuplePat pats _ _) = all go pats
        go1 (ListPat{}) = False
        go1 (PArrPat{}) = False
        go1 (ConPatIn{}) = False
        go1
          (ConPatOut{pat_con = L _ (RealDataCon con), pat_args = details})
          = isJust (tyConSingleDataCon_maybe (dataConTyCon con)) &&
              all go (hsConPatArgs details)
        go1 (ConPatOut{pat_con = L _ (PatSynCon _pat)}) = False
        go1 (LitPat{}) = False
        go1 (NPat{}) = False
        go1 (NPlusKPat{}) = False
        go1 (SplicePat{}) = urk pat
        urk pat = pprPanic "isIrrefutableHsPat:" (ppr pat)

hsPatNeedsParens :: Pat a -> Bool
hsPatNeedsParens (NPlusKPat{}) = True
hsPatNeedsParens (SplicePat{}) = False
hsPatNeedsParens (ConPatIn _ ds) = conPatNeedsParens ds
hsPatNeedsParens p@(ConPatOut{}) = conPatNeedsParens (pat_args p)
hsPatNeedsParens (SigPatIn{}) = True
hsPatNeedsParens (SigPatOut{}) = True
hsPatNeedsParens (ViewPat{}) = True
hsPatNeedsParens (CoPat _ p _) = hsPatNeedsParens p
hsPatNeedsParens (WildPat{}) = False
hsPatNeedsParens (VarPat{}) = False
hsPatNeedsParens (LazyPat{}) = False
hsPatNeedsParens (BangPat{}) = False
hsPatNeedsParens (ParPat{}) = False
hsPatNeedsParens (AsPat{}) = False
hsPatNeedsParens (TuplePat{}) = False
hsPatNeedsParens (ListPat{}) = False
hsPatNeedsParens (PArrPat{}) = False
hsPatNeedsParens (LitPat{}) = False
hsPatNeedsParens (NPat{}) = False

conPatNeedsParens :: HsConDetails a b -> Bool
conPatNeedsParens (PrefixCon args) = not (null args)
conPatNeedsParens (InfixCon{}) = True
conPatNeedsParens (RecCon{}) = True

collectEvVarsPats :: [Pat id] -> Bag EvVar
collectEvVarsPats = unionManyBags . map collectEvVarsPat

collectEvVarsLPat :: LPat id -> Bag EvVar
collectEvVarsLPat (L _ pat) = collectEvVarsPat pat

collectEvVarsPat :: Pat id -> Bag EvVar
collectEvVarsPat pat
  = case pat of
        LazyPat p -> collectEvVarsLPat p
        AsPat _ p -> collectEvVarsLPat p
        ParPat p -> collectEvVarsLPat p
        BangPat p -> collectEvVarsLPat p
        ListPat ps _ _ -> unionManyBags $ map collectEvVarsLPat ps
        TuplePat ps _ _ -> unionManyBags $ map collectEvVarsLPat ps
        PArrPat ps _ -> unionManyBags $ map collectEvVarsLPat ps
        ConPatOut{pat_dicts = dicts, pat_args = args} -> unionBags
                                                           (listToBag dicts)
                                                           $
                                                           unionManyBags $
                                                             map collectEvVarsLPat $
                                                               hsConPatArgs args
        SigPatOut p _ -> collectEvVarsLPat p
        CoPat _ p _ -> collectEvVarsPat p
        ConPatIn _ _ -> panic "foldMapPatBag: ConPatIn"
        SigPatIn _ _ -> panic "foldMapPatBag: SigPatIn"
        _other_pat -> emptyBag