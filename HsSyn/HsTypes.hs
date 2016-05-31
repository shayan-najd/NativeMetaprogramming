{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
module HsTypes
       (HsType(..), LHsType, HsKind, LHsKind, HsTyVarBndr(..),
        LHsTyVarBndr, LHsQTyVars(..), HsImplicitBndrs(..),
        HsWildCardBndrs(..), LHsSigType, LHsSigWcType, LHsWcType,
        HsTupleSort(..), HsContext, LHsContext, HsTyLit(..), HsIPName(..),
        hsIPNameFS, HsAppType(..), LHsAppType, LBangType, BangType,
        HsSrcBang(..), HsImplBang(..), SrcStrictness(..),
        SrcUnpackedness(..), getBangType, getBangStrictness,
        ConDeclField(..), LConDeclField, pprConDeclFields,
        updateGadtResult, HsConDetails(..), FieldOcc(..), LFieldOcc,
        mkFieldOcc, AmbiguousFieldOcc(..), mkAmbiguousFieldOcc,
        rdrNameAmbiguousFieldOcc, selectorAmbiguousFieldOcc,
        unambiguousFieldOcc, ambiguousFieldOcc, HsWildCardInfo(..),
        mkAnonWildCardTy, wildCardName, sameWildCard, mkHsImplicitBndrs,
        mkHsWildCardBndrs, hsImplicitBody, mkEmptyImplicitBndrs,
        mkEmptyWildCardBndrs, mkHsQTvs, hsQTvExplicit, emptyLHsQTvs,
        isEmptyLHsQTvs, isHsKindedTyVar, hsTvbAllKinded, hsScopedTvs,
        hsWcScopedTvs, dropWildCards, hsTyVarName, hsAllLTyVarNames,
        hsLTyVarLocNames, hsLTyVarName, hsLTyVarLocName,
        hsExplicitLTyVarNames, splitLHsInstDeclTy, getLHsInstDeclHead,
        getLHsInstDeclClass_maybe, splitLHsPatSynTy, splitLHsForAllTy,
        splitLHsQualTy, splitLHsSigmaTy, splitHsFunType, splitHsAppsTy,
        splitHsAppTys, getAppsTyHead_maybe, hsTyGetAppHead_maybe, mkHsOpTy,
        mkHsAppTy, mkHsAppTys, ignoreParens, hsSigType, hsSigWcType,
        hsLTyVarBndrToType, hsLTyVarBndrsToTypes, pprParendHsType,
        pprHsForAll, pprHsForAllTvs, pprHsForAllExtra, pprHsContext,
        pprHsContextNoArrow, pprHsContextMaybe)
       where
import {-# SOURCE #-} HsExpr (HsSplice, pprSplice)
import PlaceHolder (PostTc, PostRn, DataId, PlaceHolder(..))
import Id (Id)
import Name (Name)
import RdrName (RdrName)
import NameSet (NameSet, emptyNameSet)
import DataCon
       (HsSrcBang(..), HsImplBang(..), SrcStrictness(..),
        SrcUnpackedness(..))
import TysPrim (funTyConName)
import Type
import HsDoc
import BasicTypes
import SrcLoc
import StaticFlags
import Outputable
import FastString
import Maybes (isJust)
import Data.Data hiding (Fixity)
import Data.Maybe (fromMaybe)
import Control.Monad (unless)
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup as Semigroup

type LBangType name = Located (BangType name)

type BangType name = HsType name

getBangType :: LHsType a -> LHsType a
getBangType (L _ (HsBangTy _ ty)) = ty
getBangType ty = ty

getBangStrictness :: LHsType a -> HsSrcBang
getBangStrictness (L _ (HsBangTy s _)) = s
getBangStrictness _ = (HsSrcBang Nothing NoSrcUnpack NoSrcStrict)

type LHsContext name = Located (HsContext name)

type HsContext name = [LHsType name]

type LHsType name = Located (HsType name)

type HsKind name = HsType name

type LHsKind name = Located (HsKind name)

type LHsTyVarBndr name = Located (HsTyVarBndr name)

data LHsQTyVars name = HsQTvs{hsq_implicit :: PostRn name [Name],
                              hsq_explicit :: [LHsTyVarBndr name],
                              hsq_dependent :: PostRn name NameSet}
                     deriving Typeable

deriving instance (DataId name) => Data (LHsQTyVars name)

mkHsQTvs :: [LHsTyVarBndr RdrName] -> LHsQTyVars RdrName
mkHsQTvs tvs
  = HsQTvs{hsq_implicit = PlaceHolder, hsq_explicit = tvs,
           hsq_dependent = PlaceHolder}

hsQTvExplicit :: LHsQTyVars name -> [LHsTyVarBndr name]
hsQTvExplicit = hsq_explicit

emptyLHsQTvs :: LHsQTyVars Name
emptyLHsQTvs = HsQTvs [] [] emptyNameSet

isEmptyLHsQTvs :: LHsQTyVars Name -> Bool
isEmptyLHsQTvs (HsQTvs [] [] _) = True
isEmptyLHsQTvs _ = False

data HsImplicitBndrs name thing = HsIB{hsib_vars ::
                                       PostRn name [Name],
                                       hsib_body :: thing}
                                deriving Typeable

data HsWildCardBndrs name thing = HsWC{hswc_wcs ::
                                       PostRn name [Name],
                                       hswc_ctx :: Maybe SrcSpan, hswc_body :: thing}
                                deriving Typeable

deriving instance
         (Data name, Data thing, Data (PostRn name [Name])) => Data
         (HsImplicitBndrs name thing)

deriving instance
         (Data name, Data thing, Data (PostRn name [Name])) => Data
         (HsWildCardBndrs name thing)

type LHsSigType name = HsImplicitBndrs name (LHsType name)

type LHsWcType name = HsWildCardBndrs name (LHsType name)

type LHsSigWcType name = HsImplicitBndrs name (LHsWcType name)

hsImplicitBody :: HsImplicitBndrs name thing -> thing
hsImplicitBody (HsIB{hsib_body = body}) = body

hsSigType :: LHsSigType name -> LHsType name
hsSigType = hsImplicitBody

hsSigWcType :: LHsSigWcType name -> LHsType name
hsSigWcType sig_ty = hswc_body (hsib_body sig_ty)

dropWildCards :: LHsSigWcType name -> LHsSigType name
dropWildCards sig_ty = sig_ty{hsib_body = hsSigWcType sig_ty}

mkHsImplicitBndrs :: thing -> HsImplicitBndrs RdrName thing
mkHsImplicitBndrs x = HsIB{hsib_body = x, hsib_vars = PlaceHolder}

mkHsWildCardBndrs :: thing -> HsWildCardBndrs RdrName thing
mkHsWildCardBndrs x
  = HsWC{hswc_body = x, hswc_wcs = PlaceHolder, hswc_ctx = Nothing}

mkEmptyImplicitBndrs :: thing -> HsImplicitBndrs Name thing
mkEmptyImplicitBndrs x = HsIB{hsib_body = x, hsib_vars = []}

mkEmptyWildCardBndrs :: thing -> HsWildCardBndrs Name thing
mkEmptyWildCardBndrs x
  = HsWC{hswc_body = x, hswc_wcs = [], hswc_ctx = Nothing}

newtype HsIPName = HsIPName FastString
                 deriving (Eq, Data, Typeable)

hsIPNameFS :: HsIPName -> FastString
hsIPNameFS (HsIPName n) = n

instance Outputable HsIPName where
        ppr (HsIPName n) = char '?' <> ftext n

instance OutputableBndr HsIPName where
        pprBndr _ n = ppr n
        pprInfixOcc n = ppr n
        pprPrefixOcc n = ppr n

data HsTyVarBndr name = UserTyVar (Located name)
                      | KindedTyVar (Located name) (LHsKind name)
                      deriving Typeable

deriving instance (DataId name) => Data (HsTyVarBndr name)

isHsKindedTyVar :: HsTyVarBndr name -> Bool
isHsKindedTyVar (UserTyVar{}) = False
isHsKindedTyVar (KindedTyVar{}) = True

hsTvbAllKinded :: LHsQTyVars name -> Bool
hsTvbAllKinded = all (isHsKindedTyVar . unLoc) . hsQTvExplicit

data HsType name = HsForAllTy{hst_bndrs :: [LHsTyVarBndr name],
                              hst_body :: LHsType name}
                 | HsQualTy{hst_ctxt :: LHsContext name, hst_body :: LHsType name}
                 | HsTyVar (Located name)
                 | HsAppsTy [LHsAppType name]
                 | HsAppTy (LHsType name) (LHsType name)
                 | HsFunTy (LHsType name) (LHsType name)
                 | HsListTy (LHsType name)
                 | HsPArrTy (LHsType name)
                 | HsTupleTy HsTupleSort [LHsType name]
                 | HsOpTy (LHsType name) (Located name) (LHsType name)
                 | HsParTy (LHsType name)
                 | HsIParamTy HsIPName (LHsType name)
                 | HsEqTy (LHsType name) (LHsType name)
                 | HsKindSig (LHsType name) (LHsKind name)
                 | HsSpliceTy (HsSplice name) (PostTc name Kind)
                 | HsDocTy (LHsType name) LHsDocString
                 | HsBangTy HsSrcBang (LHsType name)
                 | HsRecTy [LConDeclField name]
                 | HsCoreTy Type
                 | HsExplicitListTy (PostTc name Kind) [LHsType name]
                 | HsExplicitTupleTy [PostTc name Kind] [LHsType name]
                 | HsTyLit HsTyLit
                 | HsWildCardTy (HsWildCardInfo name)
                 deriving Typeable

deriving instance (DataId name) => Data (HsType name)

data HsTyLit = HsNumTy SourceText Integer
             | HsStrTy SourceText FastString
             deriving (Data, Typeable)

newtype HsWildCardInfo name = AnonWildCard (PostRn name
                                              (Located Name))
                            deriving Typeable

deriving instance (DataId name) => Data (HsWildCardInfo name)

type LHsAppType name = Located (HsAppType name)

data HsAppType name = HsAppInfix (Located name)
                    | HsAppPrefix (LHsType name)
                    deriving Typeable

deriving instance (DataId name) => Data (HsAppType name)

instance OutputableBndr name => Outputable (HsAppType name) where
        ppr = ppr_app_ty TopPrec

data HsTupleSort = HsUnboxedTuple
                 | HsBoxedTuple
                 | HsConstraintTuple
                 | HsBoxedOrConstraintTuple
                 deriving (Data, Typeable)

type LConDeclField name = Located (ConDeclField name)

data ConDeclField name = ConDeclField{cd_fld_names ::
                                      [LFieldOcc name],
                                      cd_fld_type :: LBangType name,
                                      cd_fld_doc :: Maybe LHsDocString}
                       deriving Typeable

deriving instance (DataId name) => Data (ConDeclField name)

instance (OutputableBndr name) => Outputable (ConDeclField name)
         where
        ppr (ConDeclField fld_n fld_ty _)
          = ppr fld_n <+> dcolon <+> ppr fld_ty

data HsConDetails arg rec = PrefixCon [arg]
                          | RecCon rec
                          | InfixCon arg arg
                          deriving (Data, Typeable)

instance (Outputable arg, Outputable rec) => Outputable
         (HsConDetails arg rec) where
        ppr (PrefixCon args) = text "PrefixCon" <+> ppr args
        ppr (RecCon rec) = text "RecCon:" <+> ppr rec
        ppr (InfixCon l r) = text "InfixCon:" <+> ppr [l, r]

updateGadtResult ::
                   (Monad m) =>
                   (SDoc -> m ()) ->
                     SDoc ->
                       HsConDetails (LHsType Name) (Located [LConDeclField Name]) ->
                         LHsType Name ->
                           m (HsConDetails (LHsType Name) (Located [LConDeclField Name]),
                              LHsType Name)
updateGadtResult failWith doc details ty
  = do let (arg_tys, res_ty) = splitHsFunType ty
           badConSig = text "Malformed constructor signature"
       case details of
           InfixCon{} -> pprPanic "updateGadtResult" (ppr ty)
           RecCon{} -> do unless (null arg_tys) (failWith (doc <+> badConSig))
                          return (details, res_ty)
           PrefixCon{} -> return (PrefixCon arg_tys, res_ty)

hsWcScopedTvs :: LHsSigWcType Name -> [Name]
hsWcScopedTvs sig_ty
  | HsIB{hsib_vars = vars, hsib_body = sig_ty1} <- sig_ty,
    HsWC{hswc_wcs = nwcs, hswc_body = sig_ty2} <- sig_ty1 =
    case sig_ty2 of
        L _ (HsForAllTy{hst_bndrs = tvs}) -> vars ++
                                               nwcs ++ map hsLTyVarName tvs
        _ -> nwcs

hsScopedTvs :: LHsSigType Name -> [Name]
hsScopedTvs sig_ty
  | HsIB{hsib_vars = vars, hsib_body = sig_ty2} <- sig_ty,
    L _ (HsForAllTy{hst_bndrs = tvs}) <- sig_ty2 =
    vars ++ map hsLTyVarName tvs
  | otherwise = []

hsTyVarName :: HsTyVarBndr name -> name
hsTyVarName (UserTyVar (L _ n)) = n
hsTyVarName (KindedTyVar (L _ n) _) = n

hsLTyVarName :: LHsTyVarBndr name -> name
hsLTyVarName = hsTyVarName . unLoc

hsExplicitLTyVarNames :: LHsQTyVars name -> [name]
hsExplicitLTyVarNames qtvs = map hsLTyVarName (hsQTvExplicit qtvs)

hsAllLTyVarNames :: LHsQTyVars Name -> [Name]
hsAllLTyVarNames (HsQTvs{hsq_implicit = kvs, hsq_explicit = tvs})
  = kvs ++ map hsLTyVarName tvs

hsLTyVarLocName :: LHsTyVarBndr name -> Located name
hsLTyVarLocName = fmap hsTyVarName

hsLTyVarLocNames :: LHsQTyVars name -> [Located name]
hsLTyVarLocNames qtvs = map hsLTyVarLocName (hsQTvExplicit qtvs)

hsLTyVarBndrToType :: LHsTyVarBndr name -> LHsType name
hsLTyVarBndrToType = fmap cvt
  where cvt (UserTyVar n) = HsTyVar n
        cvt (KindedTyVar (L name_loc n) kind)
          = HsKindSig (L name_loc (HsTyVar (L name_loc n))) kind

hsLTyVarBndrsToTypes :: LHsQTyVars name -> [LHsType name]
hsLTyVarBndrsToTypes (HsQTvs{hsq_explicit = tvbs})
  = map hsLTyVarBndrToType tvbs

wildCardName :: HsWildCardInfo Name -> Name
wildCardName (AnonWildCard (L _ n)) = n

sameWildCard ::
             Located (HsWildCardInfo name) ->
               Located (HsWildCardInfo name) -> Bool
sameWildCard (L l1 (AnonWildCard _)) (L l2 (AnonWildCard _))
  = l1 == l2

ignoreParens :: LHsType name -> LHsType name
ignoreParens (L _ (HsParTy ty)) = ignoreParens ty
ignoreParens (L _ (HsAppsTy [L _ (HsAppPrefix ty)]))
  = ignoreParens ty
ignoreParens ty = ty

mkAnonWildCardTy :: HsType RdrName
mkAnonWildCardTy = HsWildCardTy (AnonWildCard PlaceHolder)

mkHsOpTy ::
         LHsType name -> Located name -> LHsType name -> HsType name
mkHsOpTy ty1 op ty2 = HsOpTy ty1 op ty2

mkHsAppTy :: LHsType name -> LHsType name -> LHsType name
mkHsAppTy t1 t2 = addCLoc t1 t2 (HsAppTy t1 t2)

mkHsAppTys :: LHsType name -> [LHsType name] -> LHsType name
mkHsAppTys = foldl mkHsAppTy

splitHsFunType :: LHsType Name -> ([LHsType Name], LHsType Name)
splitHsFunType (L _ (HsParTy ty)) = splitHsFunType ty
splitHsFunType (L _ (HsFunTy x y))
  | (args, res) <- splitHsFunType y = (x : args, res)
splitHsFunType orig_ty@(L _ (HsAppTy t1 t2)) = go t1 [t2]
  where go (L _ (HsTyVar (L _ fn))) tys
          | fn == funTyConName, [t1, t2] <- tys,
            (args, res) <- splitHsFunType t2 = (t1 : args, res)
        go (L _ (HsAppTy t1 t2)) tys = go t1 (t2 : tys)
        go (L _ (HsParTy ty)) tys = go ty tys
        go _ _ = ([], orig_ty)
splitHsFunType other = ([], other)

getAppsTyHead_maybe ::
                    [LHsAppType name] -> Maybe (LHsType name, [LHsType name])
getAppsTyHead_maybe tys
  = case splitHsAppsTy tys of
        ([app1 : apps], []) -> Just (mkHsAppTys app1 apps, [])
        ([app1l : appsl, app1r : appsr], [L loc op]) -> Just
                                                          (L loc (HsTyVar (L loc op)),
                                                           [mkHsAppTys app1l appsl,
                                                            mkHsAppTys app1r appsr])
        _ -> Nothing

splitHsAppsTy ::
              [LHsAppType name] -> ([[LHsType name]], [Located name])
splitHsAppsTy = go [] [] []
  where go acc acc_non acc_sym []
          = (reverse (reverse acc : acc_non), reverse acc_sym)
        go acc acc_non acc_sym (L _ (HsAppPrefix ty) : rest)
          = go (ty : acc) acc_non acc_sym rest
        go acc acc_non acc_sym (L _ (HsAppInfix op) : rest)
          = go [] (reverse acc : acc_non) (op : acc_sym) rest

hsTyGetAppHead_maybe ::
                     LHsType name -> Maybe (Located name, [LHsType name])
hsTyGetAppHead_maybe = go []
  where go tys (L _ (HsTyVar ln)) = Just (ln, tys)
        go tys (L _ (HsAppsTy apps))
          | Just (head, args) <- getAppsTyHead_maybe apps =
            go (args ++ tys) head
        go tys (L _ (HsAppTy l r)) = go (r : tys) l
        go tys (L _ (HsOpTy l (L loc n) r)) = Just (L loc n, l : r : tys)
        go tys (L _ (HsParTy t)) = go tys t
        go tys (L _ (HsKindSig t _)) = go tys t
        go _ _ = Nothing

splitHsAppTys ::
              LHsType Name -> [LHsType Name] -> (LHsType Name, [LHsType Name])
splitHsAppTys (L _ (HsAppTy f a)) as = splitHsAppTys f (a : as)
splitHsAppTys (L _ (HsParTy f)) as = splitHsAppTys f as
splitHsAppTys f as = (f, as)

splitLHsPatSynTy ::
                 LHsType name ->
                   ([LHsTyVarBndr name], LHsContext name, LHsContext name,
                    LHsType name)
splitLHsPatSynTy ty
  | L _ (HsQualTy{hst_ctxt = req, hst_body = ty2}) <- ty1,
    L _ (HsQualTy{hst_ctxt = prov, hst_body = ty3}) <- ty2 =
    (tvs, req, prov, ty3)
  | L _ (HsQualTy{hst_ctxt = req, hst_body = ty2}) <- ty1 =
    (tvs, req, noLoc [], ty2)
  | otherwise = (tvs, noLoc [], noLoc [], ty1)
  where (tvs, ty1) = splitLHsForAllTy ty

splitLHsSigmaTy ::
                LHsType name ->
                  ([LHsTyVarBndr name], LHsContext name, LHsType name)
splitLHsSigmaTy ty
  | (tvs, ty1) <- splitLHsForAllTy ty,
    (ctxt, ty2) <- splitLHsQualTy ty1 = (tvs, ctxt, ty2)

splitLHsForAllTy ::
                 LHsType name -> ([LHsTyVarBndr name], LHsType name)
splitLHsForAllTy
  (L _ (HsForAllTy{hst_bndrs = tvs, hst_body = body})) = (tvs, body)
splitLHsForAllTy body = ([], body)

splitLHsQualTy :: LHsType name -> (LHsContext name, LHsType name)
splitLHsQualTy (L _ (HsQualTy{hst_ctxt = ctxt, hst_body = body}))
  = (ctxt, body)
splitLHsQualTy body = (noLoc [], body)

splitLHsInstDeclTy ::
                   LHsSigType Name -> ([Name], LHsContext Name, LHsType Name)
splitLHsInstDeclTy (HsIB{hsib_vars = itkvs, hsib_body = inst_ty})
  | (tvs, cxt, body_ty) <- splitLHsSigmaTy inst_ty =
    (itkvs ++ map hsLTyVarName tvs, cxt, body_ty)
  where

getLHsInstDeclHead :: LHsSigType name -> LHsType name
getLHsInstDeclHead inst_ty
  | (_tvs, _cxt, body_ty) <- splitLHsSigmaTy (hsSigType inst_ty) =
    body_ty

getLHsInstDeclClass_maybe ::
                          LHsSigType name -> Maybe (Located name)
getLHsInstDeclClass_maybe inst_ty
  = do let head_ty = getLHsInstDeclHead inst_ty
       (cls, _) <- hsTyGetAppHead_maybe head_ty
       return cls

type LFieldOcc name = Located (FieldOcc name)

data FieldOcc name = FieldOcc{rdrNameFieldOcc :: Located RdrName,
                              selectorFieldOcc :: PostRn name name}
                   deriving Typeable

deriving instance Eq (PostRn name name) => Eq (FieldOcc name)

deriving instance Ord (PostRn name name) => Ord (FieldOcc name)

deriving instance (Data name, Data (PostRn name name)) => Data
         (FieldOcc name)

instance Outputable (FieldOcc name) where
        ppr = ppr . rdrNameFieldOcc

mkFieldOcc :: Located RdrName -> FieldOcc RdrName
mkFieldOcc rdr = FieldOcc rdr PlaceHolder

data AmbiguousFieldOcc name = Unambiguous (Located RdrName)
                                          (PostRn name name)
                            | Ambiguous (Located RdrName) (PostTc name name)
                            deriving Typeable

deriving instance
         (Data name, Data (PostRn name name), Data (PostTc name name)) =>
         Data (AmbiguousFieldOcc name)

instance Outputable (AmbiguousFieldOcc name) where
        ppr = ppr . rdrNameAmbiguousFieldOcc

instance OutputableBndr (AmbiguousFieldOcc name) where
        pprInfixOcc = pprInfixOcc . rdrNameAmbiguousFieldOcc
        pprPrefixOcc = pprPrefixOcc . rdrNameAmbiguousFieldOcc

mkAmbiguousFieldOcc :: Located RdrName -> AmbiguousFieldOcc RdrName
mkAmbiguousFieldOcc rdr = Unambiguous rdr PlaceHolder

rdrNameAmbiguousFieldOcc :: AmbiguousFieldOcc name -> RdrName
rdrNameAmbiguousFieldOcc (Unambiguous (L _ rdr) _) = rdr
rdrNameAmbiguousFieldOcc (Ambiguous (L _ rdr) _) = rdr

selectorAmbiguousFieldOcc :: AmbiguousFieldOcc Id -> Id
selectorAmbiguousFieldOcc (Unambiguous _ sel) = sel
selectorAmbiguousFieldOcc (Ambiguous _ sel) = sel

unambiguousFieldOcc :: AmbiguousFieldOcc Id -> FieldOcc Id
unambiguousFieldOcc (Unambiguous rdr sel) = FieldOcc rdr sel
unambiguousFieldOcc (Ambiguous rdr sel) = FieldOcc rdr sel

ambiguousFieldOcc :: FieldOcc name -> AmbiguousFieldOcc name
ambiguousFieldOcc (FieldOcc rdr sel) = Unambiguous rdr sel

instance (OutputableBndr name) => Outputable (HsType name) where
        ppr ty = pprHsType ty

instance Outputable HsTyLit where
        ppr = ppr_tylit

instance (OutputableBndr name) => Outputable (LHsQTyVars name)
         where
        ppr (HsQTvs{hsq_explicit = tvs}) = interppSP tvs

instance (OutputableBndr name) => Outputable (HsTyVarBndr name)
         where
        ppr (UserTyVar n) = ppr n
        ppr (KindedTyVar n k) = parens $ hsep [ppr n, dcolon, ppr k]

instance (Outputable thing) => Outputable
         (HsImplicitBndrs name thing) where
        ppr (HsIB{hsib_body = ty}) = ppr ty

instance (Outputable thing) => Outputable
         (HsWildCardBndrs name thing) where
        ppr (HsWC{hswc_body = ty}) = ppr ty

instance Outputable (HsWildCardInfo name) where
        ppr (AnonWildCard _) = char '_'

pprHsForAll ::
              OutputableBndr name =>
              [LHsTyVarBndr name] -> LHsContext name -> SDoc
pprHsForAll = pprHsForAllExtra Nothing

pprHsForAllExtra ::
                   OutputableBndr name =>
                   Maybe SrcSpan -> [LHsTyVarBndr name] -> LHsContext name -> SDoc
pprHsForAllExtra extra qtvs cxt
  = pprHsForAllTvs qtvs <+> pprHsContextExtra show_extra (unLoc cxt)
  where show_extra = isJust extra

pprHsForAllTvs ::
                 OutputableBndr name => [LHsTyVarBndr name] -> SDoc
pprHsForAllTvs qtvs
  | show_forall = forAllLit <+> interppSP qtvs <> dot
  | otherwise = empty
  where show_forall = opt_PprStyle_Debug || not (null qtvs)

pprHsContext :: (OutputableBndr name) => HsContext name -> SDoc
pprHsContext = maybe empty (<+> darrow) . pprHsContextMaybe

pprHsContextNoArrow ::
                      (OutputableBndr name) => HsContext name -> SDoc
pprHsContextNoArrow = fromMaybe empty . pprHsContextMaybe

pprHsContextMaybe ::
                    (OutputableBndr name) => HsContext name -> Maybe SDoc
pprHsContextMaybe [] = Nothing
pprHsContextMaybe [L _ pred] = Just $ ppr_mono_ty FunPrec pred
pprHsContextMaybe cxt = Just $ parens (interpp'SP cxt)

pprHsContextExtra ::
                    (OutputableBndr name) => Bool -> HsContext name -> SDoc
pprHsContextExtra show_extra ctxt
  | not show_extra = pprHsContext ctxt
  | null ctxt = char '_' <+> darrow
  | otherwise = parens (sep (punctuate comma ctxt')) <+> darrow
  where ctxt' = map ppr ctxt ++ [char '_']

pprConDeclFields ::
                   OutputableBndr name => [LConDeclField name] -> SDoc
pprConDeclFields fields
  = braces (sep (punctuate comma (map ppr_fld fields)))
  where ppr_fld
          (L _
             (ConDeclField{cd_fld_names = ns, cd_fld_type = ty,
                           cd_fld_doc = doc}))
          = ppr_names ns <+> dcolon <+> ppr ty <+> ppr_mbDoc doc
        ppr_names [n] = ppr n
        ppr_names ns = sep (punctuate comma (map ppr ns))

pprHsType, pprParendHsType ::
             (OutputableBndr name) => HsType name -> SDoc
pprHsType ty = ppr_mono_ty TopPrec (prepare ty)
pprParendHsType ty = ppr_mono_ty TyConPrec ty

prepare :: HsType name -> HsType name
prepare (HsParTy ty) = prepare (unLoc ty)
prepare (HsAppsTy [L _ (HsAppPrefix (L _ ty))]) = prepare ty
prepare ty = ty

ppr_mono_lty ::
               (OutputableBndr name) => TyPrec -> LHsType name -> SDoc
ppr_mono_lty ctxt_prec ty = ppr_mono_ty ctxt_prec (unLoc ty)

ppr_mono_ty ::
              (OutputableBndr name) => TyPrec -> HsType name -> SDoc
ppr_mono_ty ctxt_prec (HsForAllTy{hst_bndrs = tvs, hst_body = ty})
  = maybeParen ctxt_prec FunPrec $
      sep [pprHsForAllTvs tvs, ppr_mono_lty TopPrec ty]
ppr_mono_ty ctxt_prec
  (HsQualTy{hst_ctxt = L _ ctxt, hst_body = ty})
  = maybeParen ctxt_prec FunPrec $
      sep [pprHsContext ctxt, ppr_mono_lty TopPrec ty]
ppr_mono_ty _ (HsBangTy b ty) = ppr b <> ppr_mono_lty TyConPrec ty
ppr_mono_ty _ (HsRecTy flds) = pprConDeclFields flds
ppr_mono_ty _ (HsTyVar (L _ name)) = pprPrefixOcc name
ppr_mono_ty prec (HsFunTy ty1 ty2) = ppr_fun_ty prec ty1 ty2
ppr_mono_ty _ (HsTupleTy con tys)
  = tupleParens std_con (pprWithCommas ppr tys)
  where std_con
          = case con of
                HsUnboxedTuple -> UnboxedTuple
                _ -> BoxedTuple
ppr_mono_ty _ (HsKindSig ty kind)
  = parens (ppr_mono_lty TopPrec ty <+> dcolon <+> ppr kind)
ppr_mono_ty _ (HsListTy ty) = brackets (ppr_mono_lty TopPrec ty)
ppr_mono_ty _ (HsPArrTy ty) = paBrackets (ppr_mono_lty TopPrec ty)
ppr_mono_ty prec (HsIParamTy n ty)
  = maybeParen prec FunPrec
      (ppr n <+> dcolon <+> ppr_mono_lty TopPrec ty)
ppr_mono_ty _ (HsSpliceTy s _) = pprSplice s
ppr_mono_ty _ (HsCoreTy ty) = ppr ty
ppr_mono_ty _ (HsExplicitListTy _ tys)
  = quote $ brackets (interpp'SP tys)
ppr_mono_ty _ (HsExplicitTupleTy _ tys)
  = quote $ parens (interpp'SP tys)
ppr_mono_ty _ (HsTyLit t) = ppr_tylit t
ppr_mono_ty _ (HsWildCardTy (AnonWildCard _)) = char '_'
ppr_mono_ty ctxt_prec (HsEqTy ty1 ty2)
  = maybeParen ctxt_prec TyOpPrec $
      ppr_mono_lty TyOpPrec ty1 <+> char '~' <+>
        ppr_mono_lty TyOpPrec ty2
ppr_mono_ty ctxt_prec (HsAppsTy tys)
  = maybeParen ctxt_prec TyConPrec $
      hsep (map (ppr_app_ty TopPrec . unLoc) tys)
ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty)
  = maybeParen ctxt_prec TyConPrec $
      hsep [ppr_mono_lty FunPrec fun_ty, ppr_mono_lty TyConPrec arg_ty]
ppr_mono_ty ctxt_prec (HsOpTy ty1 (L _ op) ty2)
  = maybeParen ctxt_prec TyOpPrec $
      sep
        [ppr_mono_lty TyOpPrec ty1,
         sep [pprInfixOcc op, ppr_mono_lty TyOpPrec ty2]]
ppr_mono_ty _ (HsParTy ty) = parens (ppr_mono_lty TopPrec ty)
ppr_mono_ty ctxt_prec (HsDocTy ty doc)
  = maybeParen ctxt_prec TyOpPrec $
      ppr_mono_lty TyOpPrec ty <+> ppr (unLoc doc)

ppr_fun_ty ::
             (OutputableBndr name) =>
             TyPrec -> LHsType name -> LHsType name -> SDoc
ppr_fun_ty ctxt_prec ty1 ty2
  = let p1 = ppr_mono_lty FunPrec ty1
        p2 = ppr_mono_lty TopPrec ty2
      in maybeParen ctxt_prec FunPrec $ sep [p1, text "->" <+> p2]

ppr_app_ty ::
             OutputableBndr name => TyPrec -> HsAppType name -> SDoc
ppr_app_ty _ (HsAppInfix (L _ n)) = pprInfixOcc n
ppr_app_ty _ (HsAppPrefix (L _ (HsTyVar (L _ n)))) = pprPrefixOcc n
ppr_app_ty ctxt (HsAppPrefix ty) = ppr_mono_lty ctxt ty

ppr_tylit :: HsTyLit -> SDoc
ppr_tylit (HsNumTy _ i) = integer i
ppr_tylit (HsStrTy _ s) = text (show s)