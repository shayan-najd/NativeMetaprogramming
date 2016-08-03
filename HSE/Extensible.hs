{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE PatternSynonyms,
             LambdaCase,
             GADTs,
             PolyKinds,
             TypeOperators,
             DeriveDataTypeable,
             RankNTypes,
             ScopedTypeVariables,
             TemplateHaskell,
             DataKinds #-}
module Extensible where

import Language.Haskell.TH hiding (cxt)
import Language.Haskell.TH.Syntax
import Data.Char
import Data.List (nubBy)
import GHC.TypeLits
import Data.Data

apps :: Type -> [Type] -> Type
apps = foldl AppT

addXtoName :: Name -> Name
addXtoName (Name (OccName occ)   _) = mkNameS (occ ++ "X")

addExttoName :: Name -> Name
addExttoName (Name (OccName occ)   _) = mkNameS (occ ++ "Ext")


fieldName :: Name -> Name
fieldName (Name (OccName (o:cc)) _) = mkNameS ((toLower o : cc) ++ "X")
fieldName _                         = error "Impossible!"

updateKind :: Maybe Kind -> Maybe Kind
updateKind Nothing  = Nothing
updateKind (Just _) = error "Updating kind signatures is not supported yet"

pattern NoBang :: Bang
pattern NoBang = Bang NoSourceUnpackedness NoSourceStrictness

appVarLit :: Name -> Name -> Type
appVarLit n n' = AppT (VarT n) (LitT (StrTyLit (getNameString n')))

addXtoHeadName :: Type -> Type
addXtoHeadName = \case
  ConT    n     -> ConT (addXtoName n)
  AppT    a b   -> AppT (addXtoHeadName a) b
  InfixT  a n b -> InfixT  a (addXtoName n) b
  UInfixT a n b -> UInfixT a (addXtoName n) b
  ParensT a     -> ParensT (addXtoHeadName a)
  a             -> a

addExt :: Type -> Type -> Type
addExt ext t = AppT t ext

addParameters :: [Name] -> Type -> Type -> Type
addParameters ns e t =
  case t of
    ConT    n     -> if n `elemName` ns
                     then addExt e (ConT n)
                     else t
    InfixT  a n b -> if n `elemName` ns
                     then apps (addExt e (ConT n)) [a, b]
                     else t
    UInfixT a n b -> if n `elemName` ns
                     then apps (addExt e (ConT n)) [a, b]
                     else t
    a             -> a

updateCon :: Name -> Con -> [Con]
updateCon r =  \case
  NormalC  n  ts    -> [NormalC  n ((NoBang, appVarLit r n) : ts)]
  RecC     n  vts   -> [RecC     n ((fieldName n , NoBang, appVarLit r n) : vts)]
  InfixC   t1 n  t2 -> [NormalC  n ((NoBang, appVarLit r n) : [t1,t2])]
  GadtC    ns ts  a -> [GadtC    [n] ((NoBang, appVarLit r n) : ts) a | n <- ns]
  RecGadtC ns vts a -> [RecGadtC [n]
                        ((fieldName n, NoBang, appVarLit r n) : vts) a | n <- ns]
  ForallC  _vs _cxt _con -> error "forall quantifications are not supported yet"

updateDeriving :: Cxt -> Cxt
updateDeriving [] = []
updateDeriving _  = error "Updating automatic derivings is not supported yet"

isConT :: forall a. Typeable a => a -> Bool
isConT x
  = case eqT of
      Just (Refl :: (a :~: Type)) ->
        case x of
          ConT _ -> True
          _      -> False
      Nothing                  -> False

collectNames :: Data a => a -> [Name]
collectNames ts = nubBy eqName [n | ConT n <- listify isConT ts]

eqName :: Name -> Name -> Bool
eqName n n' = getNameString n == getNameString n'

dependency :: [Name] -> Dec -> Maybe Name
dependency ns (DataD _ n _ _ cs _) = if any (`elemName` ns) (collectNames cs)
                                     then Just n
                                     else Nothing
dependency ns (TySynD n _ ts)      = if any (`elemName` ns) (collectNames ts)
                                     then Just n
                                     else Nothing
dependency _ _                     = Nothing

dependencies' :: [Name] -> [Dec] -> [Dec] -> [Name]
dependencies' ns _      []       = ns
dependencies' ns noDeps (d : ds) =
  case dependency ns d of
    Nothing -> dependencies' ns (d : noDeps) ds
    Just n  -> dependencies' (n : ns) [] (noDeps ++ ds)

dependencies :: [Name] -> [Dec] -> [Name]
dependencies ns ds = dependencies' ns [] ds

elemName :: Name -> [Name] -> Bool
elemName n ns = getNameString n `elem` map getNameString ns

extendDec :: [Name] -> Dec -> Dec
extendDec ns d =
  case d of
    DataD cxt nam vars mKind cons derivings ->
      if nam `elemName` ns
      then DataD cxt nam
                     ([PlainTV extVar] ++ vars)
                     mKind cons derivings
      else d
    TySynD nam vars ts                      ->
      if nam `elemName` ns
         then TySynD nam
                     ([PlainTV extVar] ++ vars) ts
         else d
    _                                       -> d

extVar :: Name
extVar = mkNameS "ext"

-- dummy constructors to carry data and simulate keywords
data Extends (s :: Symbol) = Extends String
data Extensible  = Extensible

 deriving Data

notData :: Dec -> Bool
notData = \case
  DataD{} -> False
  _       -> True

notExtAnn :: Dec -> Bool
notExtAnn = \case
  PragmaD (AnnP (TypeAnnotation _)
           (ConE e))                         -> not (e `eqName` 'Extensible)
  PragmaD (AnnP (TypeAnnotation _)
           (AppE (ConE e) (LitE (StringL _))))
                                             -> not (e `eqName` 'Extends)
  _                                          -> True

isGADT :: [Con] -> Bool
isGADT (GadtC {}    : _) = True
isGADT (RecGadtC {} : _) = True
isGADT _                 = False

getTyVarBndrName :: TyVarBndr -> Name
getTyVarBndrName = \case
  PlainTV  n   -> n
  KindedTV n _ -> n

getConNames :: Con -> [Name]
getConNames = \case
  NormalC  n  _    -> [n]
  RecC     n  _    -> [n]
  InfixC   _  n  _ -> [n]
  ForallC  _vs _cxt _con -> error "forall quantifications are not supported yet"
  GadtC    ns _  _ -> ns
  RecGadtC ns _  _ -> ns

getConBangTypes :: Con -> [BangType]
getConBangTypes = \case
  NormalC  _  ts    -> ts
  RecC     _  vts   -> map (\(_,b,t) -> (b,t)) vts
  InfixC   t1 _ t2  -> [t1,t2]
  GadtC    _ ts  _  -> ts
  RecGadtC _ vts _  -> map (\(_,b,t) -> (b,t)) vts
  ForallC  _vs _cxt _con -> error "forall quantifications are not supported yet"

getNameString :: Name -> String
getNameString (Name (OccName s) _) = s

fixLocalNames :: Name -> Name
fixLocalNames = mkNameS . getNameString

splitCons  :: Con -> [Con]
splitCons = \case
  GadtC    ns ts  t -> [GadtC    [n] ts  t | n <- ns]
  RecGadtC ns fts t -> [RecGadtC [n] fts t | n <- ns]
  a                 -> [a]

-- assumes that splitCons is applied already
newCons :: [Con] -> [Con] -> [Con]
newCons cs' = let rec = newCons cs' in
  \case
    []      -> []
    c : cs -> case getConBangTypes c of
                 (_, AppT (ConT n) (LitT (StrTyLit _))) :  _
                   | n `eqName` 'Extends -> rec cs
                 _                       -> c : rec cs

-- assumes splitCons
separateRowExtensions :: [Con] -> [(Name,String,[BangType])]
separateRowExtensions = \case
  []     -> []
  c : cs ->
    (case getConBangTypes c of
      (_, AppT (ConT n) (LitT (StrTyLit s))) :  ts
        | n `eqName` 'Extends -> ((head (getConNames c), s, ts) : )
      _                       -> id) (separateRowExtensions cs)

findCon :: Name -> [Con] -> Maybe [BangType]
findCon _ []       = Nothing
findCon n (c : cs) =
  if n `elemName` (getConNames c)
  then Just (getConBangTypes c)
  else findCon n cs


findDec :: Name -> [Dec] -> [Dec]
findDec _  []       = []
findDec n' (d@(DataD _  n _ _ _ _) : ds) = if n' `eqName` n
                                           then [d]
                                           else findDec n' ds
findDec n' (_ : ds)                      = findDec n' ds

botUp :: (Typeable b, Data a) => (b -> b) -> a -> a
botUp f = ifType f . gmapT (botUp f)

listify :: (Data a, Typeable b) => (b -> Bool) -> a -> [b]
listify p x  = foldl (++) (case cast x of
                              Just y  -> if p y then [y] else []
                              Nothing -> [])
                          (gmapQ (listify p) x)

ifType :: forall a b. (Typeable a, Typeable b) => (a -> a) -> (b -> b)
ifType f = case eqT of
             Nothing                  -> id
             Just (Refl :: (a :~: b)) -> f

rowExtendCon :: [Con] -> Name -> [TyVarBndr] -> Name -> Dec
rowExtendCon cs namX vs n
  = DataInstD []
              namX
              (map (VarT . getTyVarBndrName) vs ++
               [LitT (StrTyLit (getNameString n))])
              Nothing
              [NormalC (addXtoName n)
               (case filter (\ (_, b, _) -> b == getNameString n)
                              (separateRowExtensions cs) of
                  [(_,_,ts)] -> ts
                  _          -> [])]
              []

colExtendCon :: [Con] -> Name -> [TyVarBndr] -> Name -> Dec
colExtendCon cs namX vs n
  = DataInstD []
              namX
              (map (VarT . getTyVarBndrName) vs ++
               [LitT (StrTyLit (getNameString (addExttoName n)))])
              Nothing
              cs
              []

extensible :: Dec -> Dec
extensible (DataD cxt nam vars mKind cons derivings) =
 DataD cxt nam vars mKind
     ((if isGADT cons
       then GadtC [addExttoName nam] [(NoBang, appVarLit extVar
                                        (addExttoName nam))]
            (apps (ConT nam) (map (VarT . getTyVarBndrName) vars))
       else NormalC (addExttoName nam) [(NoBang, appVarLit extVar
                                          (addExttoName nam))]) :
      (concatMap (updateCon extVar) cons))
     (updateDeriving derivings)
extensible _ = error "Impossible!"

extending :: String -> Dec -> Dec -> [Dec]
extending ext (DataD [] nam  vars Nothing cons  [])
              (DataD [] nam' vars' Nothing cons' []) =
  let namX = mkNameS ext
      vts     = map (VarT . getTyVarBndrName) vars'
  in TySynD nam' vars'
      (apps (ConT nam)
       ([apps (ConT namX) vts] ++ (map (VarT . getTyVarBndrName) vars))) :
     colExtendCon (newCons cons (concatMap splitCons cons')) namX vars' nam :
     map (rowExtendCon cons' namX vars') (concatMap getConNames cons)
extending _ _ _ = error "Impossible"

desugarExtensible :: String -> Q [Dec] -> Q [Dec]
desugarExtensible ext ds = do
  ds' <- ds
  let nExtensibles =
        [n
        | PragmaD (AnnP (TypeAnnotation n) (ConE e)) <- ds',
          e `eqName` 'Extensible ]

      nExtenders =
        [(n,s)
        | PragmaD (AnnP (TypeAnnotation n)
                   (AppE (ConE e) (LitE (StringL s)))) <- ds',
          e `eqName` 'Extends ]

      dExtensibles =
        [ extensible d
        | n <- nExtensibles,
          d <- findDec n ds' ]

      dRest =
        [ d | d <- ds',
              case d of
                DataD _ n _ _ _ _ ->
                  not (n `elemName` nExtensibles ||
                       n `elemName` (map fst nExtenders))
                _                 -> notExtAnn d]

      dRestExtensibles = dExtensibles ++ dRest
      nsDeps = dependencies nExtensibles dRestExtensibles

      upd :: Data a => a -> a
      upd    = botUp (addParameters nsDeps (VarT extVar))

      dRestExtensibles' = upd (map (extendDec nsDeps) dRestExtensibles)

      dExtenders =
        concat [extending ext (upd d') (upd d)
               | (n,s) <- nExtenders ,
                 d     <- findDec n ds' ,
                 d'    <- findDec (mkName s) ds' ]
      dFinal = dRestExtensibles' ++
        (if length nExtenders /= 0
         then
           let vars' = head [ vs | (n , _) <- nExtenders,
                                   (DataD _ _ vs _ _ _) <- findDec n ds']
           in [DataFamilyD (mkNameS ext)
                (vars' ++ [KindedTV (mkNameS "lbl")
                           (ConT (mkNameS "Symbol"))]) Nothing] ++
              dExtenders
         else [])
  return (botUp fixLocalNames dFinal)
