{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE PatternSynonyms,
             LambdaCase,
             GADTs,
             PolyKinds,
             TypeOperators,
             DeriveDataTypeable,
             RankNTypes,
             ScopedTypeVariables,
             TemplateHaskell#-}
module Extensible where

import Language.Haskell.TH hiding (cxt)
import Language.Haskell.TH.Syntax
import Data.Char
import Data.Generics
import Data.List (nubBy)

apps :: Type -> [Type] -> Type
apps = foldl AppT

addXtoName :: Name -> Name
addXtoName (Name (OccName occ)   _) = mkNameS (occ ++ "X")

fieldName :: Name -> Name
fieldName (Name (OccName (o:cc)) _) = mkNameS ((toLower o : cc) ++ "X")
fieldName _                         = error "Impossible!"

updateKind :: Maybe Kind -> Maybe Kind
updateKind Nothing  = Nothing
updateKind (Just _) = error "Updating kind signatures is not supported yet"

pattern NoBang :: Bang
pattern NoBang = Bang NoSourceUnpackedness NoSourceStrictness

appVarLit :: Name -> Name -> Type
appVarLit n n' = AppT (VarT n) (LitT (StrTyLit (getString n')))

addXtoHeadName :: Type -> Type
addXtoHeadName = \case
  ConT    n     -> ConT (addXtoName n)
  AppT    a b   -> AppT (addXtoHeadName a) b
  InfixT  a n b -> InfixT  a (addXtoName n) b
  UInfixT a n b -> UInfixT a (addXtoName n) b
  ParensT a     -> ParensT (addXtoHeadName a)
  a             -> a

addRowCol :: Type -> Type -> Type -> Type
addRowCol r c t = apps t [r,c]

addParameters :: [Name] -> Type -> Type -> Type -> Type
addParameters ns r c t =
  case t of
    ConT    n     -> if n `elem` ns
                     then addRowCol r c (ConT (addXtoName n))
                     else t
    InfixT  a n b -> if n `elem` ns
                     then apps (addRowCol r c (ConT (addXtoName n))) [a, b]
                     else t
    UInfixT a n b -> if n `elem` ns
                     then apps (addRowCol r c (ConT (addXtoName n))) [a, b]
                     else t
    a             -> a

updateCon :: Name -> Con -> [Con]
updateCon r =  \case
  NormalC  n  ts    -> [NormalC n ((NoBang, appVarLit r n) : ts)]
  RecC     n  vts   -> [RecC    n ((fieldName n , NoBang, appVarLit r n) : vts)]
  InfixC   t1 n  t2 -> [NormalC n ((NoBang, appVarLit r n) : [t1,t2])]
  GadtC    ns ts  a -> [GadtC [n] ((NoBang, appVarLit r n) : ts) a | n <- ns]
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
eqName n n' = getString n == getString n'

dependency :: [Name] -> Dec -> Maybe Name
dependency ns (DataD _ n _ _ cs _) = if any (`elemName` ns) (collectNames cs)
                                     then Just n
                                     else Nothing
dependency ns (TySynD n _ ts)      = if any (`elemName` ns) (collectNames ts)
                                     then Just n
                                     else Nothing
dependency _ _                     = error ("Only data declarations and" ++
                                            "type synonyms are supported")

dependencies' :: [Name] -> [Dec] -> [Dec] -> [Name]
dependencies' ns _      []       = ns
dependencies' ns noDeps (d : ds) =
  case dependency ns d of
    Nothing -> dependencies' ns (d : noDeps) ds
    Just n  -> dependencies' (n : ns) [] (noDeps ++ ds)

dependencies :: [Name] -> [Dec] -> [Name]
dependencies ns ds = dependencies' ns [] ds

elemName :: Name -> [Name] -> Bool
elemName n ns = getString n `elem` map getString ns

extendDec :: [Name] -> Dec -> Dec
extendDec ns d =
  case d of
    DataD cxt nam vars mKind cons derivings ->
      if nam `elemName` ns
      then DataD cxt (addXtoName nam)
                     ([PlainTV rowVar,  PlainTV colVar] ++ vars)
                     mKind cons derivings
      else d
    TySynD nam vars ts                      ->
      if nam `elemName` ns
         then TySynD (addXtoName nam)
                     ([PlainTV rowVar,  PlainTV colVar] ++ vars) ts
         else d
    _                                       ->
      error "Only data declarations and type synonyms are supported"

rowVar :: Name
rowVar = mkName "row"

colVar :: Name
colVar = mkName "col"

extensible' :: Dec -> Dec
extensible' (DataD cxt nam vars mKind cons derivings) =
 let nam'   = addXtoName nam
 in DataD cxt nam vars mKind
     ((if isGADT cons
       then GadtC [nam'] [(NoBang, appVarLit colVar nam')]
            (apps (ConT nam) (map (VarT . getTyVarBndrName) vars))
       else NormalC nam' [(NoBang, appVarLit colVar nam')]) :
      (concatMap (updateCon rowVar) cons))
     (updateDeriving derivings)
extensible' _ = error "Impossible!"

data Extensiblity = Extensible
                  | Extends String
 deriving Data

notData :: Dec -> Bool
notData = \case
  DataD{} -> False
  _       -> True

notExtAnn :: Dec -> Bool
notExtAnn = \case
  PragmaD (AnnP (TypeAnnotation _) (ConE e)) -> not (e `eqName` 'Extensible)
  _                                          -> True

desugarExtensible :: Q [Dec] -> Q [Dec]
desugarExtensible ds = do ds' <- ds
                          let nExtensibles  =
                                [n | PragmaD
                                      (AnnP (TypeAnnotation n) (ConE e)) <- ds',
                                     e `eqName` 'Extensible ]
                          let ds'' = [ extensible' d
                                     | d@(DataD _ n _ _ _ _)  <- ds',
                                       n `elemName` nExtensibles] ++
                                     [ d
                                     | d@(DataD _ n _ _ _ _)  <- ds',
                                       not (n `elemName` nExtensibles)]
                                     ++ filter (\ d -> notData d &&
                                                       notExtAnn  d) ds'
                          let nsDeps = dependencies nExtensibles ds''
                          return(map (extendDec nsDeps) $
                                 botUp (addParameters nsDeps (VarT rowVar)
                                                             (VarT colVar))
                                 ds'')

isGADT :: [Con] -> Bool
isGADT (GadtC{}    : _) = True
isGADT (RecGadtC{} : _) = True
isGADT _                = False

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


findCon :: Name -> [Con] -> Maybe [BangType]
findCon _ []       = Nothing
findCon n (c : cs) = if getString n `elem` (map getString (getConNames c))
                     then Just (getConBangTypes c)
                     else findCon n cs

addRowXtoName :: Name -> Name
addRowXtoName (Name (OccName occ) _) =  mkNameS (occ ++ "RowX")

addColXtoName :: Name -> Name
addColXtoName (Name (OccName occ) _) =  mkNameS (occ ++ "ColX")

getString :: Name -> String
getString (Name (OccName s) _) = s

rowExtendCon :: [Con] -> Name -> [TyVarBndr] -> Name -> Dec
rowExtendCon cs namRowX vs n
  = DataInstD []
              namRowX
              (map (VarT . getTyVarBndrName) vs ++
               [LitT (StrTyLit (getString n))])
              Nothing
              [NormalC (addRowXtoName n)
               (case findCon n cs of
                  Nothing -> []
                  Just ts -> ts)]
              []

colExtendCon :: [Con] -> Name -> [TyVarBndr] -> Name -> Dec
colExtendCon cs namColX vs n
  = DataInstD []
              namColX
              (map (VarT . getTyVarBndrName) vs ++
               [LitT (StrTyLit (getString n))])
              Nothing
              cs
              []

splitCons  :: Con -> [Con]
splitCons = \case
  GadtC    ns ts  t -> [GadtC    [n] ts  t | n <- ns]
  RecGadtC ns fts t -> [RecGadtC [n] fts t | n <- ns]
  a                 -> [a]

-- assumes that splitCons is applied already
newCons :: [Con] -> [Con] -> [Con]
newCons cs' = let rec = newCons cs' in
  \case
    []       -> []
    (c : cs) -> if head (map getString (getConNames c))
                   `elem` map getString (concatMap getConNames cs')
                then rec cs
                else c : rec cs

extending' :: String -> [Name] -> Name -> Dec -> Q [Dec]
extending' ext ns bas (DataD [] nam' vars' Nothing cons' []) = do
  TyConI (DataD [] nam _vars Nothing cons []) <- qReify (addXtoName bas)
  let ns'     = ns
      namRowX = addRowXtoName (mkName ext)
      namColX = addColXtoName (mkName ext)
      vts     = map (VarT . getTyVarBndrName) vars'
      upd     = botUp (addParameters ns' (apps (ConT namRowX) vts)
                                         (apps (ConT namColX) vts))
  return $ upd
    (TySynD nam' vars'
      (apps (ConT nam)
       ([apps (ConT namRowX) vts, apps (ConT namColX) vts] ++ vts)) :
     colExtendCon (newCons cons (concatMap splitCons cons')) namColX vars' nam :
     map (rowExtendCon cons' namRowX vars') (concatMap getConNames cons))
extending' _ _ _ _ = error "Impossible"


findDec :: Name -> [Dec] -> [Dec]
findDec _  []       = []
findDec n' (d@(DataD _  n _ _ _ _) : ds) = if n' `eqName` n
                                           then [d]
                                           else findDec n' ds
findDec n' (_ : ds)                      = findDec n' ds

extending :: String -> [Name] -> Q [Dec] -> Q [Dec]
extending ext ns ds = do
  let namRowX = addRowXtoName (mkName ext)
      namColX = addColXtoName (mkName ext)
  ds' <- ds
  let nExtenders = [(n,s) | PragmaD (AnnP (TypeAnnotation n)
                              (AppE (ConE e) (LitE (StringL s)))) <- ds',
                            e `eqName` 'Extends ]
  eds <- fmap concat (sequence [extending' ext ns (mkName s) d
                               | (n , s) <- nExtenders,
                                 d       <- findDec n ds'])
  -- what if no datatype to extend?
  let vars' = head [vs | (n , _) <- nExtenders,
                         (DataD _ _ vs _ _ _) <- findDec n ds']
  return
    (DataFamilyD namRowX
     (vars' ++ [KindedTV (mkName "lbl") (ConT (mkNameS "Symbol"))]) Nothing :
     DataFamilyD namColX
     (vars' ++ [KindedTV (mkName "lbl") (ConT (mkNameS "Symbol"))]) Nothing :
     eds)

topDown :: (Typeable b, Data a) => (b -> b) -> a -> a
topDown f = everywhere' (ifType f)

botUp :: (Typeable b, Data a) => (b -> b) -> a -> a
botUp   f = everywhere  (ifType f)


ifType :: forall a b. (Typeable a, Typeable b) => (a -> a) -> (b -> b)
ifType f = case eqT of
             Nothing                  -> id
             Just (Refl :: (a :~: b)) -> f
