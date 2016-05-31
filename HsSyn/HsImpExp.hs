{-# LANGUAGE DeriveDataTypeable #-}
module HsImpExp where
import Module (ModuleName)
import HsDoc (HsDocString)
import OccName (HasOccName(..), isTcOcc, isSymOcc)
import BasicTypes (SourceText, StringLiteral(..))
import FieldLabel (FieldLbl(..))
import Outputable
import FastString
import SrcLoc
import Data.Data

type LImportDecl name = Located (ImportDecl name)

data ImportDecl name = ImportDecl{ideclSourceSrc ::
                                  Maybe SourceText,
                                  ideclName :: Located ModuleName,
                                  ideclPkgQual :: Maybe StringLiteral, ideclSource :: Bool,
                                  ideclSafe :: Bool, ideclQualified :: Bool, ideclImplicit :: Bool,
                                  ideclAs :: Maybe ModuleName,
                                  ideclHiding :: Maybe (Bool, Located [LIE name])}
                     deriving (Data, Typeable)

simpleImportDecl :: ModuleName -> ImportDecl name
simpleImportDecl mn
  = ImportDecl{ideclSourceSrc = Nothing, ideclName = noLoc mn,
               ideclPkgQual = Nothing, ideclSource = False, ideclSafe = False,
               ideclImplicit = False, ideclQualified = False, ideclAs = Nothing,
               ideclHiding = Nothing}

instance (OutputableBndr name, HasOccName name) => Outputable
         (ImportDecl name) where
        ppr
          (ImportDecl{ideclName = mod', ideclPkgQual = pkg,
                      ideclSource = from, ideclSafe = safe, ideclQualified = qual,
                      ideclImplicit = implicit, ideclAs = as, ideclHiding = spec})
          = hang
              (hsep
                 [text "import", ppr_imp from, pp_implicit implicit, pp_safe safe,
                  pp_qual qual, pp_pkg pkg, ppr mod', pp_as as])
              4
              (pp_spec spec)
          where pp_implicit False = empty
                pp_implicit True = ptext (sLit ("(implicit)"))
                pp_pkg Nothing = empty
                pp_pkg (Just (StringLiteral _ p)) = doubleQuotes (ftext p)
                pp_qual False = empty
                pp_qual True = text "qualified"
                pp_safe False = empty
                pp_safe True = text "safe"
                pp_as Nothing = empty
                pp_as (Just a) = text "as" <+> ppr a
                ppr_imp True = text "{-# SOURCE #-}"
                ppr_imp False = empty
                pp_spec Nothing = empty
                pp_spec (Just (False, (L _ ies))) = ppr_ies ies
                pp_spec (Just (True, (L _ ies))) = text "hiding" <+> ppr_ies ies
                ppr_ies [] = text "()"
                ppr_ies ies = char '(' <+> interpp'SP ies <+> char ')'

type LIE name = Located (IE name)

data IE name = IEVar (Located name)
             | IEThingAbs (Located name)
             | IEThingAll (Located name)
             | IEThingWith (Located name) IEWildcard [Located name]
                           [Located (FieldLbl name)]
             | IEModuleContents (Located ModuleName)
             | IEGroup Int HsDocString
             | IEDoc HsDocString
             | IEDocNamed String
             deriving (Eq, Data, Typeable)

data IEWildcard = NoIEWildcard
                | IEWildcard Int
                deriving (Eq, Data, Typeable)

ieName :: IE name -> name
ieName (IEVar (L _ n)) = n
ieName (IEThingAbs (L _ n)) = n
ieName (IEThingWith (L _ n) _ _ _) = n
ieName (IEThingAll (L _ n)) = n
ieName _ = panic "ieName failed pattern match!"

ieNames :: IE a -> [a]
ieNames (IEVar (L _ n)) = [n]
ieNames (IEThingAbs (L _ n)) = [n]
ieNames (IEThingAll (L _ n)) = [n]
ieNames (IEThingWith (L _ n) _ ns _) = n : map unLoc ns
ieNames (IEModuleContents _) = []
ieNames (IEGroup _ _) = []
ieNames (IEDoc _) = []
ieNames (IEDocNamed _) = []

pprImpExp :: (HasOccName name, OutputableBndr name) => name -> SDoc
pprImpExp name = type_pref <+> pprPrefixOcc name
  where occ = occName name
        type_pref
          | isTcOcc occ && isSymOcc occ = text "type"
          | otherwise = empty

instance (HasOccName name, OutputableBndr name) => Outputable
         (IE name) where
        ppr (IEVar var) = pprPrefixOcc (unLoc var)
        ppr (IEThingAbs thing) = pprImpExp (unLoc thing)
        ppr (IEThingAll thing)
          = hcat [pprImpExp (unLoc thing), text "(..)"]
        ppr (IEThingWith thing wc withs flds)
          = pprImpExp (unLoc thing) <>
              parens
                (fsep
                   (punctuate comma ppWiths ++ map (ppr . flLabel . unLoc) flds))
          where ppWiths
                  = case wc of
                        NoIEWildcard -> map (pprImpExp . unLoc) withs
                        IEWildcard pos -> let (bs, as)
                                                = splitAt pos (map (pprImpExp . unLoc) withs)
                                            in bs ++ [text ".."] ++ as
        ppr (IEModuleContents mod') = text "module" <+> ppr mod'
        ppr (IEGroup n _) = text ("<IEGroup: " ++ show n ++ ">")
        ppr (IEDoc doc) = ppr doc
        ppr (IEDocNamed string) = text ("<IEDocNamed: " ++ string ++ ">")