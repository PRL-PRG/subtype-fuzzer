{-|
Module      : QModel
Description : Model of quasi-types for Julia PL
License     : GPL-3
Maintainer  : a.pelenitsyn@email.com
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE DeriveDataTypeable #-}

module QModel where

import Data.Typeable
import Text.PrettyPrint.Annotated.WL

data QTau = H             -- Hole -- will turn into var or kind *-type
          | A QTau        -- Application of (some) invarint constructor
          | P QTau QTau   -- Tuple
          | U QTau QTau   -- Union
          deriving (Typeable)
{-
instance Show QTau where
    show H    = "☐"
    show (A t) = "Ref{" ++ show t ++ "}" 
    show (P t1 t2) = "Tuple{" ++ show t1 ++ ", " ++ show t2 ++ "}"
    show (U t1 t2) = "Union{" ++ show t1 ++ ", " ++ show t2 ++ "}"
-}

-- d1 d2 -> "{$d1, $d2}"

ind :: Integer
ind = 4

prettyTyCon :: (Pretty a2, Pretty a1) 
            => String -> a1 -> a2 -> Doc a3
prettyTyCon name t1 t2 = align $ text name                   <#>
                            indent 4 (lbrace <+> pretty t1)  <#>
                            indent 4 (comma <+> pretty t2)   <#>
                            rbrace

instance Pretty QTau where
    pretty H         = char '☐'
    pretty (A t)     = align $ text "Ref" <> lbrace <#> 
                            indent 4 (pretty t)     <#> 
                            rbrace
    pretty (P t1 t2) = prettyTyCon "Tuple" t1 t2
    pretty (U t1 t2) = prettyTyCon "Union" t1 t2

