{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module WiredTiger.Context where

import Language.C.Inline.Context
import qualified Data.Map.Strict as Map
import Language.C.Types

wiredtigerCtx :: Context
wiredtigerCtx = mempty {
  ctxTypesTable = Map.fromList
    [ (TypeName "WT_CONNECTION", [t| () |])
    , (TypeName "WT_SESSION", [t| () |])
    , (TypeName "WT_CURSOR", [t| () |])
    ]
}
