{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module WiredTiger.Context where

import Language.C.Inline.Context
import qualified Data.Map.Strict as Map
import Language.C.Types
import WiredTiger.Types

wiredtigerCtx :: Context
wiredtigerCtx = mempty {
  ctxTypesTable = Map.fromList
    [ (TypeName "WT_CONNECTION", [t| ConnectionImpl |])
    , (TypeName "WT_SESSION", [t| SessionImpl |])
    , (TypeName "WT_CURSOR", [t| CursorImpl |])
    ]
}
