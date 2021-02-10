module Plugin.SMLPlugin (plugin) where

import GHC.Plugins

import Plugin.LanguagePlugin
import Plugin.Trans.Config

plugin :: Plugin
plugin = setConfigFlagsFor flags languagePlugin
  where
    flags = [ (fst monadModConfigStr, "Plugin.Effect.Monad")
            , (fst monadNameConfigStr, "SML")
            , (fst preludeModConfigStr, "Plugin.SMLPlugin.Prelude")
            , (fst builtInModConfigStr, "Plugin.SMLPlugin.BuiltIn") ]
