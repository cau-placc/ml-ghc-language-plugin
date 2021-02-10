module Plugin.SMLPlugin (plugin) where

import GHC.Plugins
import Language.Haskell.TH (Extension(..))

import Plugin.LanguagePlugin
import Plugin.Trans.Config

plugin :: Plugin
plugin = addStrictFlag (setConfigFlagsFor flags languagePlugin)
  where
    flags = [ (fst monadModConfigStr, "Plugin.Effect.Monad")
            , (fst monadNameConfigStr, "SML")
            , (fst preludeModConfigStr, "Plugin.SMLPlugin.Prelude")
            , (fst builtInModConfigStr, "Plugin.SMLPlugin.BuiltIn") ]

addStrictFlag :: Plugin -> Plugin
addStrictFlag pl@(Plugin { dynflagsPlugin = dflagsPl }) =
  pl { dynflagsPlugin = setStrict }
  where
    setStrict cmdOpts flags = do
      pluginFlags <- dflagsPl cmdOpts flags
      return (pluginFlags `xopt_set` Strict `xopt_set` StrictData)
