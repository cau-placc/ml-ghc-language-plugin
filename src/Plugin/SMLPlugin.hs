module Plugin.SMLPlugin (plugin) where

import GHC.Plugins
import Language.Haskell.TH (Extension(..))

import Plugin.LanguagePlugin
import Plugin.Trans.Config

plugin :: Plugin
plugin = addStrictFlag (setConfigFlagsFor flags languagePlugin)
  where
    flags = [ (fst monadModConfigStr, "Plugin.SMLPlugin.Monad")
            , (fst monadNameConfigStr, "SML")
            , (fst funModConfigStr, "Plugin.SMLPlugin.Monad")
            , (fst funNameConfigStr, "-->")
            , (fst preludeModConfigStr, "Plugin.SMLPlugin.Prelude")
            , (fst builtInModConfigStr, "Plugin.SMLPlugin.BuiltIn") ]

addStrictFlag :: Plugin -> Plugin
addStrictFlag pl@(Plugin { driverPlugin = drivPl }) =
  pl { driverPlugin = setStrict }
  where
    setStrict cmdOpts hsc = do
      hsc' <- drivPl cmdOpts hsc
      let pluginFlags = hsc_dflags hsc' `xopt_set` StrictData -- `xopt_set` Strict
      return (hsc' { hsc_dflags = pluginFlags } )
