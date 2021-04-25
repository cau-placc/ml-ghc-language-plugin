# Standard ML plugin for GHC

The goal of this project is to make prototypical implementation of a plugin
for the Glasgow Haskell Compiler (GHC),
such that the GHC can be used to compile strict Standard ML-style programs.

## Compatibility

This plugin only works with a specific GHC 9.1 commit and cannot be used with other versions. It will be updated to 9.2, as soon as that GHC is released

## Using the plugin
The plugin can be activated within a module by adding both
`{-# OPTIONS_GHC -fplugin Plugin.SMLPlugin   #-}` to the top of the file.

This brings a few basic functions, data types and type classes into scope, as well as some side-effect operations.

To evaluate an ML-Style computation, the user can import the computation in an ordinary Haskell module and use a function from `Plugin.SMLPlugin.Encapsulation` to execute it.

## Using the plugin in a sandbox

A sandbox project is available to play around with in `sandbox/`. It can be loaded by executing `stack repl sandbox` from the root of the repository.

## Known Issues

 - Adding instances of derivable type classes to primitive types is not possible
 - Most Language extensions are unsupported and will crash at compilation or run-time, but some of the extensions might work.
 - Sharing in let-expressions does not work in some edge-cases
 - Using `:r` in GHCi only works on the second try
 - Type errors sometimes mention the effectful versions of type constructors
 - HIE and HaskellLanguageServer do not work  
 - ~Stack outputs some decoding failures while compiling the project. This can be ignored safely.~ Fixed with stack version 2.3.3

## Debugging

In order see, when my plugin generates invalid code, use the GHC option `-dcore-lint`. This type checks the generated core-code and emits an error message when something went wrong. I am very interested in finding those issues.
This option can also be turned on via `{-# OPTIONS_GHC -dcore-lint #-}`
