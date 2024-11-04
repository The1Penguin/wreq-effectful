# wreq-effectful

Adaptation of the [wreq](https://hackage.haskell.org/package/wreq)
library for the effectful ecosystem.

## Example

A sample usage for logging to both standard output and Elasticsearch:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Effectful
import Effectful.Wreq
import Control.Lens

main :: IO ()
main = runEff . runWreq $
    getWith (defaults & header "Accept" .~ ["application/json"]) "https://hackage.haskell.org/users/" >>=
    asValue >>=
    liftIO . print . (^. responseBody)
```
