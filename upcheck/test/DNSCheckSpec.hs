{-# LANGUAGE TypeApplications #-}

module DNSCheckSpec (spec) where

import Autodocodec.Nix
import Test.Syd
import UpCheck

spec :: Spec
spec = do
  describe "Specification" $
    it "outputs the same nix options" $
      pureGoldenTextFile "options.nix" $
        renderNixOptionTypeViaCodec @CheckSpec
