module Test.Main where

import Control.Monad.Trans.MSF.Maybe (listToMaybeS, runMaybeT)
import Control.Monad.Writer.Trans (lift, tell)
import Data.MonadicStreamFunction (arrM, embed)

import Control.Monad.Writer (execWriter)
import Data.List ((..), fromFoldable)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Prelude (Unit, flip, ($), (>>>))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "listToMaybeS" do
    it "First emits all list elements before throwing exception" do
        "Hello" `shouldEqual` (execWriter $ runMaybeT $ flip embed ((1 :: Int)..10) $ listToMaybeS (fromFoldable ["H", "el", "lo"]) >>> arrM (tell >>> lift))
