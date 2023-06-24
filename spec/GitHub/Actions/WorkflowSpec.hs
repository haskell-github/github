{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.Actions.WorkflowSpec where

import qualified GitHub as GH

import Prelude ()
import Prelude.Compat

import           Data.Aeson      (eitherDecodeStrict)
import           Data.ByteString (ByteString)
import           Data.FileEmbed  (embedFile)
import qualified Data.Vector     as V
import           Test.Hspec      (Spec, describe, it, shouldBe)

fromRightS :: Show a => Either a b -> b
fromRightS (Right b) = b
fromRightS (Left a) = error $ "Expected a Right and got a Left" ++ show a

spec :: Spec
spec = do
    describe "decoding workflow payloads" $ do
        it "decodes workflow list" $ do
            V.length (GH.withTotalCountItems workflowList) `shouldBe` 1

  where
    workflowList:: GH.WithTotalCount GH.Workflow
    workflowList =
        fromRightS (eitherDecodeStrict workflowPayload)

    workflowPayload :: ByteString
    workflowPayload = $(embedFile "fixtures/actions/workflow-list.json")
