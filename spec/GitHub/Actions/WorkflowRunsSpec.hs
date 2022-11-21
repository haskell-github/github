{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.Actions.WorkflowRunsSpec where

import qualified GitHub as GH

import Prelude ()
import Prelude.Compat

import           Data.Aeson         (eitherDecodeStrict)
import           Data.ByteString    (ByteString)
import qualified Data.Vector as V
import           Data.FileEmbed     (embedFile)
import           Test.Hspec
                 (Spec, describe, it, shouldBe)

fromRightS :: Show a => Either a b -> b
fromRightS (Right b) = b
fromRightS (Left a) = error $ "Expected a Right and got a Left" ++ show a

spec :: Spec
spec = do
    describe "decoding workflow runs payloads" $ do
        it "decodes workflow runs list" $ do
            V.length (GH.withTotalCountItems workflowRunsList) `shouldBe` 3

  where
    workflowRunsList:: GH.WithTotalCount GH.WorkflowRun
    workflowRunsList =
        fromRightS (eitherDecodeStrict workflowRunsPayload)

    workflowRunsPayload :: ByteString
    workflowRunsPayload = $(embedFile "fixtures/actions/workflow-runs-list.json")