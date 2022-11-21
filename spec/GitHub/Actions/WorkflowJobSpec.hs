{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.Actions.WorkflowJobSpec where

import qualified GitHub as GH
import GitHub.Data.Id

import Prelude ()
import Prelude.Compat

import           Data.Aeson         (eitherDecodeStrict)
import           Data.ByteString    (ByteString)
import           Data.FileEmbed     (embedFile)
import           Test.Hspec
                 (Spec, describe, it, shouldBe)

fromRightS :: Show a => Either a b -> b
fromRightS (Right b) = b
fromRightS (Left a) = error $ "Expected a Right and got a Left" ++ show a

spec :: Spec
spec = do
    describe "decoding workflow jobs payloads" $ do
        it "decodes workflow job" $ do
            GH.jobId workflowJob `shouldBe` Id 9183275828

  where
    workflowJob:: GH.Job
    workflowJob=
        fromRightS (eitherDecodeStrict workflowJobPayload)

    workflowJobPayload :: ByteString
    workflowJobPayload = $(embedFile "fixtures/actions/workflow-job.json")