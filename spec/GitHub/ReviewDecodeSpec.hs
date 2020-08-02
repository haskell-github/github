{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.ReviewDecodeSpec where

import Data.Aeson         (eitherDecodeStrict)
import Data.Either.Compat (isRight)
import Data.FileEmbed     (embedFile)
import Test.Hspec
       (Spec, describe, it, shouldSatisfy)

import GitHub.Data (Review)

spec :: Spec
spec = do
  describe "PENDING state" $ do
    -- https://docs.github.com/en/rest/reference/pulls#create-a-review-for-a-pull-request
    -- > Pull request reviews created in the PENDING state do not include the submitted_at property in the response.
    it "decodes review when submitted_at is missing" $ do
      let reviewInfo = eitherDecodeStrict $(embedFile "fixtures/pull-request-pending-review.json") :: Either String Review
      reviewInfo `shouldSatisfy` isRight

  describe "Other states" $ do
    it "decodes review" $ do
      let reviewInfo = eitherDecodeStrict $(embedFile "fixtures/pull-request-approved-review.json") :: Either String Review
      reviewInfo `shouldSatisfy` isRight
