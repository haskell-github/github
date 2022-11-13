{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.Actions.ArtifactsSpec where

import qualified GitHub as GH

import Prelude ()
import Prelude.Compat

import           Data.Aeson         (eitherDecodeStrict)
import           Data.ByteString    (ByteString)
import           Data.Either.Compat (isRight)
import           Data.FileEmbed     (embedFile)
import           Data.Foldable      (for_)
import           Data.String        (fromString)
import qualified Data.Vector        as V
import           System.Environment (lookupEnv)
import           Test.Hspec
                 (Spec, describe, it, pendingWith, shouldBe, shouldSatisfy)

fromRightS :: Show a => Either a b -> b
fromRightS (Right b) = b
fromRightS (Left a) = error $ "Expected a Right and got a Left" ++ show a

withAuth :: (GH.Auth -> IO ()) -> IO ()
withAuth action = do
    mtoken <- lookupEnv "GITHUB_TOKEN"
    case mtoken of
        Nothing    -> pendingWith "no GITHUB_TOKEN"
        Just token -> action (GH.OAuth $ fromString token)

spec :: Spec
spec = do
    describe "artifactsForR" $ do
        it "works" $ withAuth $ \auth -> for_ repos $ \(owner, repo) -> do
            cs <- GH.executeRequest auth $
                GH.artifactsForR owner repo GH.FetchAll
            cs `shouldSatisfy` isRight

    describe "decoding artifacts payloads" $ do
        it "decodes artifacts list payload" $ do
            GH.withTotalCountTotalCount artifactList `shouldBe` 23809
            V.length (GH.withTotalCountItems artifactList) `shouldBe` 2
        it "decodes signle artifact payload" $ do
            GH.artifactName artifact `shouldBe` "dist-without-markdown"
            GH.workflowRunHeadSha (GH.workflowRun artifact) `shouldBe` "601593ecb1d8a57a04700fdb445a28d4186b8954"

  where
    repos =
      [ ("thoughtbot", "paperclip")
      , ("phadej", "github")
      ]

    artifactList :: GH.WithTotalCount GH.Artifact
    artifactList =
        fromRightS (eitherDecodeStrict artifactsListPayload)

    artifact :: GH.Artifact
    artifact =
        fromRightS (eitherDecodeStrict artifactPayload)

    artifactsListPayload :: ByteString
    artifactsListPayload = $(embedFile "fixtures/actions/artifacts-list.json")

    artifactPayload :: ByteString
    artifactPayload = $(embedFile "fixtures/actions/artifact.json")
