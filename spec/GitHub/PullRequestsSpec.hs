{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module GitHub.PullRequestsSpec where

import qualified GitHub as GH

import Prelude ()
import Prelude.Compat

import           Data.Aeson
                 (FromJSON (..), eitherDecodeStrict, withObject, (.:))
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.Either.Compat         (isRight)
import           Data.FileEmbed             (embedFile)
import           Data.Foldable              (for_)
import           Data.String                (fromString)
import           Data.Tagged                (Tagged (..))
import           Data.Text                  (Text)
import qualified Data.Vector                as V
import           System.Environment         (lookupEnv)
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
    describe "pullRequestsForR" $ do
        it "works" $ withAuth $ \auth -> for_ repos $ \(owner, repo) -> do
            cs <- GH.executeRequest auth $
                GH.pullRequestsForR owner repo opts GH.FetchAll
            cs `shouldSatisfy` isRight

    describe "pullRequestPatchR" $
        it "works" $ withAuth $ \auth -> do
            Right patch <- GH.executeRequest auth $
                GH.pullRequestPatchR "phadej" "github" (GH.IssueNumber 349)
            head (LBS8.lines patch) `shouldBe` "From c0e4ad33811be82e1f72ee76116345c681703103 Mon Sep 17 00:00:00 2001"

    describe "decoding pull request payloads" $ do
        it "decodes a pull request 'opened' payload" $ do
            V.length (GH.simplePullRequestRequestedReviewers simplePullRequestOpened)
                `shouldBe` 0

            V.length (GH.pullRequestRequestedReviewers pullRequestOpened)
                `shouldBe` 0

        it "decodes a pull request 'review_requested' payload" $ do
            V.length (GH.simplePullRequestRequestedReviewers simplePullRequestReviewRequested)
                `shouldBe` 1

            V.length (GH.pullRequestRequestedReviewers pullRequestReviewRequested)
                `shouldBe` 1

        it "decodes a pull request 'team_requested' payload" $ do
          V.length (GH.simplePullRequestRequestedTeamReviewers simplePullRequestTeamReviewRequested)
                `shouldBe` 1

          V.length (GH.pullRequestRequestedTeamReviewers pullRequestTeamReviewRequested)
                `shouldBe` 1

    describe "checking if a pull request is merged" $ do
        it "works" $ withAuth $ \auth -> do
            b <- GH.executeRequest auth $ GH.isPullRequestMergedR "phadej" "github" (GH.IssueNumber 14)
            b `shouldSatisfy` isRight
            fromRightS b `shouldBe` True

    describe "Draft Pull Request" $ do
        it "works" $ withAuth $ \auth -> do
            cs <- GH.executeRequest auth $
                draftPullRequestsForR "phadej" "github" opts GH.FetchAll

            cs `shouldSatisfy` isRight

  where
    repos =
      [ ("thoughtbot", "paperclip")
      , ("phadej", "github")
      ]
    opts = GH.stateClosed

    simplePullRequestOpened :: GH.SimplePullRequest
    simplePullRequestOpened =
        fromRightS (eitherDecodeStrict prOpenedPayload)

    pullRequestOpened :: GH.PullRequest
    pullRequestOpened =
        fromRightS (eitherDecodeStrict prOpenedPayload)

    simplePullRequestReviewRequested :: GH.SimplePullRequest
    simplePullRequestReviewRequested =
        fromRightS (eitherDecodeStrict prReviewRequestedPayload)

    simplePullRequestTeamReviewRequested :: GH.SimplePullRequest
    simplePullRequestTeamReviewRequested =
        fromRightS (eitherDecodeStrict prTeamReviewRequestedPayload)

    pullRequestReviewRequested :: GH.PullRequest
    pullRequestReviewRequested =
        fromRightS (eitherDecodeStrict prReviewRequestedPayload)

    pullRequestTeamReviewRequested :: GH.PullRequest
    pullRequestTeamReviewRequested =
        fromRightS (eitherDecodeStrict prTeamReviewRequestedPayload)

    prOpenedPayload :: ByteString
    prOpenedPayload = $(embedFile "fixtures/pull-request-opened.json")

    prReviewRequestedPayload :: ByteString
    prReviewRequestedPayload = $(embedFile "fixtures/pull-request-review-requested.json")

    prTeamReviewRequestedPayload :: ByteString
    prTeamReviewRequestedPayload = $(embedFile "fixtures/pull-request-team-review-requested.json")

-------------------------------------------------------------------------------
-- Draft Pull Requests
-------------------------------------------------------------------------------

draftPullRequestsForR
    :: GH.Name GH.Owner
    -> GH.Name GH.Repo
    -> GH.PullRequestMod
    -> GH.FetchCount
    -> GH.GenRequest ('GH.MtPreview ShadowCat) k (V.Vector DraftPR)
draftPullRequestsForR user repo opts = GH.PagedQuery
    ["repos", GH.toPathPart user, GH.toPathPart repo, "pulls"]
    (GH.prModToQueryString opts)

data DraftPR = DraftPR
    { dprId     :: !(GH.Id GH.PullRequest)
    , dprNumber :: !GH.IssueNumber
    , dprTitle  :: !Text
    , dprDraft  :: !Bool
    }
  deriving (Show)

instance FromJSON DraftPR where
    parseJSON = withObject "DraftPR" $ \obj -> DraftPR
        <$> obj .: "id"
        <*> obj .: "number"
        <*> obj .: "title"
        <*> obj .: "draft"

-- | @application/vnd.github.shadow-cat-preview+json@ <https://developer.github.com/v3/previews/#draft-pull-requests>
data ShadowCat

instance GH.PreviewAccept ShadowCat where
    previewContentType = Tagged "application/vnd.github.shadow-cat-preview+json"

instance FromJSON a => GH.PreviewParseResponse ShadowCat a where
    previewParseResponse _ res = Tagged (GH.parseResponseJSON res)
