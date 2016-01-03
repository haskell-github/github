{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Github.OrganizationsSpec where

import Github.Auth             (GithubAuth (..))
import Github.Data.Definitions (SimpleOrganization (..))
import Github.Organizations    (publicOrganizationsFor')

import Data.Aeson.Compat  (eitherDecodeStrict)
import Data.Either.Compat (isRight)
import Data.FileEmbed     (embedFile)
import System.Environment (lookupEnv)
import Test.Hspec         (Spec, describe, it, pendingWith, shouldBe,
                           shouldSatisfy)

fromRightS :: Show a => Either a b -> b
fromRightS (Right b) = b
fromRightS (Left a) = error $ "Expected a Right and got a Left" ++ show a

withAuth :: (GithubAuth -> IO ()) -> IO ()
withAuth action = do
  mtoken <- lookupEnv "GITHUB_TOKEN"
  case mtoken of
    Nothing    -> pendingWith "no GITHUB_TOKEN"
    Just token -> action (GithubOAuth token)

spec :: Spec
spec =
  describe "publicOrganizationsFor'" $ do
    it "decodes simple organization json" $ do
      let orgs = eitherDecodeStrict $(embedFile "fixtures/user-organizations.json")
      simpleOrganizationLogin (head $ fromRightS orgs) `shouldBe` "github"

    it "returns information about the user's organizations" $ withAuth $ \auth -> do
      orgs <- publicOrganizationsFor' (Just auth) "mike-burns"
      orgs  `shouldSatisfy` isRight
