{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GitHub.Actions.SecretsSpec where

import qualified GitHub as GH

import Prelude ()
import Prelude.Compat

import           Data.Aeson         (eitherDecodeStrict)
import           Data.ByteString    (ByteString)
import           Data.FileEmbed     (embedFile)
import qualified Data.Vector        as V
import           Test.Hspec
                 (Spec, describe, it, shouldBe)

fromRightS :: Show a => Either a b -> b
fromRightS (Right b) = b
fromRightS (Left a) = error $ "Expected a Right and got a Left" ++ show a

spec :: Spec
spec = do
    describe "decoding secrets payloads" $ do
        it "decodes selected repo list payload" $ do
            V.length (GH.withTotalCountItems repoList) `shouldBe` 1
        it "decodes secret list payload" $ do
            V.length (GH.withTotalCountItems orgSecretList) `shouldBe` 2
        it "decodes public key payload" $ do
            GH.publicKeyId orgPublicKey `shouldBe` "568250167242549743"

  where
    repoList :: GH.WithTotalCount GH.SelectedRepo
    repoList =
        fromRightS (eitherDecodeStrict repoListPayload)

    orgSecretList:: GH.WithTotalCount GH.OrganizationSecret
    orgSecretList=
        fromRightS (eitherDecodeStrict orgSecretListPayload)

    orgPublicKey:: GH.PublicKey
    orgPublicKey=
        fromRightS (eitherDecodeStrict orgPublicKeyPayload)

    repoListPayload :: ByteString
    repoListPayload = $(embedFile "fixtures/actions/selected-repositories-for-secret.json")

    orgSecretListPayload :: ByteString
    orgSecretListPayload = $(embedFile "fixtures/actions/org-secrets-list.json")

    orgPublicKeyPayload :: ByteString
    orgPublicKeyPayload = $(embedFile "fixtures/actions/org-public-key.json")