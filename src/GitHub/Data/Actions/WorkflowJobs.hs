{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}

module GitHub.Data.Actions.WorkflowJobs (
    JobStep(..),
    Job(..),
    ) where

import GitHub.Data.Id          (Id)
import GitHub.Data.Name        (Name)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
       (Applicative ((<*>)), Data, Eq, FromJSON (parseJSON), Generic, Integer,
       Ord, Show, Text, Typeable, UTCTime, Vector, withObject, ($), (.:),
       (<$>))
import Prelude ()

import GitHub.Data.Actions.Common       (WithTotalCount (WithTotalCount))
import GitHub.Data.Actions.WorkflowRuns (WorkflowRun)


data JobStep = JobStep
    {
     jobStepName                 :: !(Name JobStep)
    , jobStepStatus                 :: !Text
    , jobStepConclusion                 :: !Text
    , jobStepNumber                 :: !Integer
    , jobStepStartedAt                 :: !UTCTime
    , jobStepCompletedAt                 :: !UTCTime
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

data Job = Job
    {
     jobId                 :: !(Id Job)
    , jobRunId                 :: !(Id WorkflowRun)
    , jobRunUrl                 :: !URL
    , jobRunAttempt                 :: !Integer
    , jobNodeId                 :: !Text
    , jobHeadSha                 :: !Text
    , jobUrl                 :: !URL
    , jobHtmlUrl                 :: !URL
    , jobStatus                 :: !Text
    , jobConclusion                 :: !Text
    , jobStartedAt                 :: !UTCTime
    , jobCompletedAt                 :: !UTCTime
    , jobSteps                 :: !(Vector JobStep)
    , jobRunCheckUrl                 :: !URL
    , jobLabels                 :: !(Vector Text)
    , jobRunnerId                 :: !Integer
    , jobRunnerName                 :: !Text
    , jobRunnerGroupId                 :: !Integer
    , jobRunnerGroupName                 :: !Text
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)


-------------------------------------------------------------------------------
-- JSON instances
-------------------------------------------------------------------------------

instance FromJSON JobStep where
    parseJSON = withObject "JobStep" $ \o -> JobStep
        <$> o .: "name"
        <*> o .: "status"
        <*> o .: "conclusion"
        <*> o .: "number"
        <*> o .: "started_at"
        <*> o .: "completed_at"

instance FromJSON Job where
    parseJSON = withObject "Job" $ \o -> Job
        <$> o .: "id"
        <*> o .: "run_id"
        <*> o .: "run_url"
        <*> o .: "run_attempt"
        <*> o .: "node_id"
        <*> o .: "head_sha"
        <*> o .: "url"
        <*> o .: "html_url"
        <*> o .: "status"
        <*> o .: "conclusion"
        <*> o .: "started_at"
        <*> o .: "completed_at"
        <*> o .: "steps"
        <*> o .: "check_run_url"
        <*> o .: "labels"
        <*> o .: "runner_id"
        <*> o .: "runner_name"
        <*> o .: "runner_group_id"
        <*> o .: "runner_group_name"

instance FromJSON (WithTotalCount Job) where
    parseJSON = withObject "JobList" $ \o -> WithTotalCount
        <$> o .: "jobs"
        <*> o .: "total_count"
