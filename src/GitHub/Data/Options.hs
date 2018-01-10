{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Module with modifiers for pull requests' and issues' listings.
module GitHub.Data.Options (
    -- * Common modifiers
    stateOpen,
    stateClosed,
    stateAll,
    sortAscending,
    sortDescending,
    sortByCreated,
    sortByUpdated,
    -- * Pull Requests
    PullRequestMod,
    prModToQueryString,
    optionsBase,
    optionsNoBase,
    optionsHead,
    optionsNoHead,
    sortByPopularity,
    sortByLongRunning,
    -- * Issues
    IssueMod,
    issueModToQueryString,
    sortByComments,
    optionsLabels,
    optionsSince,
    optionsSinceAll,
    optionsAssignedIssues,
    optionsCreatedIssues,
    optionsMentionedIssues,
    optionsSubscribedIssues,
    optionsAllIssues,
    -- * Repo issues
    IssueRepoMod,
    issueRepoModToQueryString,
    optionsIrrelevantMilestone,
    optionsAnyMilestone,
    optionsNoMilestone,
    optionsIrrelevantAssignee,
    optionsAnyAssignee,
    optionsNoAssignee,
    -- * Data
    IssueState (..),
    MergeableState (..),
    -- * Internal
    HasState,
    HasDirection,
    HasCreatedUpdated,
    HasComments,
    HasLabels,
    HasSince,
    ) where

import GitHub.Data.Definitions
import GitHub.Data.Id          (Id, untagId)
import GitHub.Data.Milestone   (Milestone)
import GitHub.Data.Name        (Name, untagName)
import GitHub.Internal.Prelude
import Prelude ()

import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

-- | 'GitHub.Data.Issues.Issue' or 'GitHub.Data.PullRequests.PullRequest' state
data IssueState
    = StateOpen
    | StateClosed
  deriving
    (Eq, Ord, Show, Enum, Bounded, Generic, Typeable, Data)

instance ToJSON IssueState where
    toJSON StateOpen    = String "open"
    toJSON StateClosed  = String "closed"

instance FromJSON IssueState where
    parseJSON (String "open")   = pure StateOpen
    parseJSON (String "closed") = pure StateClosed
    parseJSON v                 = typeMismatch "IssueState" v

instance NFData IssueState where rnf = genericRnf
instance Binary IssueState

-- | 'GitHub.Data.PullRequests.PullRequest' mergeable_state
data MergeableState
    = StateUnknown
    | StateClean
    | StateDirty
    | StateUnstable
    | StateBlocked
    | StateBehind
  deriving
    (Eq, Ord, Show, Enum, Bounded, Generic, Typeable, Data)

instance ToJSON MergeableState where
    toJSON StateUnknown  = String "unknown"
    toJSON StateClean    = String "clean"
    toJSON StateDirty    = String "dirty"
    toJSON StateUnstable = String "unstable"
    toJSON StateBlocked  = String "blocked"
    toJSON StateBehind   = String "behind"

instance FromJSON MergeableState where
    parseJSON (String "unknown")  = pure StateUnknown
    parseJSON (String "clean")    = pure StateClean
    parseJSON (String "dirty")    = pure StateDirty
    parseJSON (String "unstable") = pure StateUnstable
    parseJSON (String "blocked")  = pure StateBlocked
    parseJSON (String "behind")   = pure StateBehind
    parseJSON v                   = typeMismatch "MergeableState" v

instance NFData MergeableState where rnf = genericRnf
instance Binary MergeableState

data SortDirection
    = SortAscending
    | SortDescending
  deriving
    (Eq, Ord, Show, Enum, Bounded, Generic, Typeable, Data)

instance NFData SortDirection where rnf = genericRnf
instance Binary SortDirection

-- PR

data SortPR
    = SortPRCreated
    | SortPRUpdated
    | SortPRPopularity
    | SortPRLongRunning
  deriving
    (Eq, Ord, Show, Enum, Bounded, Generic, Typeable, Data)

instance NFData SortPR where rnf = genericRnf
instance Binary SortPR

-- Issue
data IssueFilter
    = IssueFilterAssigned
    | IssueFilterCreated
    | IssueFilterMentioned
    | IssueFilterSubscribed
    | IssueFilterAll
  deriving
    (Eq, Ord, Show, Enum, Bounded, Generic, Typeable, Data)

instance NFData IssueFilter where rnf = genericRnf
instance Binary IssueFilter

data SortIssue
    = SortIssueCreated
    | SortIssueUpdated
    | SortIssueComments
  deriving
    (Eq, Ord, Show, Enum, Bounded, Generic, Typeable, Data)

instance NFData SortIssue where rnf = genericRnf
instance Binary SortIssue

data FilterBy a
    = FilterAny
    | FilterNone
    | FilterBy a
    | FilterNotSpecified
      -- ^ e.g. for milestones "any" means "any milestone".
      -- I.e. won't show issues without mileston specified
  deriving
    (Eq, Ord, Show, Generic, Typeable, Data)

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

class HasState mod where
    state :: Maybe IssueState -> mod

stateOpen :: HasState mod => mod
stateOpen = state (Just StateOpen)

stateClosed :: HasState mod => mod
stateClosed = state (Just StateClosed)

stateAll :: HasState mod => mod
stateAll = state Nothing

instance HasState PullRequestMod where
    state s = PRMod $ \opts ->
        opts { pullRequestOptionsState = s }

instance HasState IssueMod where
    state s = IssueMod $ \opts ->
        opts { issueOptionsState = s }

instance HasState IssueRepoMod where
    state s = IssueRepoMod $ \opts ->
        opts { issueRepoOptionsState = s }


class HasDirection mod where
    sortDir :: SortDirection -> mod

sortAscending :: HasDirection mod => mod
sortAscending = sortDir SortAscending

sortDescending :: HasDirection mod => mod
sortDescending = sortDir SortDescending

instance HasDirection PullRequestMod where
    sortDir x = PRMod $ \opts ->
        opts { pullRequestOptionsDirection = x }

instance HasDirection IssueMod where
    sortDir x = IssueMod $ \opts ->
        opts { issueOptionsDirection = x }

instance HasDirection IssueRepoMod where
    sortDir x = IssueRepoMod $ \opts ->
        opts { issueRepoOptionsDirection = x }


class HasCreatedUpdated mod where
    sortByCreated :: mod
    sortByUpdated :: mod

instance HasCreatedUpdated PullRequestMod where
    sortByCreated = PRMod $ \opts ->
        opts { pullRequestOptionsSort = SortPRCreated }
    sortByUpdated = PRMod $ \opts ->
        opts { pullRequestOptionsSort = SortPRUpdated }

instance HasCreatedUpdated IssueMod where
    sortByCreated = IssueMod $ \opts ->
        opts { issueOptionsSort = SortIssueCreated }
    sortByUpdated = IssueMod $ \opts ->
        opts { issueOptionsSort = SortIssueUpdated }

instance HasCreatedUpdated IssueRepoMod where
    sortByCreated = IssueRepoMod $ \opts ->
        opts { issueRepoOptionsSort = SortIssueCreated }
    sortByUpdated = IssueRepoMod $ \opts ->
        opts { issueRepoOptionsSort = SortIssueUpdated }

-------------------------------------------------------------------------------
-- Pull Request
-------------------------------------------------------------------------------

-- | See <https://developer.github.com/v3/pulls/#parameters>.
data PullRequestOptions = PullRequestOptions
    { pullRequestOptionsState     :: !(Maybe IssueState)
    , pullRequestOptionsHead      :: !(Maybe Text)
    , pullRequestOptionsBase      :: !(Maybe Text)
    , pullRequestOptionsSort      :: !SortPR
    , pullRequestOptionsDirection :: !SortDirection
    }
  deriving
    (Eq, Ord, Show, Generic, Typeable, Data)

defaultPullRequestOptions :: PullRequestOptions
defaultPullRequestOptions = PullRequestOptions
    { pullRequestOptionsState     = Just StateOpen
    , pullRequestOptionsHead      = Nothing
    , pullRequestOptionsBase      = Nothing
    , pullRequestOptionsSort      = SortPRCreated
    , pullRequestOptionsDirection = SortDescending
    }

-- | See <https://developer.github.com/v3/pulls/#parameters>.
newtype PullRequestMod = PRMod (PullRequestOptions -> PullRequestOptions)

instance Semigroup PullRequestMod where
    PRMod f <> PRMod g = PRMod (g . f)

instance Monoid PullRequestMod where
    mempty  = PRMod id
    mappend = (<>)

toPullRequestOptions :: PullRequestMod -> PullRequestOptions
toPullRequestOptions (PRMod f) = f defaultPullRequestOptions

prModToQueryString :: PullRequestMod -> QueryString
prModToQueryString = pullRequestOptionsToQueryString . toPullRequestOptions

pullRequestOptionsToQueryString :: PullRequestOptions -> QueryString
pullRequestOptionsToQueryString (PullRequestOptions st head_ base sort dir) =
    [ mk "state"     state'
    , mk "sort"      sort'
    , mk "direction" direction'
    ] ++ catMaybes
    [ mk "head" <$> head'
    , mk "base" <$> base'
    ]
  where
    mk k v = (k, Just v)
    state' = case st of
        Nothing          -> "all"
        Just StateOpen   -> "open"
        Just StateClosed -> "closed"
    sort' = case sort of
        SortPRCreated     -> "created"
        SortPRUpdated     -> "updated"
        SortPRPopularity  -> "popularity"
        SortPRLongRunning -> "long-running"
    direction' = case dir of
       SortDescending -> "desc"
       SortAscending  -> "asc"
    head' = fmap TE.encodeUtf8 head_
    base' = fmap TE.encodeUtf8 base

-------------------------------------------------------------------------------
-- Pull request modifiers
-------------------------------------------------------------------------------

optionsBase :: Text -> PullRequestMod
optionsBase x = PRMod $ \opts ->
    opts { pullRequestOptionsBase = Just x }

optionsNoBase :: PullRequestMod
optionsNoBase = PRMod $ \opts ->
    opts { pullRequestOptionsBase = Nothing }

optionsHead :: Text -> PullRequestMod
optionsHead x = PRMod $ \opts ->
    opts { pullRequestOptionsHead = Just x }

optionsNoHead :: PullRequestMod
optionsNoHead = PRMod $ \opts ->
    opts { pullRequestOptionsHead = Nothing }

sortByPopularity :: PullRequestMod
sortByPopularity = PRMod $ \opts ->
    opts { pullRequestOptionsSort = SortPRPopularity }

sortByLongRunning :: PullRequestMod
sortByLongRunning = PRMod $ \opts ->
    opts { pullRequestOptionsSort = SortPRLongRunning }

-------------------------------------------------------------------------------
-- Issues
-------------------------------------------------------------------------------

-- | See <https://developer.github.com/v3/issues/#parameters>.
data IssueOptions = IssueOptions
    { issueOptionsFilter    :: !IssueFilter
    , issueOptionsState     :: !(Maybe IssueState)
    , issueOptionsLabels    :: ![Name IssueLabel] -- TODO: change to newtype
    , issueOptionsSort      :: !SortIssue
    , issueOptionsDirection :: !SortDirection
    , issueOptionsSince     :: !(Maybe UTCTime)
    }
  deriving
    (Eq, Ord, Show, Generic, Typeable, Data)

defaultIssueOptions :: IssueOptions
defaultIssueOptions = IssueOptions
    { issueOptionsFilter    = IssueFilterAssigned
    , issueOptionsState     = Just StateOpen
    , issueOptionsLabels    = []
    , issueOptionsSort      = SortIssueCreated
    , issueOptionsDirection = SortDescending
    , issueOptionsSince     = Nothing
    }

-- | See <https://developer.github.com/v3/issues/#parameters>.
newtype IssueMod = IssueMod (IssueOptions -> IssueOptions)

instance Semigroup IssueMod where
    IssueMod f <> IssueMod g = IssueMod (g . f)

instance Monoid IssueMod where
    mempty  = IssueMod id
    mappend = (<>)

toIssueOptions :: IssueMod -> IssueOptions
toIssueOptions (IssueMod f) = f defaultIssueOptions

issueModToQueryString :: IssueMod -> QueryString
issueModToQueryString = issueOptionsToQueryString . toIssueOptions

issueOptionsToQueryString :: IssueOptions -> QueryString
issueOptionsToQueryString (IssueOptions filt st labels sort dir since) =
    [ mk "state"     state'
    , mk "sort"      sort'
    , mk "direction" direction'
    , mk "filter" filt'
    ] ++ catMaybes
    [ mk "labels" <$> labels'
    , mk "since" <$> since'
    ]
  where
    mk k v = (k, Just v)
    filt' = case filt of
        IssueFilterAssigned   -> "assigned"
        IssueFilterCreated    -> "created"
        IssueFilterMentioned  -> "mentioned"
        IssueFilterSubscribed -> "subscribed"
        IssueFilterAll        -> "all"
    state' = case st of
        Nothing          -> "all"
        Just StateOpen   -> "open"
        Just StateClosed -> "closed"
    sort' = case sort of
        SortIssueCreated  -> "created"
        SortIssueUpdated  -> "updated"
        SortIssueComments -> "comments"
    direction' = case dir of
       SortDescending -> "desc"
       SortAscending  -> "asc"

    since' = fmap (TE.encodeUtf8 . T.pack . show) since
    labels' = TE.encodeUtf8 . T.intercalate "," . fmap untagName <$> nullToNothing labels

nullToNothing :: Foldable f => f a -> Maybe (f a)
nullToNothing xs
    | null xs   = Nothing
    | otherwise = Just xs

-------------------------------------------------------------------------------
-- Issues modifiers
-------------------------------------------------------------------------------

class HasComments mod where
    sortByComments :: mod

instance HasComments IssueMod where
    sortByComments = IssueMod $ \opts ->
        opts { issueOptionsSort = SortIssueComments }

instance HasComments IssueRepoMod where
    sortByComments = IssueRepoMod $ \opts ->
        opts { issueRepoOptionsSort = SortIssueComments }


class HasLabels mod where
    optionsLabels :: Foldable f => f (Name IssueLabel) -> mod

instance HasLabels IssueMod where
    optionsLabels lbls = IssueMod $ \opts ->
        opts { issueOptionsLabels = toList lbls }

instance HasLabels IssueRepoMod where
    optionsLabels lbls = IssueRepoMod $ \opts ->
        opts { issueRepoOptionsLabels = toList lbls }


class HasSince mod where
    optionsSince :: UTCTime -> mod
    optionsSinceAll :: mod

instance HasSince IssueMod where
    optionsSince since = IssueMod $ \opts ->
        opts { issueOptionsSince = Just since }
    optionsSinceAll = IssueMod $ \opts ->
        opts { issueOptionsSince = Nothing }

instance HasSince IssueRepoMod where
    optionsSince since = IssueRepoMod $ \opts ->
        opts { issueRepoOptionsSince = Just since }
    optionsSinceAll = IssueRepoMod $ \opts ->
        opts { issueRepoOptionsSince = Nothing }

-------------------------------------------------------------------------------
-- Only issues modifiers
-------------------------------------------------------------------------------

optionsAssignedIssues, optionsCreatedIssues, optionsMentionedIssues,
  optionsSubscribedIssues, optionsAllIssues  :: IssueMod
optionsAssignedIssues   = issueFilter IssueFilterAssigned
optionsCreatedIssues    = issueFilter IssueFilterCreated
optionsMentionedIssues  = issueFilter IssueFilterMentioned
optionsSubscribedIssues = issueFilter IssueFilterSubscribed
optionsAllIssues        = issueFilter IssueFilterAll

issueFilter :: IssueFilter -> IssueMod
issueFilter f = IssueMod $ \opts ->
    opts { issueOptionsFilter = f }

-------------------------------------------------------------------------------
-- Issues repo
-------------------------------------------------------------------------------

data IssueRepoOptions = IssueRepoOptions
    { issueRepoOptionsMilestone :: !(FilterBy (Id Milestone))
    , issueRepoOptionsState     :: !(Maybe IssueState)
    , issueRepoOptionsAssignee  :: !(FilterBy (Name User))
    , issueRepoOptionsCreator   :: !(Maybe (Name User))
    , issueRepoOptionsMentioned :: !(Maybe (Name User))
    , issueRepoOptionsLabels    :: ![Name IssueLabel]
    , issueRepoOptionsSort      :: !SortIssue
    , issueRepoOptionsDirection :: !SortDirection
    , issueRepoOptionsSince     :: !(Maybe UTCTime)
    }
  deriving
    (Eq, Ord, Show, Generic, Typeable, Data)

defaultIssueRepoOptions :: IssueRepoOptions
defaultIssueRepoOptions = IssueRepoOptions
    { issueRepoOptionsMilestone = FilterNotSpecified
    , issueRepoOptionsState     = (Just StateOpen)
    , issueRepoOptionsAssignee  = FilterNotSpecified
    , issueRepoOptionsCreator   = Nothing
    , issueRepoOptionsMentioned = Nothing
    , issueRepoOptionsLabels    = []
    , issueRepoOptionsSort      = SortIssueCreated
    , issueRepoOptionsDirection = SortDescending
    , issueRepoOptionsSince     = Nothing
    }

-- | See <https://developer.github.com/v3/issues/#parameters-1>.
newtype IssueRepoMod = IssueRepoMod (IssueRepoOptions -> IssueRepoOptions)

instance Semigroup IssueRepoMod where
    IssueRepoMod f <> IssueRepoMod g = IssueRepoMod (g . f)

instance Monoid IssueRepoMod where
    mempty  = IssueRepoMod id
    mappend = (<>)

toIssueRepoOptions :: IssueRepoMod -> IssueRepoOptions
toIssueRepoOptions (IssueRepoMod f) = f defaultIssueRepoOptions

issueRepoModToQueryString :: IssueRepoMod -> QueryString
issueRepoModToQueryString = issueRepoOptionsToQueryString . toIssueRepoOptions

issueRepoOptionsToQueryString :: IssueRepoOptions -> QueryString
issueRepoOptionsToQueryString IssueRepoOptions {..} =
    [ mk "state"     state'
    , mk "sort"      sort'
    , mk "direction" direction'
    ] ++ catMaybes
    [ mk "milestone" <$> milestone'
    , mk "assignee"  <$> assignee'
    , mk "labels"    <$> labels'
    , mk "since"     <$> since'
    , mk "creator"   <$> creator'
    , mk "mentioned" <$> mentioned'
    ]
  where
    mk k v = (k, Just v)
    filt f x = case x of
        FilterAny          -> Just "*"
        FilterNone         -> Just "none"
        FilterBy x'        -> Just $ TE.encodeUtf8 $ f x'
        FilterNotSpecified -> Nothing

    milestone' = filt (T.pack . show . untagId) issueRepoOptionsMilestone
    assignee'  = filt untagName issueRepoOptionsAssignee

    state' = case issueRepoOptionsState of
        Nothing          -> "all"
        Just StateOpen   -> "open"
        Just StateClosed -> "closed"
    sort' = case issueRepoOptionsSort of
        SortIssueCreated  -> "created"
        SortIssueUpdated  -> "updated"
        SortIssueComments -> "comments"
    direction' = case issueRepoOptionsDirection of
       SortDescending -> "desc"
       SortAscending  -> "asc"

    since'     = TE.encodeUtf8 . T.pack . show <$> issueRepoOptionsSince
    labels'    = TE.encodeUtf8 . T.intercalate "," . fmap untagName <$> nullToNothing issueRepoOptionsLabels
    creator'   = TE.encodeUtf8 . untagName <$> issueRepoOptionsCreator
    mentioned' = TE.encodeUtf8 . untagName <$> issueRepoOptionsMentioned

-------------------------------------------------------------------------------
-- Issues repo modifiers
-------------------------------------------------------------------------------

-- | Don't care about milestones.
--
-- 'optionsAnyMilestone' means there should be some milestone, but it can be any.
--
-- See <https://developer.github.com/v3/issues/#list-issues-for-a-repository>
optionsIrrelevantMilestone :: IssueRepoMod
optionsIrrelevantMilestone = IssueRepoMod $ \opts ->
    opts { issueRepoOptionsMilestone = FilterNotSpecified }

optionsAnyMilestone :: IssueRepoMod
optionsAnyMilestone = IssueRepoMod $ \opts ->
    opts { issueRepoOptionsMilestone = FilterAny }

optionsNoMilestone :: IssueRepoMod
optionsNoMilestone = IssueRepoMod $ \opts ->
    opts { issueRepoOptionsMilestone = FilterNone }

optionsIrrelevantAssignee :: IssueRepoMod
optionsIrrelevantAssignee = IssueRepoMod $ \opts ->
    opts { issueRepoOptionsAssignee = FilterNotSpecified }

optionsAnyAssignee :: IssueRepoMod
optionsAnyAssignee = IssueRepoMod $ \opts ->
    opts { issueRepoOptionsAssignee = FilterAny }

optionsNoAssignee :: IssueRepoMod
optionsNoAssignee = IssueRepoMod $ \opts ->
    opts { issueRepoOptionsAssignee = FilterNone }
