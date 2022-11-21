{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
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
    optionsCreator,
    optionsMentioned,
    optionsIrrelevantMilestone,
    optionsAnyMilestone,
    optionsNoMilestone,
    optionsMilestone,
    optionsIrrelevantAssignee,
    optionsAnyAssignee,
    optionsNoAssignee,
    optionsAssignee,
    -- * Actions artifacts
    ArtifactMod,
    artifactModToQueryString,
    optionsArtifactName,
    -- * Actions cache
    CacheMod,
    cacheModToQueryString,
    optionsRef,
    optionsNoRef,
    optionsKey,
    optionsNoKey,
    optionsDirectionAsc,
    optionsDirectionDesc,
    sortByCreatedAt,
    sortByLastAccessedAt,
    sortBySizeInBytes,
    -- * Actions workflow runs
    WorkflowRunMod,
    workflowRunModToQueryString,
    optionsWorkflowRunActor,
    optionsWorkflowRunBranch,
    optionsWorkflowRunEvent,
    optionsWorkflowRunStatus,
    optionsWorkflowRunCreated,
    optionsWorkflowRunHeadSha,
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
    parseJSON = withText "IssueState" $ \t -> case T.toLower t of
        "open"   -> pure StateOpen
        "closed" -> pure StateClosed
        _        -> fail $ "Unknown IssueState: " <> T.unpack t

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
    | StateDraft
  deriving
    (Eq, Ord, Show, Enum, Bounded, Generic, Typeable, Data)

instance ToJSON MergeableState where
    toJSON StateUnknown  = String "unknown"
    toJSON StateClean    = String "clean"
    toJSON StateDirty    = String "dirty"
    toJSON StateUnstable = String "unstable"
    toJSON StateBlocked  = String "blocked"
    toJSON StateBehind   = String "behind"
    toJSON StateDraft    = String "draft"

instance FromJSON MergeableState where
    parseJSON = withText "MergeableState" $ \t -> case T.toLower t of
        "unknown"  -> pure StateUnknown
        "clean"    -> pure StateClean
        "dirty"    -> pure StateDirty
        "unstable" -> pure StateUnstable
        "blocked"  -> pure StateBlocked
        "behind"   -> pure StateBehind
        "draft"    -> pure StateDraft
        _          -> fail $ "Unknown MergeableState: " <> T.unpack t

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

-- Actions cache

data SortCache
    = SortCacheCreatedAt
    | SortCacheLastAccessedAt
    | SortCacheSizeInBytes
  deriving
    (Eq, Ord, Show, Enum, Bounded, Generic, Typeable, Data)

instance NFData SortCache where rnf = genericRnf
instance Binary SortCache

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

-- | See <https://docs.github.com/en/rest/reference/issues#list-issues-assigned-to-the-authenticated-user--parameters>.
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

-- | See <https://docs.github.com/en/rest/reference/issues#list-issues-assigned-to-the-authenticated-user--parameters>.
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

-- | Parameters of "list repository issues" (@get /repos/{owner}/{repo}/issues@).
--
-- See <https://docs.github.com/en/rest/reference/issues#list-repository-issues>.
--
data IssueRepoOptions = IssueRepoOptions
    { issueRepoOptionsMilestone :: !(FilterBy (Id Milestone))   -- ^ 'optionsMilestone' etc.
    , issueRepoOptionsState     :: !(Maybe IssueState)          -- ^ 'HasState'
    , issueRepoOptionsAssignee  :: !(FilterBy (Name User))      -- ^ 'optionsAssignee' etc.
    , issueRepoOptionsCreator   :: !(Maybe (Name User))         -- ^ 'optionsCreator'
    , issueRepoOptionsMentioned :: !(Maybe (Name User))         -- ^ 'optionsMentioned'
    , issueRepoOptionsLabels    :: ![Name IssueLabel]           -- ^ 'HasLabels'
    , issueRepoOptionsSort      :: !SortIssue                   -- ^ 'HasCreatedUpdated' and 'HasComments'
    , issueRepoOptionsDirection :: !SortDirection               -- ^ 'HasDirection'
    , issueRepoOptionsSince     :: !(Maybe UTCTime)             -- ^ 'HasSince'
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

-- | Issues created by a certain user.
optionsCreator :: Name User -> IssueRepoMod
optionsCreator u = IssueRepoMod $ \opts ->
    opts { issueRepoOptionsCreator = Just u }

-- | Issue mentioning the given user.
optionsMentioned :: Name User -> IssueRepoMod
optionsMentioned u = IssueRepoMod $ \opts ->
    opts { issueRepoOptionsMentioned = Just u }

-- | Don't care about milestones (default).
--
-- 'optionsAnyMilestone' means there should be some milestone, but it can be any.
--
-- See <https://developer.github.com/v3/issues/#list-issues-for-a-repository>
optionsIrrelevantMilestone :: IssueRepoMod
optionsIrrelevantMilestone = IssueRepoMod $ \opts ->
    opts { issueRepoOptionsMilestone = FilterNotSpecified }

-- | Issues that have a milestone.
optionsAnyMilestone :: IssueRepoMod
optionsAnyMilestone = IssueRepoMod $ \opts ->
    opts { issueRepoOptionsMilestone = FilterAny }

-- | Issues that have no milestone.
optionsNoMilestone :: IssueRepoMod
optionsNoMilestone = IssueRepoMod $ \opts ->
    opts { issueRepoOptionsMilestone = FilterNone }

-- | Issues with the given milestone.
optionsMilestone :: Id Milestone -> IssueRepoMod
optionsMilestone m = IssueRepoMod $ \opts ->
    opts { issueRepoOptionsMilestone = FilterBy m }

-- | Issues with or without assignee (default).
optionsIrrelevantAssignee :: IssueRepoMod
optionsIrrelevantAssignee = IssueRepoMod $ \opts ->
    opts { issueRepoOptionsAssignee = FilterNotSpecified }

-- | Issues assigned to someone.
optionsAnyAssignee :: IssueRepoMod
optionsAnyAssignee = IssueRepoMod $ \opts ->
    opts { issueRepoOptionsAssignee = FilterAny }

-- | Issues assigned to nobody.
optionsNoAssignee :: IssueRepoMod
optionsNoAssignee = IssueRepoMod $ \opts ->
    opts { issueRepoOptionsAssignee = FilterNone }

-- | Issues assigned to a specific user.
optionsAssignee :: Name User -> IssueRepoMod
optionsAssignee u = IssueRepoMod $ \opts ->
    opts { issueRepoOptionsAssignee = FilterBy u }
    
-------------------------------------------------------------------------------
-- Actions artifacts
-------------------------------------------------------------------------------
-- | See <https://docs.github.com/en/rest/actions/artifacts#list-artifacts-for-a-repository>.
data ArtifactOptions = ArtifactOptions
    { artifactOptionsName     :: !(Maybe Text)
    }
  deriving
    (Eq, Ord, Show, Generic, Typeable, Data)

defaultArtifactOptions :: ArtifactOptions
defaultArtifactOptions = ArtifactOptions
    { artifactOptionsName = Nothing
    }

-- | See <https://docs.github.com/en/rest/actions/artifacts#list-artifacts-for-a-repository>.
newtype ArtifactMod = ArtifactMod (ArtifactOptions -> ArtifactOptions)

instance Semigroup ArtifactMod where
    ArtifactMod f <> ArtifactMod g = ArtifactMod (g . f)

instance Monoid ArtifactMod where
    mempty  = ArtifactMod id
    mappend = (<>)

-- | Filters artifacts by exact match on their name field.
optionsArtifactName :: Text -> ArtifactMod
optionsArtifactName n = ArtifactMod $ \opts ->
    opts { artifactOptionsName = Just n }

toArtifactOptions :: ArtifactMod -> ArtifactOptions
toArtifactOptions (ArtifactMod f) = f defaultArtifactOptions

artifactModToQueryString :: ArtifactMod -> QueryString
artifactModToQueryString = artifactOptionsToQueryString . toArtifactOptions

artifactOptionsToQueryString :: ArtifactOptions -> QueryString
artifactOptionsToQueryString (ArtifactOptions name) =
    catMaybes
    [ mk "name" <$> name'
    ]
  where
    mk k v = (k, Just v)
    name' = fmap TE.encodeUtf8 name
-------------------------------------------------------------------------------
-- Actions cache
-------------------------------------------------------------------------------

-- | See <https://docs.github.com/en/rest/actions/cache#list-github-actions-caches-for-a-repository>.
data CacheOptions = CacheOptions
    { cacheOptionsRef     :: !(Maybe Text)
    , cacheOptionsKey      :: !(Maybe Text)
    , cacheOptionsSort      :: !(Maybe SortCache)
    , cacheOptionsDirection :: !(Maybe SortDirection)
    }
  deriving
    (Eq, Ord, Show, Generic, Typeable, Data)

defaultCacheOptions :: CacheOptions
defaultCacheOptions = CacheOptions
    { cacheOptionsRef = Nothing
    , cacheOptionsKey  = Nothing
    , cacheOptionsSort  = Nothing
    , cacheOptionsDirection = Nothing
    }

-- | See <https://docs.github.com/en/rest/actions/cache#list-github-actions-caches-for-a-repository>.
newtype CacheMod = CacheMod (CacheOptions -> CacheOptions)

instance Semigroup CacheMod where
    CacheMod f <> CacheMod g = CacheMod (g . f)

instance Monoid CacheMod where
    mempty  = CacheMod id
    mappend = (<>)

toCacheOptions :: CacheMod -> CacheOptions
toCacheOptions (CacheMod f) = f defaultCacheOptions

cacheModToQueryString :: CacheMod -> QueryString
cacheModToQueryString = cacheOptionsToQueryString . toCacheOptions

cacheOptionsToQueryString :: CacheOptions -> QueryString
cacheOptionsToQueryString (CacheOptions ref key sort dir) =
    catMaybes
    [ mk "ref" <$> ref'
    , mk "key" <$> key'
    , mk "sort" <$> sort'
    , mk "directions" <$> direction'
    ]
  where
    mk k v = (k, Just v)
    sort' = fmap (\case
        SortCacheCreatedAt     -> "created_at"
        SortCacheLastAccessedAt     -> "last_accessed_at"
        SortCacheSizeInBytes  -> "size_in_bytes") sort
    direction' = fmap (\case
       SortDescending -> "desc"
       SortAscending  -> "asc") dir
    ref' = fmap TE.encodeUtf8 ref
    key' = fmap TE.encodeUtf8 key

-------------------------------------------------------------------------------
-- Cache modifiers
-------------------------------------------------------------------------------

optionsRef :: Text -> CacheMod
optionsRef x = CacheMod $ \opts ->
    opts { cacheOptionsRef = Just x }

optionsNoRef :: CacheMod
optionsNoRef = CacheMod $ \opts ->
    opts { cacheOptionsRef = Nothing }

optionsKey :: Text -> CacheMod
optionsKey x = CacheMod $ \opts ->
    opts { cacheOptionsKey = Just x }

optionsNoKey :: CacheMod
optionsNoKey = CacheMod $ \opts ->
    opts { cacheOptionsKey = Nothing }

optionsDirectionAsc :: CacheMod
optionsDirectionAsc = CacheMod $ \opts ->
    opts { cacheOptionsDirection = Just SortAscending }

optionsDirectionDesc :: CacheMod
optionsDirectionDesc = CacheMod $ \opts ->
    opts { cacheOptionsDirection = Just SortDescending }

sortByCreatedAt :: CacheMod
sortByCreatedAt = CacheMod $ \opts ->
    opts { cacheOptionsSort = Just SortCacheCreatedAt }

sortByLastAccessedAt :: CacheMod
sortByLastAccessedAt = CacheMod $ \opts ->
    opts { cacheOptionsSort = Just SortCacheLastAccessedAt }

sortBySizeInBytes :: CacheMod
sortBySizeInBytes = CacheMod $ \opts ->
    opts { cacheOptionsSort = Just SortCacheSizeInBytes }

-------------------------------------------------------------------------------
-- Actions workflow runs
-------------------------------------------------------------------------------

-- | See <https://docs.github.com/en/rest/actions/workflow-runs#list-workflow-runs-for-a-repository>.
data WorkflowRunOptions = WorkflowRunOptions
    { workflowRunOptionsActor     :: !(Maybe Text)
    , workflowRunOptionsBranch      :: !(Maybe Text)
    , workflowRunOptionsEvent      :: !(Maybe Text)
    , workflowRunOptionsStatus :: !(Maybe Text)
    , workflowRunOptionsCreated :: !(Maybe Text)
    , workflowRunOptionsHeadSha :: !(Maybe Text)
    }
  deriving
    (Eq, Ord, Show, Generic, Typeable, Data)

defaultWorkflowRunOptions :: WorkflowRunOptions
defaultWorkflowRunOptions = WorkflowRunOptions
    { workflowRunOptionsActor     = Nothing
    , workflowRunOptionsBranch      = Nothing
    , workflowRunOptionsEvent     = Nothing
    , workflowRunOptionsStatus = Nothing
    , workflowRunOptionsCreated = Nothing
    , workflowRunOptionsHeadSha = Nothing
    }

-- | See <https://docs.github.com/en/rest/actions/workflow-runs#list-workflow-runs-for-a-repository>.
newtype WorkflowRunMod = WorkflowRunMod (WorkflowRunOptions -> WorkflowRunOptions)

instance Semigroup WorkflowRunMod where
    WorkflowRunMod f <> WorkflowRunMod g = WorkflowRunMod (g . f)

instance Monoid WorkflowRunMod where
    mempty  = WorkflowRunMod id
    mappend = (<>)

toWorkflowRunOptions :: WorkflowRunMod -> WorkflowRunOptions
toWorkflowRunOptions (WorkflowRunMod f) = f defaultWorkflowRunOptions

workflowRunModToQueryString :: WorkflowRunMod -> QueryString
workflowRunModToQueryString = workflowRunOptionsToQueryString . toWorkflowRunOptions

workflowRunOptionsToQueryString :: WorkflowRunOptions -> QueryString
workflowRunOptionsToQueryString (WorkflowRunOptions actor branch event status created headSha) =
    catMaybes
    [ mk "actor" <$> actor'
    , mk "branch" <$> branch'
    , mk "event" <$> event'
    , mk "status" <$> status'
    , mk "created" <$> created'
    , mk "head_sha" <$> headSha'
    ]
  where
    mk k v = (k, Just v)
    actor' = fmap TE.encodeUtf8 actor
    branch' = fmap TE.encodeUtf8 branch
    event' = fmap TE.encodeUtf8 event
    status' = fmap TE.encodeUtf8 status
    created' = fmap TE.encodeUtf8 created
    headSha' = fmap TE.encodeUtf8 headSha

-------------------------------------------------------------------------------
-- Workflow run modifiers
-------------------------------------------------------------------------------

optionsWorkflowRunActor :: Text -> WorkflowRunMod
optionsWorkflowRunActor x = WorkflowRunMod $ \opts ->
    opts { workflowRunOptionsActor = Just x }

optionsWorkflowRunBranch :: Text -> WorkflowRunMod
optionsWorkflowRunBranch x = WorkflowRunMod $ \opts ->
    opts { workflowRunOptionsBranch = Just x }

optionsWorkflowRunEvent :: Text -> WorkflowRunMod
optionsWorkflowRunEvent x = WorkflowRunMod $ \opts ->
    opts { workflowRunOptionsEvent = Just x }

optionsWorkflowRunStatus :: Text -> WorkflowRunMod
optionsWorkflowRunStatus x = WorkflowRunMod $ \opts ->
    opts { workflowRunOptionsStatus = Just x }

optionsWorkflowRunCreated :: Text -> WorkflowRunMod
optionsWorkflowRunCreated x = WorkflowRunMod $ \opts ->
    opts { workflowRunOptionsCreated = Just x }

optionsWorkflowRunHeadSha :: Text -> WorkflowRunMod
optionsWorkflowRunHeadSha x = WorkflowRunMod $ \opts ->
    opts { workflowRunOptionsHeadSha = Just x }