{- | This module contains functions to work with issues withing GitHub API.
-}

module Hit.Issue
       ( -- * For CLI commands
         runIssue
       , createIssue
       , assignIssue
       , fetchIssue

         -- * Internal helpers
       , mkIssueId
       , getIssueTitle
       , getOwnerRepo
       , parseOwnerRepo
       , showIssueName
       ) where

import Colourista (blue, blueBg, bold, errorMessage, formatWith, green, red, reset, successMessage,
                   warningMessage)
import Data.Vector (Vector)
import GitHub (Error (..), Id, Issue (..), IssueLabel (..), IssueState (..), Name, Owner, Repo,
               SimpleUser (..), User, getUrl, milestoneNumber, mkId, mkName, unIssueNumber, untagId,
               untagName)
import GitHub.Auth (Auth (OAuth))
import GitHub.Data.Options (stateOpen)
import GitHub.Endpoints.Issues (EditIssue (..), NewIssue (..), editOfIssue, issue', issuesForRepo')
import GitHub.Endpoints.Issues.Milestones (milestones')
import Shellmet (($|))
import System.Environment (lookupEnv)

import Hit.Core (IssueOptions (..), Milestone (..))
import Hit.Git.Common (getUsername)
import Hit.Prompt (arrow)

import qualified Hit.Formatting as Fmt

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified GitHub.Endpoints.Issues as GitHub

----------------------------------------------------------------------------
-- CLI for issues
----------------------------------------------------------------------------

-- | Run the @issue@ command.
runIssue :: IssueOptions -> IO ()
runIssue IssueOptions{..} = case ioIssueNumber of
    Just num -> getIssue $ mkIssueId num
    Nothing  -> me >>= getAllIssues ioMilestone
  where
    me :: IO (Maybe Text)
    me = if ioMe
        then Just <$> getUsername
        else pure Nothing

{- | Get the list of the opened issues for the current project and
display short information about each issue.
-}
getAllIssues
    :: Maybe Milestone  -- ^ Project Milestone
    -> Maybe Text  -- ^ User name of the assignee
    -> IO ()
getAllIssues milestone me = withOwnerRepo (\t o r -> issuesForRepo' t o r stateOpen) >>= \case
    Left err -> errorMessage $ show err
    Right is -> do
        let maxLen = Fmt.maxLenOn showIssueNumber is
        milestoneId <- getMilestoneId
        for_ (filterIssues milestoneId is) $ \i -> do
            let thisLen = T.length $ showIssueNumber i
                padSize = maxLen - thisLen
            putTextLn $ showIssueName blue padSize i
  where
    filterIssues :: Maybe Int -> Vector Issue -> Vector Issue
    filterIssues milestoneId = V.filter
        (\i ->
            isNotPR i
            && my i
            && i `isInMilestone` milestoneId
        )

    my :: Issue -> Bool
    my issue = case me of
        Just (makeName -> username) -> username `isAssignedToIssue` issue
        Nothing                     -> True

    isNotPR :: Issue -> Bool
    isNotPR Issue{..} = isNothing issuePullRequest

    getMilestoneId :: IO (Maybe Int)
    getMilestoneId = case milestone of
        Just (MilestoneId mId) -> pure $ Just mId
        Just CurrentMilestone  -> fetchCurrentMilestoneId
        Nothing                -> pure Nothing

    isInMilestone :: Issue -> Maybe Int -> Bool
    isInMilestone Issue{..} = \case
        Just milestoneId -> issueMilestoneId == Just milestoneId
        Nothing  -> True
      where
        issueMilestoneId :: Maybe Int
        issueMilestoneId = untagId . milestoneNumber <$> issueMilestone

-- | Show issue number with alignment and its name.
showIssueName :: Text -> Int -> Issue -> Text
showIssueName colorCode padSize i@Issue{..} =
    arrow <> colorCode <> " [#" <> showIssueNumber i <> "] " <> padding <> reset <> issueTitle
  where
    padding :: Text
    padding = T.replicate padSize " "

-- | Show the issue number.
showIssueNumber :: Issue -> Text
showIssueNumber = show . unIssueNumber . issueNumber

-- | Get the 'Issue' by given issue number and pretty print it fully to terminal.
getIssue :: Id Issue -> IO ()
getIssue num = fetchIssue num >>= putTextLn . showIssueFull

-- | Show full information about the issue.
showIssueFull :: Issue -> Text
showIssueFull i@Issue{..} = T.intercalate "\n" $
       showIssueName (statusToCode issueState) 0 i
     : [ highlight "    Assignees: " <> assignees | not $ null issueAssignees]
    ++ [ highlight "    Labels: " <> labels | not $ null issueLabels]
    ++ [ highlight "    URL: " <> getUrl url | Just url <- [issueHtmlUrl]]
    ++ [ indentDesc desc | Just (T.strip -> desc) <- [issueBody], desc /= ""]
  where
    statusToCode :: IssueState -> Text
    statusToCode = \case
        StateOpen -> blue
        StateClosed -> red

    indentDesc :: Text -> Text
    indentDesc = unlines
        . map ("    " <> )
        . (highlight "Description:" :)
        . lines

    assignees :: Text
    assignees = T.intercalate ", "
        $ map (untagName . simpleUserLogin)
        $ toList issueAssignees

    labels :: Text
    labels = T.intercalate " "
        $ map (putLabel . untagName . labelName)
        $ toList issueLabels

    putLabel :: Text -> Text
    putLabel = formatWith [blueBg]

    highlight :: Text -> Text
    highlight = formatWith [bold, green]

-- | Create an 'Issue' by given title 'Text'
-- QUESTION: should we create 'Login' newtype to add more type-safety here?
createIssue :: Text -> Text -> IO (Either Error Issue)
createIssue title login = withAuthOwnerRepo $ \token owner repo ->
    GitHub.createIssue token owner repo $ mkNewIssue title login

{- | Assign the user to the given 'Issue'.

This function can fail assignment due to the following reasons:

 * Auth token fetch failure
 * Assignment query to GutHub failure

The function should inform user about corresponding 'Error' in each case and
continue working.
-}
assignIssue :: Issue -> Text -> IO ()
assignIssue issue username = do
    res <- withAuthOwnerRepo $ \token owner repo -> do
        let assignee :: Name User
            assignee = makeName @User username
        let curAssignees :: V.Vector (Name User)
            curAssignees = V.map simpleUserLogin $ issueAssignees issue

        if assignee `isAssignedToIssue` issue
        then pure $ Right (issue, True)
        else do
            -- TODO: this is hack to cheat on GitHub library, as it
            -- doesn't use the correct id in query.
            let issId = mkIssueId (unIssueNumber $ issueNumber issue)
            (, False) <<$>> GitHub.editIssue token owner repo issId editOfIssue
                { editIssueAssignees = Just $ V.cons assignee curAssignees
                }

    case res of
        Right (iss, isAlreadyAssigned) ->
            if isAlreadyAssigned
            then pass
            else successMessage $ "You were assigned to the issue #" <>
                showIssueNumber iss
        Left err  -> do
            errorMessage "Can not assign you to the issue."
            putTextLn $ "    " <> show err

-- | Is the user assigned to the given 'Issue'?
isAssignedToIssue :: Name User -> Issue -> Bool
isAssignedToIssue assignee = V.elem assignee .
    V.map simpleUserLogin . issueAssignees

----------------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------------

-- | Fetch only issue title.
getIssueTitle :: Id Issue -> IO Text
getIssueTitle num = issueTitle <$> fetchIssue num

{- | Fetch 'Issue' by 'Id'. If no issue found then print error and
exit with failure.
-}
fetchIssue :: Id Issue -> IO Issue
fetchIssue iNum = withOwnerRepo (\t o r -> issue' t o r iNum) >>= \case
    Left err -> errorMessage (show err) >> exitFailure
    Right issue -> pure issue

{- | Fetches all open milestones. Then figure out the current one and return its
ID as 'Int'.

If it could not fetch, or there is no open milestones then prints a warning
message and returns 'Nothing'.
-}
fetchCurrentMilestoneId :: IO (Maybe Int)
fetchCurrentMilestoneId = withOwnerRepo milestones' >>= \case
    Left err -> warningMessage (show err) >> pure Nothing
    Right ms -> case sortBy (flip compare) $ map (untagId . milestoneNumber) $ toList ms of
        []  -> warningMessage "" >> pure Nothing
        m:_ -> pure $ Just m

-- | Perform action by given auth token, owner and repo name.
withOwnerRepo
    :: (Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error a))
    -> IO (Either Error a)
withOwnerRepo action = getOwnerRepo >>= \case
    Just (owner, repo) -> do
        token <- getGitHubToken
        action token owner repo
    Nothing -> do
        let errorText = "Cannot get the owner/repo names"
        errorMessage errorText
        pure $ Left $ ParseError errorText

{- | Similar to 'withOwnerRepo', but returns the 'UserError' when cannot get the
GitHub Token, as the given action should work with the 'Auth' instead of 'Maybe
Auth'.
-}
withAuthOwnerRepo
    :: (Auth -> Name Owner -> Name Repo -> IO (Either Error a))
    -> IO (Either Error a)
withAuthOwnerRepo action = withOwnerRepo $ \token owner repo -> case token of
    Just auth -> action auth owner repo
    Nothing -> do
        let errorText = "Can not get GITHUB_TOKEN"
        errorMessage errorText
        pure $ Left $ UserError errorText

-- | Smart constructor for @'Id' 'Issue'@.
mkIssueId :: Int -> Id Issue
mkIssueId = mkId $ Proxy @Issue

-- | Smart constructor for 'Name'.
makeName :: forall a . Text -> Name a
makeName = mkName (Proxy @a)

-- | Create new issue with title and assignee.
mkNewIssue :: Text -> Text -> NewIssue
mkNewIssue title login = NewIssue
    { newIssueTitle     = title
    , newIssueBody      = Nothing
    , newIssueAssignees = V.singleton $ makeName @User login
    , newIssueMilestone = Nothing
    , newIssueLabels    = Nothing
    }

-- | Get authentication GitHub token from the environment variable @GITHUB_TOKEN@.
getGitHubToken :: IO (Maybe Auth)
getGitHubToken = do
    token <- lookupEnv "GITHUB_TOKEN"
    pure $ OAuth . encodeUtf8 <$> token

----------------------------------------------------------------------------
-- Fetch and parse name and repo from URL
----------------------------------------------------------------------------

-- | Get the owner and the repository name.
getOwnerRepo :: IO (Maybe (Name Owner, Name Repo))
getOwnerRepo = parseOwnerRepo <$> "git" $| ["remote", "get-url", "origin"]

{- |
__Note:__ this works with GitHub projects!

This function supports four kinds of the URLs:

SSH one:

@
git@github.com:kowainik/hit-on.git
@

or

@
git@github.com:kowainik/hit-on
@

And HTTPS one:

@
https://github.com/kowainik/hit-on.git
@

or

@
https://github.com/kowainik/hit-on
@
-}
parseOwnerRepo :: Text -> Maybe (Name Owner, Name Repo)
parseOwnerRepo url =
    ( T.stripPrefix "git@github.com:"     url
  <|> T.stripPrefix "https://github.com/" url
    ) >>= stripGitSuffix >>= separateName
  where
    separateName :: Text -> Maybe (Name Owner, Name Repo)
    separateName nm =
        let (owner, T.drop 1 -> repo) = T.breakOn "/" nm in
        guard (owner /= "" && repo /= "") *> Just (makeName owner, makeName repo)

    stripGitSuffix :: Text -> Maybe Text
    stripGitSuffix x = whenNothing (T.stripSuffix ".git" x) (Just x)
