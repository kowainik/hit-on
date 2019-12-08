{- | This module contains functions to work with issues withing GitHub API.
-}

module Hit.Issue
       ( -- * For CLI commands
         runIssue
       , createIssue

         -- * Internal helpers
       , mkIssueId
       , getIssueTitle
       , getOwnerRepo
       , parseOwnerRepo
       , showIssueName
       ) where

import Data.Vector (Vector)
import GitHub (Error (..), Id, Issue (..), IssueLabel (..), IssueState (..), Name, Owner, Repo,
               SimpleUser (..), User, getUrl, mkId, mkName, unIssueNumber, untagName)
import GitHub.Auth (Auth (OAuth))
import GitHub.Data.Options (stateOpen)
import GitHub.Endpoints.Issues (NewIssue (..), issue', issuesForRepo')
import Shellmet (($|))
import System.Environment (lookupEnv)

import Hit.ColorTerminal (arrow, blueBg, blueCode, boldCode, errorMessage, greenCode, redCode,
                          resetCode)
import qualified Hit.Formatting as Fmt

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified GitHub.Endpoints.Issues as GitHub

----------------------------------------------------------------------------
-- CLI for issues
----------------------------------------------------------------------------

-- | Run the @issue@ command.
runIssue :: Maybe Int -> Maybe Text -> IO ()
runIssue issue me = case issue of
    Just num -> getIssue $ mkIssueId num
    Nothing  -> getAllIssues me

{- | Get the list of the opened issues for the current project and
display short information about each issue.
-}
getAllIssues :: Maybe Text -> IO ()
getAllIssues me = withOwnerRepo (\t o r -> issuesForRepo' t o r stateOpen) >>= \case
    Left err -> errorMessage $ show err
    Right is -> do
        let maxLen = Fmt.maxLenOn (show . issueNumber) is
        for_ (my is) $ \i -> do
            let thisLen = T.length $ show (issueNumber i)
                padSize = maxLen - thisLen
            putTextLn $ showIssueName blueCode padSize i
  where
    my :: Vector Issue -> Vector Issue
    my issues = case me of
        Just (makeName -> username) -> V.filter (assignedTo username . issueAssignees) issues
        Nothing                     -> issues

    -- Is the username an element of assignees vector?
    assignedTo :: Name User -> Vector SimpleUser -> Bool
    assignedTo user = isJust . V.find ((user ==) . simpleUserLogin)

-- | Show issue number with alignment and its name.
showIssueName :: Text -> Int -> Issue -> Text
showIssueName colorCode padSize Issue{..} =
    arrow <> colorCode <> " [#" <> show (unIssueNumber issueNumber) <> "] " <> padding <> resetCode <> issueTitle
  where
    padding :: Text
    padding = T.replicate padSize " "

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
        StateOpen -> blueCode
        StateClosed -> redCode

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
    putLabel x = blueBg <> x <> resetCode

    highlight :: Text -> Text
    highlight x = boldCode <> greenCode <> x <> resetCode

-- | Create an 'Issue' by given title 'Text'
-- QUESTION: should we create 'Login' newtype to add more type-safety here?
createIssue :: Text -> Text -> IO (Either Error Issue)
createIssue title login = withOwnerRepo $ \token owner repo -> case token of
    Just oAuth -> GitHub.createIssue oAuth owner repo $ mkNewIssue title login
    Nothing -> do
        let errorText = "Can not get GITHUB_TOKEN"
        errorMessage errorText
        pure $ Left $ ParseError errorText

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
