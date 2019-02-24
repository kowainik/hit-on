module Hit.Issue
       ( runIssue

         -- * Internal helpers
       , mkIssueId
       , getIssueTitle
       , getOwnerRepo
       , parseOwnerRepo
       ) where

import GitHub (Error (..), Id, Issue (..), IssueState (..), Name, Owner, Repo, getUrl, mkId, mkName)
import GitHub.Auth (Auth (OAuth))
import GitHub.Data.Options (stateOpen)
import GitHub.Endpoints.Issues (issue', issuesForRepo')
import Shellmet (($|))
import System.Environment (lookupEnv)

import Hit.ColorTerminal (arrow, blueCode, errorMessage, redCode, resetCode)

import qualified Data.Text as T


-- | Run the @issue@ command.
runIssue :: Maybe Int -> IO ()
runIssue = \case
    Just num -> getIssue $ mkIssueId num
    Nothing -> getAllIssues

-- | Get the list of the opened issues for the current project.
getAllIssues :: IO ()
getAllIssues = withOwnerRepo (\t o r -> issuesForRepo' t o r stateOpen) >>= \case
    Left err -> errorMessage $ show err
    Right is -> for_ is (putTextLn . showIssueName blueCode)

-- | Get the 'Issue' by given issue number.
getIssue :: Id Issue -> IO ()
getIssue num = fetchIssue num >>= \case
    Left err -> errorMessage $ show err
    Right is -> putTextLn $ showIssueFull is

showIssueName :: Text -> Issue -> Text
showIssueName colorCode Issue{..} =
    arrow <> colorCode <> "[#" <> show @Text issueNumber <> "] " <> resetCode <> issueTitle

showIssueFull :: Issue -> Text
showIssueFull i@Issue{..} = T.intercalate "\n" $
       showIssueName (statusToCode issueState) i
     : [ "    URL: " <> getUrl url | Just url <- [issueHtmlUrl]]
    ++ [ "    " <> desc | Just (T.strip -> desc) <- [issueBody], desc /= ""]
  where
    statusToCode :: IssueState -> Text
    statusToCode = \case
        StateOpen -> blueCode
        StateClosed -> redCode

mkIssueId :: Int -> Id Issue
mkIssueId = mkId $ Proxy @Issue

makeName :: forall a . Text -> Name a
makeName = mkName (Proxy @a)

fetchIssue :: Id Issue -> IO (Either Error Issue)
fetchIssue iNum = withOwnerRepo (\t o r -> issue' t o r iNum)

getIssueTitle :: Id Issue -> IO Text
getIssueTitle num = fetchIssue num >>= \case
    Left err -> errorMessage (show err) >> exitFailure
    Right Issue{..} -> pure issueTitle

withOwnerRepo
    :: (Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error a))
    -> IO (Either Error a)
withOwnerRepo action = getOwnerRepo >>= \case
    Just (owner, repo) -> do
        token <- lookupEnv "GITHUB_TOKEN"
        let gitHubToken = OAuth . encodeUtf8 <$> token
        action gitHubToken owner repo
    Nothing -> do
        let errTxt = "Can not get the owner/repo names"
        errorMessage errTxt
        pure $ Left $ ParseError errTxt


-- | Get the owner and the repository name.
getOwnerRepo :: IO (Maybe (Name Owner, Name Repo))
getOwnerRepo = parseOwnerRepo <$> "git" $| ["remote", "get-url", "origin"]

{- |
__Note:__ this works with GitHub projects!

This function supports two kinds of the URLs:

SSH one:

@
git@github.com:kowainik/hit-on.git
@

And HTTPS one:

@
https://github.com/kowainik/hit-on.git
@
-}
parseOwnerRepo :: Text -> Maybe (Name Owner, Name Repo)
parseOwnerRepo url =
    ( T.stripPrefix "git@github.com:"     url
  <|> T.stripPrefix "https://github.com/" url
    ) >>= T.stripSuffix ".git" >>= separateName
  where
    separateName :: Text -> Maybe (Name Owner, Name Repo)
    separateName nm =
        let (owner, T.drop 1 -> repo) = T.breakOn "/" nm in
        guard (owner /= "" && repo /= "") *> Just (makeName owner, makeName repo)
