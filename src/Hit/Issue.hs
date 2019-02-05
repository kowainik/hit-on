module Hit.Issue
       ( runIssue

         -- * Internal helpers
       , mkIssueId
       , getIssueTitle
       ) where

import GitHub (Error, Id, Issue (..), IssueState (..), getUrl, mkId)
import GitHub.Data.Options (stateOpen)
import GitHub.Endpoints.Issues (issue, issuesForRepo)

import Hit.ColorTerminal (arrow, blueCode, errorMessage, redCode, resetCode)

import qualified Data.Text as T


-- | Run the @issue@ command.
runIssue :: Maybe Int -> IO ()
runIssue = \case
    Just num -> getIssue $ mkIssueId num
    Nothing -> getAllIssues

-- | Get the list of the opened issues for the current project.
getAllIssues :: IO ()
getAllIssues = issuesForRepo "kowainik" "hit-on" stateOpen >>= \case
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

fetchIssue :: Id Issue -> IO (Either Error Issue)
fetchIssue = issue "kowainik" "hit-on"

getIssueTitle :: Id Issue -> IO Text
getIssueTitle num = fetchIssue num >>= \case
    Left err -> errorMessage (show err) >> exitFailure
    Right Issue{..} -> pure issueTitle
