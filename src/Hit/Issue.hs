module Hit.Issue
       ( runIssue

         -- * Internal helpers
       , mkIssueId
       , getIssueTitle
       , getOwnerRepo
       , parseOwnerRepo
       , issueTitle
       , fetchIssue
       , assignToIssue
       , Issue
       ) where

import Data.Vector (Vector)
import GitHub (Error (..), Id, Issue (..), IssueLabel (..), IssueState (..), Name, Owner, Repo,
               SimpleUser (..), User, getUrl, mkId, mkName, untagName, editIssueAssignees)
import GitHub.Auth (Auth (OAuth))
import GitHub.Data.Options (stateOpen)
import GitHub.Endpoints.Issues (issue', issuesForRepo', editOfIssue, editIssue, unIssueNumber)
import Shellmet (($|))
import System.Environment (lookupEnv)

import Hit.ColorTerminal (arrow, blueBg, blueCode, boldCode, errorMessage, greenCode, redCode,
                          resetCode)

import qualified Data.Text as T
import qualified Data.Vector as V


-- | Run the @issue@ command.
runIssue :: Maybe Int -> Maybe Text -> IO ()
runIssue issue me = case issue of
    Just num -> printFullIssue $ mkIssueId num
    Nothing  -> getAllIssues me

-- | Get the list of the opened issues for the current project.
getAllIssues :: Maybe Text -> IO ()
getAllIssues me = withOwnerRepo (\t o r -> issuesForRepo' t o r stateOpen) >>= \case
    Left err -> errorMessage $ show err
    Right is -> for_ (my is) (putTextLn . showIssueName blueCode)
  where
    my :: Vector Issue -> Vector Issue
    my issues = case me of
        Just (makeName -> username) -> V.filter (assignedTo username . issueAssignees) issues
        Nothing                     -> issues

    -- Is the username an element of assignees vector?
    assignedTo :: Name User -> Vector SimpleUser -> Bool
    assignedTo user = isJust . V.find ((user ==) . simpleUserLogin)

-- | Get the 'Issue' by given issue number.
printFullIssue :: Id Issue -> IO ()
printFullIssue num = fetchIssue num >>= putTextLn . showIssueFull

showIssueName :: Text -> Issue -> Text
showIssueName colorCode Issue{..} =
    arrow <> colorCode <> " [#" <> show @Text issueNumber <> "] " <> resetCode <> issueTitle

showIssueFull :: Issue -> Text
showIssueFull i@Issue{..} = T.intercalate "\n" $
       showIssueName (statusToCode issueState) i
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

mkIssueId :: Int -> Id Issue
mkIssueId = mkId $ Proxy @Issue

makeName :: forall a . Text -> Name a
makeName = mkName (Proxy @a)

fetchIssue :: Id Issue -> IO Issue
fetchIssue iNum = withOwnerRepo (\t o r -> issue' t o r iNum) >>= \case
    Left err -> errorMessage (show err) >> exitFailure
    Right issue -> pure issue

getIssueTitle :: Id Issue -> IO Text
getIssueTitle num = issueTitle <$> fetchIssue num

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

assignToIssue :: Issue -> IO (Either Error Issue)
assignToIssue issue = withOwnerRepo assignAction
  where
    assignAction Nothing _ _ = pure $ Left $ UserError "GITHUB_TOKEN lookup failed"
    assignAction (Just t) o r = do
        let currentAssignees = simpleUserLogin <$> issueAssignees issue
            newAssignee = makeName $ untagName o
            newAssignees = V.cons newAssignee currentAssignees
            isn = mkIssueId $ unIssueNumber $ issueNumber $ issue
        editIssue t o r isn editOfIssue {editIssueAssignees = Just newAssignees}
