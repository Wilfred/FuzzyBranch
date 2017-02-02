import System.Directory(getCurrentDirectory, doesDirectoryExist) -- from directory
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.FilePath(takeDirectory)
import System.Process(readProcess)
import Data.List(isInfixOf, find, delete, lines)
import Data.List.Split(splitOn) -- from split
import Data.String.Utils(join) -- from MissingH
import Data.Monoid(mappend)


type BranchName = String
type CommitHash = String
data Branch = LocalBranch BranchName | RemoteBranch BranchName deriving (Show, Eq)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [searchString] -> do
      repoPath <- getCurrentDirectory >>= gitDirectory
      case repoPath of
        Nothing -> do
          putStrLn "No git repo here or in any parent directory."
          exitFailure
        Just _ -> do
          allBranches <- getAllBranches
          let branches = trackingBranches allBranches
              
          case matchBranchExactly branches searchString of
            Just branch -> checkoutBranch branch
            
            _ -> case matchBranchSubstring branches searchString of
              [] -> do
                putStrLn $ "Couldn't find any branches that match '" ++ searchString ++ "', trying commit hash."
                checkoutCommit searchString
              [branch] ->
                checkoutBranch branch
              branches -> do
                putStrLn $ "Found multiple branches that match '" ++ searchString ++ "':"
                putStr $ unlines $ map formatBranch branches
                exitFailure
              
    _ -> do
      putStrLn "Usage: git-fuzzy <substring of branch name>"
      exitFailure

formatBranch :: Branch -> String
formatBranch (LocalBranch name) = "* " ++ name ++ " (local)"
formatBranch (RemoteBranch name) = "* " ++ name ++ " (remote)"
      
checkoutBranch :: Branch -> IO ()
checkoutBranch (LocalBranch name) = do
  output <- readProcess "git" ["checkout", name] []
  putStr output
checkoutBranch (RemoteBranch name) = do
  output <- readProcess "git" ["checkout", name] []
  putStr output

checkoutCommit :: CommitHash -> IO ()
checkoutCommit name = do
  output <- readProcess "git" ["checkout", name] []
  putStr output

-- equivalent to splitOn, but only split once, on the first split point
splitFirst :: Eq a => [a] -> [a] -> ([a], [a])
splitFirst element list =
  let
    splitElements = splitOn element list
    beforeSplit = head splitElements
    afterSplit = join element $ drop 1 splitElements
  in
   (beforeSplit, afterSplit)


type RefsPath = String

-- Call git for-each-ref and return the lines written to stdout.
-- http://stackoverflow.com/a/40122019/509706
gitRefs :: RefsPath -> IO [String]
gitRefs path = do
  stdout <- readProcess "git" ["for-each-ref", "--format=%(refname:short)", path] []
  return $ delete "" $ lines stdout

getAllBranches :: IO [Branch]
getAllBranches = do
  localBranchNames <- gitRefs "refs/heads/"
  let localBranches = map LocalBranch localBranchNames
  remoteBranchNames <- gitRefs "refs/remotes/origin/"
  -- TODO: save the remote name instead of just discarding it
  let remoteBranchNames' = [snd $ splitFirst "/" name | name <- remoteBranchNames, name /= "origin/hEAD"]
  let remoteBranches = map RemoteBranch remoteBranchNames'
  return $ localBranches ++ remoteBranches
  
-- FIXME: assumes / is never a git repo
gitDirectory :: FilePath -> IO (Maybe FilePath)
gitDirectory "/" = return Nothing
gitDirectory path = do
  hasDotGit <- isGitDirectory path
  if hasDotGit 
    then 
    return $ Just path 
    else 
    gitDirectory $ takeDirectory path

isGitDirectory :: FilePath -> IO Bool
isGitDirectory path = doesDirectoryExist $ path ++ "/.git"

-- a list of local branches and only the remote branches that don't
-- have corresponding local branches
trackingBranches :: [Branch] -> [Branch]
trackingBranches [] = []
trackingBranches branches = 
  let localBranchNames = [LocalBranch n | LocalBranch n <- branches]
      remoteOnlyNames = [RemoteBranch n | RemoteBranch n <- branches, LocalBranch n `notElem` localBranchNames]
  in
   mappend localBranchNames remoteOnlyNames

branchName :: Branch -> String
branchName (LocalBranch name) = name
branchName (RemoteBranch name) = name

-- filter branches to only those whose name contains a string
matchBranchSubstring :: [Branch] -> BranchName -> [Branch]
matchBranchSubstring branches needle = filter substringMatches branches
  where
    substringMatches b = needle `isInfixOf` branchName b

-- return Just Branch if we have a branch with this exact name
matchBranchExactly :: [Branch] -> BranchName -> Maybe Branch
matchBranchExactly branches needle = find nameMatches branches
  where
    nameMatches b = needle == branchName b
