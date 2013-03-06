import System.Directory(getCurrentDirectory, doesDirectoryExist) -- from directory
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.FilePath(takeDirectory)
import System.Process(readProcess)
import Control.Monad(liftM)
import Data.List(isInfixOf, isPrefixOf)
import Data.List.Split(splitOn,endBy) -- from split
import Data.String.Utils(join) -- from MissingH
import Data.Maybe(mapMaybe)
import Data.Monoid(mappend)


type BranchName = String
type CommitHash = String
data Branch = LocalBranch String | RemoteBranch String deriving (Show, Eq)

main = do
  args <- getArgs
  case args of
    [searchString] -> do
      repoPath <- getCurrentDirectory >>= gitDirectory
      case repoPath of
        Nothing -> do
          putStrLn "No git repo here or in any parent directory."
          exitFailure
        Just path -> do
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
              (b:bs) -> do
                putStrLn $ "Found multiple branches that match '" ++ searchString ++ "'"
                putStrLn $ join ", " $ map show (b:bs)
                exitFailure
              
    _ -> do
      putStrLn "Usage: git-fuzzy <substring of branch name>"
      exitFailure
      
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
    beforeSplit = splitElements !! 0
    afterSplit = join element $ drop 1 splitElements
  in
   (beforeSplit, afterSplit)


getAllBranches :: IO [Branch]
getAllBranches = do
  localBranchListing <- readProcess "git" ["branch", "--no-color"] []
  let localNames = [drop 2 name | name <- splitOn "\n" localBranchListing, name /= ""]
      localBranches = [LocalBranch name | name <- localNames]
  remoteBranchListing <- readProcess "git" ["branch", "-r", "--no-color"] []
  let remoteNames = [drop 2 name | name <- splitOn "\n" remoteBranchListing, name /= ""]
      -- discard origin/HEAD
      remoteNames' = [name | name <- remoteNames, not $ "origin/HEAD" `isPrefixOf` name]
      
      -- TODO: save the remote name instead of just discarding it
      remoteNames'' = [snd $ splitFirst "/" name | name <- remoteNames']
      
      remoteBranches = [RemoteBranch name | name <- remoteNames'']
  return $ concat [localBranches, remoteBranches]
  
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
  let localNames = [LocalBranch n | LocalBranch n <- branches]
      remoteOnlyNames = [RemoteBranch n | RemoteBranch n <- branches, not $ (LocalBranch n) `elem` localNames]
  in
   mappend localNames remoteOnlyNames
   
-- filter branches to only those whose name contains a string
matchBranchSubstring :: [Branch] -> BranchName -> [Branch]
matchBranchSubstring [] needle = []
matchBranchSubstring ((LocalBranch name):branches) needle = 
  if isInfixOf needle name then
    (LocalBranch name) : (matchBranchSubstring branches needle)
  else
    matchBranchSubstring branches needle
matchBranchSubstring ((RemoteBranch name):branches) needle = 
  if isInfixOf needle name then
    (RemoteBranch name) : matchBranchSubstring branches needle
  else
    matchBranchSubstring branches needle

-- return Just Branch if we have a branch with this exact name
matchBranchExactly :: [Branch] -> BranchName -> Maybe Branch
matchBranchExactly [] needle = Nothing
matchBranchExactly ((LocalBranch name):branches) needle
  | needle == name = Just $ LocalBranch name
  | otherwise = matchBranchExactly branches needle
matchBranchExactly ((RemoteBranch name):branches) needle
  | needle == name = Just $ RemoteBranch name
  | otherwise = matchBranchExactly branches needle
