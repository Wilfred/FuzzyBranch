import System.Directory(getCurrentDirectory, doesDirectoryExist) -- from directory
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.FilePath(takeDirectory)
import System.Process(readProcess)
import Control.Monad(liftM)
import Data.List(isInfixOf)
import Data.List.Split(splitOn,endBy) -- from split
import Data.String.Utils(join) -- from MissingH
import Data.Maybe(mapMaybe)
import Data.Monoid(mappend)


data Branch = LocalBranch String | RemoteBranch String deriving (Show, Eq)

main = do
  args <- getArgs
  case args of
    [branchNameSubstring] -> do
      repoPath <- getCurrentDirectory >>= gitDirectory
      case repoPath of
        Nothing -> do
          putStrLn "No git repo here or in any parent directory."
          exitFailure
        Just path -> do
          allBranches <- getAllBranches
          let branches = trackingBranches allBranches
          
          case matchBranches branchNameSubstring branches of
            [] -> do
              putStrLn $ "Couldn't find any branches that match '" ++ branchNameSubstring ++ "'"
              exitFailure
            [branch] ->
              checkoutBranch branch
            (b:bs) -> do
              putStrLn $ "Found multiple branches that match '" ++ branchNameSubstring ++ "'"
              putStrLn $ join ", " $ map show (b:bs)
              exitFailure
              
    otherwise -> do
      putStrLn "Usage: git-fuzzy <substring of branch name>"
      exitFailure
      
checkoutBranch :: Branch -> IO ()
checkoutBranch (LocalBranch name) = do
  output <- readProcess "git" ["checkout", name] []
  putStr output
checkoutBranch (RemoteBranch name) = do
  output <- readProcess "git" ["checkout", name] []
  putStr output
  

getAllBranches :: IO [Branch]
getAllBranches = do
  localBranchListing <- readProcess "git" ["branch", "--no-color"] []
  let localBranchNames = [drop 2 name | name <- splitOn "\n" localBranchListing, name /= ""]
      localBranches = [LocalBranch name | name <- localBranchNames]
  remoteBranchListing <- readProcess "git" ["branch", "--no-color"] []
  let remoteBranchNames = [drop 2 name | name <- splitOn "\n" remoteBranchListing, name /= ""]
      remoteBranches = [RemoteBranch name | name <- remoteBranchNames]
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
-- TODO: an exact match is better than substring match
matchBranches :: String -> [Branch] -> [Branch]
matchBranches needle [] = []
matchBranches needle ((LocalBranch name):branches) = 
  if isInfixOf needle name then
    (LocalBranch name) : (matchBranches needle branches)
  else
    matchBranches needle branches
matchBranches needle ((RemoteBranch name):branches) = 
  if isInfixOf needle name then
    (RemoteBranch name) : matchBranches needle branches
  else
    matchBranches needle branches
