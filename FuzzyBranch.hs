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
              
          -- todo: rename, this isn't necessarily a substring
          case matchBranchExactly branchNameSubstring branches of
            Just branch -> checkoutBranch branch
            
            _ -> case matchBranchSubstring branchNameSubstring branches of
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
-- TODO: switch argument order
matchBranchSubstring :: String -> [Branch] -> [Branch]
matchBranchSubstring needle [] = []
matchBranchSubstring needle ((LocalBranch name):branches) = 
  if isInfixOf needle name then
    (LocalBranch name) : (matchBranchSubstring needle branches)
  else
    matchBranchSubstring needle branches
matchBranchSubstring needle ((RemoteBranch name):branches) = 
  if isInfixOf needle name then
    (RemoteBranch name) : matchBranchSubstring needle branches
  else
    matchBranchSubstring needle branches

-- return Just Branch if we have a branch with this exact name
matchBranchExactly :: String -> [Branch] -> Maybe Branch
matchBranchExactly needle [] = Nothing
matchBranchExactly needle ((LocalBranch name):branches)
  | needle == name = Just $ LocalBranch name
  | otherwise = matchBranchExactly needle branches
matchBranchExactly needle ((RemoteBranch name):branches)
  | needle == name = Just $ RemoteBranch name
  | otherwise = matchBranchExactly needle branches
