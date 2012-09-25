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
          allBranches <- getAllBranches $ path ++ "/.git/info/refs"
          let branches = trackingBranches allBranches
          let userBranchString = args !! 0
          
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
      putStrLn "Usage: fuzzy <substring of branch name>"
      exitFailure
      
checkoutBranch :: Branch -> IO ()
checkoutBranch (LocalBranch name) = do
  output <- readProcess "git" ["checkout", name] []
  putStrLn output
checkoutBranch (RemoteBranch name) = do
  output <- readProcess "git" ["checkout", name] []
  putStrLn output
  

getAllBranches :: FilePath -> IO [Branch]
getAllBranches filePath = do
  fileContents <- readFile filePath
  let fileLines = endBy "\n" fileContents
  let branchNames = map (dropWhile (/= '\t')) fileLines
  let branches = mapMaybe parseBranchName branchNames
  return branches
  
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
  
getWebBranches = do
  allBranches <- getAllBranches "/home/wilfred/work/web/.git/info/refs"
  return $ trackingBranches allBranches

-- we ignore tags
parseBranchName :: String -> Maybe Branch
parseBranchName branchName = 
  if branchLocation == "heads" then 
    Just $ LocalBranch $ join "/" $ drop 2 branchParts
  else if branchLocation == "remotes" then
         Just $ RemoteBranch $ join "/" $ drop 3 branchParts
       else Nothing
  where branchParts = (splitOn "/") branchName
        branchLocation = branchParts !! 1