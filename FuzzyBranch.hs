import System.Directory(getCurrentDirectory)
import Data.List
import Data.List.Split
import Data.String.Utils -- from MissingH
import Data.Maybe
import Data.Monoid


data Branch = LocalBranch String | RemoteBranch String deriving (Show, Eq)

-- fixme: this breaks on subdirectories of the git repo
main = do
  currentDirectory <- getCurrentDirectory
  let gitRefsPath = currentDirectory ++ "/.git/info/refs"
  putStrLn $ "Looking at path: " ++ gitRefsPath
  allBranches <- getAllBranches $ gitRefsPath
  let branches = trackingBranches allBranches
  putStrLn $ show branches


getAllBranches :: String -> IO [Branch]
getAllBranches filePath = do
  fileContents <- readFile filePath
  let fileLines = endBy "\n" fileContents
  let branchNames = map (dropWhile (/= '\t')) fileLines
  let branches = mapMaybe parseBranchName branchNames
  return branches

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
    Just $ LocalBranch branchNamePart
  else if branchLocation == "remotes" then
         Just $ RemoteBranch branchNamePart
       else Nothing
  where branchParts = (splitOn "/") branchName
        branchLocation = branchParts !! 1
        branchNamePart = join "/" (drop 2 branchParts)
        
        
  