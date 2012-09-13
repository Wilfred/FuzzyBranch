import Data.List.Split
import Data.String.Utils -- from MissingH
import Data.Maybe
import Data.Monoid


data Branch = LocalBranch String | RemoteBranch String deriving (Show, Eq)

main = putStrLn "Hello, World!"

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
        
        
  