import Data.List.Split
import Data.String.Utils -- from MissingH

data Branch = LocalBranch String | RemoteBranch String deriving Show

main = putStrLn "Hello, World!"

getAllBranches :: String -> IO [Maybe Branch]
getAllBranches filePath = do
  fileContents <- readFile filePath
  let fileLines = endBy "\n" fileContents
  let branchNames = map (dropWhile (/= '\t')) fileLines
  let branches = map parseBranchName branchNames
  return branches
  
getWebBranches = getAllBranches "/home/wilfred/work/web/.git/info/refs"

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
        
        
  