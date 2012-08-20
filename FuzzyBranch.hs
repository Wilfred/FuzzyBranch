import System.Directory

main = putStrLn "Hello, World!"

getAllBranches :: IO [FilePath]
getAllBranches = do
  allFilesOrDirectories <- getDirectoryContents "/home/wilfred/work/web/.git/refs/heads/"
  return [filePath | filePath <- allFiles, filePath /= ".", filePath /= ".."]