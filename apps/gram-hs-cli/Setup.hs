import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Simple.InstallDirs
import System.Directory
import System.FilePath
import Control.Monad

main = defaultMainWithHooks simpleUserHooks {
  postCopy = \args flags pkg_descr lbi -> do
    installManPage args flags pkg_descr lbi
    postCopy simpleUserHooks args flags pkg_descr lbi
  , postInst = \args flags pkg_descr lbi -> do
    installManPage args flags pkg_descr lbi
    postInst simpleUserHooks args flags pkg_descr lbi
}

installManPage :: [String] -> a -> PackageDescription -> LocalBuildInfo -> IO ()
installManPage _ _ pkg_descr lbi = do
  -- Install man page to user-accessible location instead of store
  homeDir <- getHomeDirectory
  let manDir = homeDir </> ".cabal" </> "share" </> "man" </> "man1"
  
  -- Get the package root from LocalBuildInfo
  -- When installing from tarball, the source is in a temp directory
  currentDir <- getCurrentDirectory
  
  -- When installing from tarball, cabal extracts to a temp directory
  -- The source is in a subdirectory named after the package
  -- Try multiple locations for the man page source
  -- 1. Relative to current directory (for source installs)
  let manFile1 = currentDir </> "man" </> "gram-hs.1"
  -- 2. In package directory (for tarball installs, cabal extracts to package-name-version/)
  let manFile2 = currentDir </> "gram-hs-cli-0.1.0.0" </> "man" </> "gram-hs.1"
  -- 3. Try parent directory
  let manFile3 = currentDir </> ".." </> "man" </> "gram-hs.1"
  -- 4. Try to find it by searching up from current directory  
  let manFile4 = currentDir </> ".." </> ".." </> "man" </> "gram-hs.1"
  -- 5. Try in apps/gram-hs-cli/man (if we're in project root)
  let manFile5 = currentDir </> "apps" </> "gram-hs-cli" </> "man" </> "gram-hs.1"
  
  -- Find which man page exists
  exists1 <- doesFileExist manFile1
  exists2 <- doesFileExist manFile2
  exists3 <- doesFileExist manFile3
  exists4 <- doesFileExist manFile4
  exists5 <- doesFileExist manFile5
  
  let manFile = if exists1 then manFile1
                else if exists2 then manFile2
                else if exists3 then manFile3
                else if exists4 then manFile4
                else if exists5 then manFile5
                else ""
  
  if not (null manFile) && (exists1 || exists2 || exists3 || exists4 || exists5)
    then do
      -- Create man directory
      createDirectoryIfMissing True manDir
      
      -- Copy man page
      let dst = manDir </> "gram-hs.1"
      copyFile manFile dst
      putStrLn $ "Installed man page to " ++ dst
    else do
      -- Silently skip if not found (might be installing from different location)
      return ()

