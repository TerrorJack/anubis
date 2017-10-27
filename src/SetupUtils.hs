module SetupUtils
  ( getUniqueComponentLocalBuildInfo
  , getLibComponentLocalBuildInfo
  , getLibInstallDirs
  , addLibBuildInfo
  , runLBIProgram
  ) where

import Data.Map
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Verbosity

getUniqueComponentLocalBuildInfo ::
     LocalBuildInfo -> ComponentName -> ComponentLocalBuildInfo
getUniqueComponentLocalBuildInfo lbi k =
  case componentNameMap lbi ! k of
    [r] -> r
    l ->
      error $
      "found " ++
      show (length l) ++ " instances of ComponentLocalBuildInfo, expecting 1"

getLibComponentLocalBuildInfo :: LocalBuildInfo -> ComponentLocalBuildInfo
getLibComponentLocalBuildInfo lbi =
  getUniqueComponentLocalBuildInfo lbi defaultLibName

getLibInstallDirs :: LocalBuildInfo -> InstallDirs FilePath
getLibInstallDirs lbi =
  absoluteComponentInstallDirs
    (localPkgDescr lbi)
    lbi
    (componentUnitId $ getLibComponentLocalBuildInfo lbi)
    NoCopyDest

addLibBuildInfo :: LocalBuildInfo -> BuildInfo -> LocalBuildInfo
addLibBuildInfo lbi bi =
  lbi
  {localPkgDescr = updatePackageDescription (Just bi, []) $ localPkgDescr lbi}

runLBIProgram :: LocalBuildInfo -> Program -> [ProgArg] -> IO ()
runLBIProgram lbi prog =
  runDbProgram
    (fromFlagOrDefault normal $ configVerbosity $ configFlags lbi)
    prog
    (withPrograms lbi)
