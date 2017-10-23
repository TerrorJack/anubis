module SetupUtils
  ( getUniqueComponentLocalBuildInfo
  , getLibComponentLocalBuildInfo
  , getLibInstallDirs
  ) where

import Data.Map
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

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
