
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-cse #-} -- ??? where'd this come from
{-# OPTIONS_GHC -fforce-recomp #-}
{-# LANGUAGE RecordWildCards #-}

module Version
  (
    getVersion
  , dumpVersion
  , toString
  , VersionInfo(..)
  )
  where

import            Data.Version  (showVersion)
import            Git.Embed
import qualified  Paths_b2hs  as P (version)
import            CompileTime


data VersionInfo = VersionInfo {
    version       :: String
  , gitCommit     :: String
  , gitCommitDate :: String
  , gitDirty      :: Bool
  -- , gitCommitMesg :: String
  , gitBranch     :: String
  , gitTag        :: String
  , gitUrl        :: String
  , compileTime   :: String
  }
  deriving (Eq, Show)

-- | Version string
toString :: VersionInfo -> String
toString VersionInfo{..} = unlines [
    "version:         " ++ version
  , "git commit:      " ++ gitCommit
  , "git commit date: " ++ gitCommitDate
  , "git dirty:       " ++ show gitDirty
  -- , "git commit mesg: " ++ gitCommitMesg
  , "git branch:      " ++ gitBranch
  , "git tag:         " ++ gitTag
  , "git url:         " ++ gitUrl
  , "compile time:    " ++ compileTime
  ]

-- | Version string
dumpVersion :: String
dumpVersion = toString getVersion

getVersion :: VersionInfo
getVersion =
    VersionInfo{..}
  where
    version :: String
    version = showVersion P.version

    gitCommit :: String
    gitCommit = $(embedGitRevision)

    gitCommitDate :: String
    gitCommitDate = $(embedGit ["log", "HEAD", "-1", "--date=iso"
                                , "--format=%cd"])

    --gitCommitMesg :: String
    --gitCommitMesg = $(embedGit ["log", "-1", "--pretty=%B"])

    gitBranch :: String
    gitBranch = $(embedGitBranch)

    gitTag :: String
    gitTag = $(embedGitDescribe ["--all"])

    gitUrl :: String
    gitUrl = $(embedGit ["config", "--get", "remote.origin.url"])

    compileTime :: String
    compileTime = $(embedISOTime)

    gitDirty :: Bool
    gitDirty =
        not $ null status
      where
        status :: String
        status = $(embedGit ["status", "--porcelain"])

