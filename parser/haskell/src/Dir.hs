{-# LANGUAGE NoImplicitPrelude #-}

module Dir where
import RIO.Directory
import RIO.FilePath
import RIO

getSubdirsRecursive :: MonadIO m => FilePath -> m [FilePath]
getSubdirsRecursive = getDirFiltered doesDirectoryExist

-- | Recursively get all files and subdirectories in the given directory.
getDirRecursive :: MonadIO m => FilePath -> m [FilePath]
getDirRecursive = getDirFiltered (const $ pure True)

-- | Recursively get all files in the given directory.
--
-- @since 0.2.3.0
getFilesRecursive :: MonadIO m => FilePath -> m [FilePath]
getFilesRecursive fp = getDirRecursive fp >>= filterM doesFileExist

{-# INLINE getDirFiltered #-}
-- | Recursively get all files and subdirectories in the given directory that
-- satisfy the given predicate. Note that the content of subdirectories not
-- matching the filter is ignored. In particular, that means something like
-- @getDirFiltered doesFileExist@ will /not/ recursively return all files.
--
-- @since 0.2.2.0
getDirFiltered :: MonadIO m => 
                (FilePath -> m Bool) -- ^ Filepath filter
               -> FilePath
               -> m [FilePath]
getDirFiltered p fp = do
    all' <- listDirectory fp
    all'' <- filterM p (mkRel <$> all')
    dirs <- filterM doesDirectoryExist all''
    case dirs of
        [] -> pure all''
        ds -> do
            next <- foldMapA (getDirFiltered p) ds
            pure $ all'' ++ next

    where mkRel = (fp </>)
          foldMapA = (fmap fold .) . traverse