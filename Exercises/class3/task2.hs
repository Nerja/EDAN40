sequence_' :: Monad m => [m ()] -> m ()
sequence_' = foldr (>>) (return())

onlyIf :: Monad m => Bool -> m () -> m ()
onlyIf df act
  | df        = act
  | otherwise = return ()

onlyIfM :: Monad m => m Bool -> m () -> m ()
onlyIfM dfMon act = do df <- dfMon
                       if df
                       then act
                       else return ()

-- onlyIfM (return (read "True" :: Bool)) (putStrLn "hej")
-- onlyIfM (return (read "False" :: Bool)) (putStrLn "hej")
