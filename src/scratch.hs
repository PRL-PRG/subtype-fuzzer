-- main working (mostly) in Conv monad instead of IO
{-
main :: IO (ConvRes [()]) 
main = convRes $ do 
    pvs <- liftIO vs
    cs <- mapM (fmap (uncurry trivialAugmenter) 
               . liftListen listenThroughIO
               . converter holeC
               ) pvs
    liftIO $ mapM (putStrLn . show) cs
-}

