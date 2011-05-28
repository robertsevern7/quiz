{-# LANGUAGE CPP #-}
#if PRODUCTION
import Controller (withQuiz)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = withQuiz $ run 3000
#else
import Controller (withQuiz)
import System.IO (hPutStrLn, stderr)
import Network.Wai.Middleware.Debug (debug)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    let port = 3000
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    withQuiz $ run port . debug
#endif
