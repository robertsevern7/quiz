import Controller
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = putStrLn "Loaded" >> withQuiz (run 3000)
