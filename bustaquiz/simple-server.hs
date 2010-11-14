import Controller
import Network.Wai.Handler.SimpleServer (run)

main :: IO ()
main = putStrLn "Loaded" >> withQuiz (run 3000)
