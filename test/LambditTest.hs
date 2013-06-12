
import Control.Monad (liftM)
import qualified Lambdit.Requests

main :: IO ()
main = do
    putStrLn "Enter username:"
    username <- getLine
    putStrLn "Enter password:"
    password <- getLine
    putStrLn "Logging in..."
    let x = Lambdit.Requests.login username password
    _ <- print `liftM` x
    putStrLn "...done."
    return ()
