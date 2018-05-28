import           PetStore.Server    (startServer)
import           System.Environment

main :: IO ()
main = do
  [port] <- getArgs
  startServer (read port)
