import System.IO
import System.Process

main :: IO ()
main = do
  tty <- openFile "/dev/tty" ReadWriteMode
  let procSpec = shell "stty raw -echo cbreak"
  _ <- createProcess procSpec
  loop tty
  where
    loop tty = do 
        x <- hGetChar tty 
        hPutChar tty x
        hPutStr tty "hello\n"
        if x == 'x' then
          return ()
        else
          loop tty
