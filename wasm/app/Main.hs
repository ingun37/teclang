module Main (main) where

import Data.Aeson qualified as J
import Data.ByteString qualified as B
import Data.ByteString.Unsafe qualified as BU
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Foreign.C (CString)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import GHC.Internal.Foreign.Marshal.Alloc qualified as Alloc
import GHC.Wasm.Prim
import MyLib qualified ()
import Text.Pretty.Simple qualified as Simple
import Util qualified

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

foreign import javascript unsafe "console.log($1)"
  js_print :: JSString -> IO ()

foreign export ccall fibo :: Int -> IO Int

fibo :: Int -> IO Int
fibo n = do
  return $ n + 2

-- FFI wrapper to accept a C-style string pointer and length
foreign export ccall processString :: Ptr CChar -> CInt -> IO (Ptr CChar)

processString :: Ptr CChar -> CInt -> IO (Ptr CChar)
processString ptr len = do
  -- 1. Convert the C-style string (Ptr CChar + length) into a Haskell ByteString
  bs <- B.packCStringLen (ptr, fromIntegral len)

  -- 2. Decode the ByteString into Text
  let input_text = TE.decodeUtf8 bs

  -- 3. Run the main logic
  let result_text = input_text

  -- 4. Encode the result Text back to a ByteString
  let resultString = T.unpack result_text
  -- 5. Convert the ByteString to a NUL-terminated CString
  --    (You MUST ensure this memory is managed/freed by the host later)
  (ptr, len) <- newCStringLen resultString
  return ptr

foreign export javascript "jsTest" jsTest :: JSString -> IO JSString

jsTest :: JSString -> IO JSString
jsTest jsval = do
  putStrLn "jsval called!"
  putStrLn $ fromJSString jsval
  return $ toJSString "hello js i'm haskell"

foreign export javascript "parseHaskellData" parseHaskellData :: JSString -> IO JSString
parseHaskellData :: JSString -> IO JSString
parseHaskellData code = toJSString <$> Util.parseHaskellData (fromJSString code)

foreign export javascript "formatHaskell" formatHaskell :: JSString -> IO JSString
formatHaskell :: JSString -> IO JSString
formatHaskell = return . toJSString . TL.unpack . Simple.pStringNoColor . fromJSString

foreign export javascript "makeHaskellData" makeHaskellData :: JSString -> IO JSString
makeHaskellData :: JSString -> IO JSString
makeHaskellData code = toJSString <$> Util.makeHaskellData (fromJSString code)