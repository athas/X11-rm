{-# LANGUAGE DeriveDataTypeable #-}
-- | Note that @XrmInitialize@ is already wrapped in the base X11
-- library as 'Graphics.X11.Xlib.rmInitialize'.
module Graphics.X11.XRM
       (
         -- * Resource manager
         RMDatabase,
         RMValue(..),
         rmGetFileDatabase,
         rmPutFileDatabase,
         rmGetStringDatabase,
         rmLocaleOfDatabase,
         rmDestroyDatabase,
         rmSetDatabase,
         rmGetDatabase,
         rmCombineFileDatabase,
         rmCombineDatabase,
         rmMergeDatabases,
         rmGetResource,
         rmPutResource,
         rmPutStringResource,
         rmPutLineResource,
         getDefault,
         rmValue,
       ) where
       
import Graphics.X11.Xlib

import Data.Data
import Foreign
import Foreign.C.String
import Foreign.C.Types

#include <X11/Xresource.h>

-- | pointer to an X11 @XrmDatabase@ structure
newtype RMDatabase = RMDatabase (Ptr RMDatabase)
#if __GLASGOW_HASKELL__
        deriving (Eq, Ord, Show, Typeable, Data)
#else
        deriving (Eq, Ord, Show)
#endif

-- | counterpart of an X11 @XrmValue@ structure
data RMValue = RMValue {
      rmvalue_size :: CInt,
      rmvalue_addr :: IntPtr
    }
    deriving (Eq, Show)

instance Storable RMValue where
  sizeOf _ = #{size XrmValue}
  alignment _ = alignment (undefined::CInt)
  peek p = do size <- #{peek XrmValue,size} p
              addr <- #{peek XrmValue,addr} p
              return (RMValue size addr)

-- | interface to the X11 library function @XrmGetFileDatabase()@.
rmGetFileDatabase :: String -> IO (Maybe RMDatabase)
rmGetFileDatabase file = withCString file $ \ c_file -> do
                         RMDatabase db <- xrmGetFileDatabase c_file
                         if db == nullPtr
                            then return Nothing
                            else return $ Just $ RMDatabase db
foreign import ccall unsafe "HsXlib.h XrmGetFileDatabase"
        xrmGetFileDatabase :: CString -> IO RMDatabase

-- | interface to the X11 library function @XrmPutFileDatabase()@.
rmPutFileDatabase :: RMDatabase -> String -> IO ()
rmPutFileDatabase db file = withCString file $ \ c_file ->
                            xrmPutFileDatabase db c_file
foreign import ccall unsafe "HsXlib.h XrmPutFileDatabase"
        xrmPutFileDatabase :: RMDatabase -> CString -> IO ()

-- | interface to the X11 library function @XrmGetStringDatabase()@.
rmGetStringDatabase :: String -> IO RMDatabase
rmGetStringDatabase s = withCString s $ \ c_s ->
                        xrmGetStringDatabase c_s
foreign import ccall unsafe "HsXlib.h XrmGetStringDatabase"
        xrmGetStringDatabase :: CString -> IO RMDatabase

-- | interface to the X11 library function @XrmLocaleOfDatabase()@.
rmLocaleOfDatabase :: RMDatabase -> IO String
rmLocaleOfDatabase db = peekCString =<< xrmLocaleOfDatabase db
foreign import ccall unsafe "HsXlib.h XrmLocaleOfDatabase"
        xrmLocaleOfDatabase :: RMDatabase -> IO CString

-- | interface to the X11 library function @XrmDestroyDatabase()@.
foreign import ccall unsafe "HsXlib.h XrmDestroyDatabase"
        rmDestroyDatabase :: RMDatabase -> IO ()

-- | interface to the X11 library function @XrmSetDatabase()@.
foreign import ccall unsafe "HsXlib.h XrmSetDatabase"
        rmSetDatabase :: Display -> RMDatabase -> IO ()

-- | interface to the X11 library function @XrmGetDatabase()@.
rmGetDatabase :: Display -> IO (Maybe RMDatabase)
rmGetDatabase dpy = do RMDatabase db <- xrmGetDatabase dpy
                       if db == nullPtr
                          then return Nothing
                          else return $ Just $ RMDatabase db
foreign import ccall unsafe "HsXlib.h XrmGetDatabase"
        xrmGetDatabase :: Display -> IO RMDatabase

-- | interface to the X11 library function @XrmCombineFileDatabase()@.
rmCombineFileDatabase :: String -> RMDatabase -> Bool -> IO ()
rmCombineFileDatabase file db override = withCString file $ \ c_file ->
        throwIfZero "rmCombineFileDatabase" $
        xrmCombineFileDatabase c_file db override
foreign import ccall unsafe "HsXlib.h XrmCombineFileDatabase"
        xrmCombineFileDatabase :: CString -> RMDatabase -> Bool -> IO Status

-- | interface to the X11 library function @XrmCombineDatabase()@.
foreign import ccall unsafe "HsXlib.h XrmCombineDatabase"
        rmCombineDatabase :: RMDatabase -> RMDatabase -> Bool -> IO ()

-- | interface to the X11 library function @XrmMergeDatabases()@.
foreign import ccall unsafe "HsXlib.h XrmMergeDatabases"
        rmMergeDatabases :: RMDatabase -> RMDatabase -> IO ()

-- | interface to the X11 library function @XrmGetResource()@.
rmGetResource :: RMDatabase -> String -> String -> IO (Maybe (String, RMValue))
rmGetResource db name clss = withCString name $ \c_name ->
                             withCString clss $ \c_clss ->
                             alloca $ \type_ret ->
                             alloca $ \val_ret -> do
                               b <- xrmGetResource db c_name c_clss type_ret val_ret
                               if b
                                  then do s <- peekCString =<< peek type_ret
                                          v <- peek val_ret
                                          return $ Just (s, v)
                                  else return Nothing
foreign import ccall unsafe "HsXlib.h XrmGetResource"
        xrmGetResource :: RMDatabase -> CString -> CString 
                       -> Ptr CString -> Ptr RMValue -> IO Bool

-- | interface to the X11 library function @XrmPutResource()@.
rmPutResource :: RMDatabase -> String -> String -> RMValue -> IO ()
rmPutResource db name clss val = withCString name $ \c_name ->
                                 withCString clss $ \c_clss ->
                                 alloca $ \c_val -> do
                                   poke c_val val
                                   xrmPutResource db c_name c_clss c_val
foreign import ccall unsafe "HsXlib.h XrmPutResource"
        xrmPutResource :: RMDatabase -> CString -> CString 
                       -> Ptr RMValue -> IO ()

-- | interface to the X11 library function @XrmPutStringResource()@.
rmPutStringResource :: RMDatabase -> String -> String -> IO ()
rmPutStringResource db spec val = withCString spec $ \c_spec ->
                                  withCString val $ \c_val ->
                                  xrmPutStringResource db c_spec c_val
foreign import ccall unsafe "HsXlib.h XrmPutStringResource"
        xrmPutStringResource :: RMDatabase -> CString -> CString -> IO ()

-- | interface to the X11 library function @XrmPutLineResource()@.
rmPutLineResource :: RMDatabase -> String -> IO ()
rmPutLineResource db line = withCString line $ \c_line ->
                            xrmPutLineResource db c_line
foreign import ccall unsafe "HsXlib.h XrmPutLineResource"
        xrmPutLineResource :: RMDatabase -> CString -> IO ()

-- | interface to the X11 library function @XGetDefault()@.
getDefault :: Display -> String -> String -> IO (Maybe String)
getDefault dpy prog opt = withCString prog $ \ c_prog ->
                          withCString opt $ \ c_opt -> do
                            s <- xGetDefault dpy c_prog c_opt
                            if s == nullPtr
                               then return Nothing 
                               else Just `fmap` peekCString s
foreign import ccall unsafe "HsXlib.h XGetDefault"
        xGetDefault :: Display -> CString -> CString -> IO CString

-- | Extract string from RMValue structure.  Make sure the RMValue
-- actually points at a null-terminated string.
rmValue :: RMValue -> IO String
rmValue val = peekCString $ intPtrToPtr $ rmvalue_addr val
