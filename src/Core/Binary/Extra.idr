module Core.Binary.Extra

import Core.Context
import Core.Core
import Core.Options
import public Core.TTC

import public JSON.Encoder
import public JSON.ToJSON

import public Core.Binary.JSON

import Data.FilePath
import System.File

%hide Text.ParseError.Bin

export
readTTCFile : TTC extra => String -> Ref Bin Binary -> Core (TTCFile extra)
readTTCFile file b
  = do hdr <- fromBuf
       when (hdr /= "TT2") $
         corrupt ("TTC header in " ++ file ++ " " ++ show hdr)
       ver <- fromBuf @{Wasteful} b
       checkTTCVersion file ver ttcVersion
       totalReq <- fromBuf
       sourceFileHash <- fromBuf
       ifaceHash <- fromBuf
       importHashes <- fromBuf
       incData <- fromBuf
       imp <- fromBuf
       ex <- fromBuf
       defs <- fromBuf
       uholes <- fromBuf
       autohs <- fromBuf
       typehs <- fromBuf
       nextv <- fromBuf
       cns <- fromBuf
       nns <- fromBuf
       pns <- fromBuf
       rws <- fromBuf
       prims <- fromBuf
       foreignImpl <- fromBuf
       nds <- fromBuf
       cgds <- fromBuf
       trans <- fromBuf
       fexp <- fromBuf
       pure (MkTTCFile ver totalReq
                       sourceFileHash ifaceHash importHashes incData
                       defs uholes autohs typehs imp nextv cns nns
                       pns rws prims foreignImpl nds cgds trans fexp ex)

export
readTTC : TTC extra => FilePath -> Core (TTCFile extra)
readTTC file = do
  Right buffer <- coreLift $ readFromFile $ interpolate file
    | Left err => throw (InternalError ("\{file}: \{show err}"))
  bin <- newRef Prims.Bin buffer
  readTTCFile (interpolate file) bin

export
writeJSON : Encoder v -> HasIO io => ToJSON a => FilePath -> a -> io (Either FileError ())
writeJSON _ file val = writeFile (interpolate file) (encodeVia v val)
