------------------------------------------------------------------------------
-- A few helper operations.
------------------------------------------------------------------------------

module System.Helpers
 where

import IOExts ( evalCmd )

import HTML.Base

------------------------------------------------------------------------------
-- The upload form is defined as a raw form since the handler
-- is written in Python. The handler script must be stored in file
-- `upload-handler.cgi`.
-- The arguments are the accepted file types, the base name of the file
-- where the contents of the uploaded file is stored (actually, the
-- Python script stores the file in the local directory `uploads`),
-- and the URL loaded after uploading the file.
uploadForm :: String -> String -> String -> HtmlExp
uploadForm filetypes uploadfilename redirecturl =
  HtmlStruct "form" [("method","post"), ("action","upload-handler.cgi"),
                     ("enctype","multipart/form-data")]
    [hiddenField "UPLOADNAME" uploadfilename,
     hiddenField "REDIRECT"   redirecturl,
     HtmlStruct "input" [("type","file"), ("name","FILENAME"),
                         ("accept",filetypes)] [],
     par [HtmlStruct "input" [("type","submit"), ("value", "Hochladen!")] []]
    ]

------------------------------------------------------------------------------
-- Checks whether a file contains a PDF.
isPdfFile :: String -> IO Bool
isPdfFile fn = do
  (rc,out,_) <- evalCmd "file" ["--brief", "--mime-type", fn] ""
  return $ rc == 0 && out == "application/pdf\n"

-- Checks whether a file contains a JPEG
isJpegFile :: String -> IO Bool
isJpegFile fn = do
  (rc,out,_) <- evalCmd "file" ["--brief", "--mime-type", fn] ""
  return $ rc == 0 && out == "image/jpeg\n"

------------------------------------------------------------------------------
