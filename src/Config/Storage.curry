------------------------------------------------------------------------------
--- Some configurations where data is stored.
------------------------------------------------------------------------------

module Config.Storage where

import FilePath     ( (</>) )

import Config.Globals

--- Prefix a file name with the directory where global form data
--- is stored during run-time.
inDataDir :: String -> String
inDataDir filename = spiceyDataDir </> filename

-- Directory where recipe pictures are stored.
pictureDir :: String
pictureDir = "../recipe/pictures"

-- Directory where recipe pictures are stored.
pdfDir :: String
pdfDir = "../recipe/pdfs"
