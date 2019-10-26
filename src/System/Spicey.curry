--------------------------------------------------------------------------
--- This module implements some auxiliary operations to support the
--- generic implementation of the Spicey entities.
--------------------------------------------------------------------------

module System.Spicey (
  Controller, EntityController(..), showRoute, editRoute, deleteRoute,
  applyControllerOn,
  redirectController,
  nextController, nextControllerForData,
  confirmDeletionPage,
  transactionController,
  getControllerURL,getControllerParams, showControllerURL,
  getPage, wDateType, wBoolean, wUncheckMaybe, wFloat,
  displayError, displayUrlError, cancelOperation,
  renderWUI, renderLabels,
  nextInProcessOr,
  stringToHtml, maybeStringToHtml,
  intToHtml,maybeIntToHtml, floatToHtml, maybeFloatToHtml,
  boolToHtml, maybeBoolToHtml, dateToHtml, maybeDateToHtml,
  userDefinedToHtml, maybeUserDefinedToHtml,
  spTable,
  setPageMessage, getPageMessage,
  saveLastUrl, getLastUrl, getLastUrls, getCurrentCatsURL
  ) where

import Char         ( isSpace, isDigit )
import Global
import ReadShowTerm ( readsQTerm )
import Time

import Database.CDBI.Connection ( SQLResult )
import HTML.Base
import HTML.Session
import HTML.Styles.Bootstrap3
import HTML.WUI

import Config.Storage
import Config.UserProcesses
import System.Routes
import System.Processes
import System.Authentication
import System.SessionInfo

--------------------------------------------------------------------------
-- a viewable can be turned into a representation which can be displayed
-- as interface
-- here: a representation of a HTML page
type Viewable = HtmlPage

type ViewBlock = [HtmlExp]

--- Controllers contains all logic and their result should be a Viewable.
--- if the behavior of controller should depend on URL parameters
--- (following the first name specifying the controller), one
--- can access these URL parameters by using the operation
--- Spicey.getControllerParams inside the controller.
type Controller = IO ViewBlock


--- The type class `EntityController` contains:
--- * the application of a controller to some entity identified by a key string
--- * an operation to construct a URL route for an entity w.r.t. to a route
---   string
class EntityController a where
  controllerOnKey :: String -> (a -> Controller) -> Controller

  entityRoute :: String -> a -> String


--- Returns the URL route to show a given entity.
showRoute :: EntityController a => a -> String
showRoute = entityRoute "show"

--- Returns the URL route to edit a given entity.
editRoute :: EntityController a => a -> String
editRoute = entityRoute "edit"

--- Returns the URL route to delete a given entity.
deleteRoute :: EntityController a => a -> String
deleteRoute = entityRoute "delete"


--- Reads an entity for a given key and applies a controller to it.
applyControllerOn :: Maybe enkey -> (enkey -> IO en)
                  -> (en -> Controller) -> Controller
applyControllerOn Nothing _ _ = displayUrlError
applyControllerOn (Just userkey) getuser usercontroller =
  getuser userkey >>= usercontroller

--- A controller to redirect to an URL starting with "?"
--- (see implementation of `getPage`).
redirectController :: String -> Controller
redirectController url = return [HtmlText url]

nextController :: Controller -> _ -> IO HtmlPage
nextController controller _ = do
  view <- controller
  getPage view

-- for WUIs
nextControllerForData :: (a -> Controller) -> a -> IO HtmlPage
nextControllerForData controller param = do
  view <- controller param
  getPage view


--- Generates a page to ask the user for a confirmation to delete an entity
--- specified in the controller URL (of the form "entity/delete/key/...").
--- The yes/no answers are references derived from the controller URL
--- where the second argument is replaced by "destroy"/"show".
--- @param question - a question asked
--- @param yescontroller - the controller used if the answer is "yes"
--- @param nocontroller  - the controller used if the answer is "no"
confirmDeletionPage :: String -> Controller
confirmDeletionPage question = do
  (entity,ctrlargs) <- getControllerURL
  listurl           <- getCurrentCatsURL
  case ctrlargs of
    (_:args) -> return $
      [h3 [htxt question],
       par [hrefButton (showControllerURL entity ("destroy":args)) [htxt "Yes"],
            nbsp,
            hrefButton listurl [htxt "No"]]]
    _ -> displayUrlError


--- A controller to execute a transaction and proceed with a given
--- controller if the transaction succeeds. Otherwise, the
--- transaction error is shown.
--- @param trans - the transaction to be executed
--- @param controller - the controller executed in case of success
transactionController :: IO (SQLResult _) -> Controller -> Controller
transactionController trans controller = do
  transResult <- trans
  either (\error -> displayError (show error))
         (\_     -> controller)
         transResult

--- If we are in a process, execute the next process depending on
--- the provided information passed in the second argument,
--- otherwise execute the given controller (first argument).
nextInProcessOr :: Controller -> Maybe ControllerResult -> Controller
nextInProcessOr controller arg = do
  isproc <- isInProcess
  if isproc then advanceInProcess arg >> return [htxt ""] -- triggers redirect
            else controller


--------------------------------------------------------------------------
-- Operations for handling URL parameters

--- Parse the URL parameter passed to the main script. The result is a pair
--- consisting of the route and the list of parameters separated by '/'.
parseUrl :: String -> (String, [String])
parseUrl urlparam =
  let (url:ctrlparams) = splitUrl urlparam
  in  (url,ctrlparams)
  
--- Splits the URL parameter passed to the main script into a list of
--- strings. The strings are separated in the URL by '/'.
splitUrl :: String -> [String]
splitUrl url =
  let (ys,zs) = break (== '/') url
   in if null zs then [ys]
                 else ys : splitUrl (tail zs)

--- Gets the controller URL and the control parameters (separated by '/').
--- For instance, if the spicey script is called with the URL
--- "spicey.cgi?listEntity/arg1/arg2", this operation returns
--- ("listEntity",["arg1","arg2"]).
getControllerURL :: IO (String, [String])
getControllerURL = getUrlParameter >>= return . parseUrl

--- Gets the control parameters from the current URL.
getControllerParams :: IO [String]
getControllerParams = getUrlParameter >>= return . snd . parseUrl

--- Shows the URL corresponding to the control parameters.
--- The first argument is the URL of the controller (e.g., "listEntity")
--- and the second argument is the list of control parameters.
showControllerURL :: String -> [String] -> String
showControllerURL ctrlurl params = '?' : ctrlurl ++ concatMap ('/':) params

--------------------------------------------------------------------------
--- Standard rendering for WUI forms to edit data.
--- @param title      - the title of the WUI form
--- @param buttontag  - the text on the submit button
--- @param cancelurl  - the URL selected if submission is cancelled
--- @param envpar     - environment parameters (e.g., user session data)
--- @param hexp       - the HTML expression representing the WUI form
--- @param handler    - the handler for submitting data
renderWUI :: String -> String -> String
          -> a -> HtmlExp -> (CgiEnv -> Controller) -> [HtmlExp]
renderWUI title buttontag cancelurl _ hexp handler =
  [h1 [htxt title],
   hexp,
   breakline,
   primButton buttontag (\env -> handler env >>= getPage),
   hrefButton cancelurl [htxt "Abbrechen"]]


--- A WUI for manipulating CalendarTime entities.
--- It is based on a WUI for dates, i.e., the time is ignored.
wDateType :: WuiSpec ClockTime
wDateType = transformWSpec (tuple2date,date2tuple) wDate
 where
  tuple2date :: (Int, Int, Int) -> ClockTime
  tuple2date (day, month, year) =
    toClockTime (CalendarTime year month day 0 0 0 0)

  date2tuple :: ClockTime -> (Int, Int, Int)
  date2tuple ct = let CalendarTime year month day _ _ _ _ = toUTCTime ct
                  in (day, month, year)

--- A WUI for manipulating date entities.
wDate :: WuiSpec (Int, Int, Int)
wDate = wTriple (wSelectInt [1..31])
                (wSelectInt [1..12])
                (wSelectInt [1950..2050])

--- A WUI for manipulating Boolean entities. In general, this view should
--- be specialized by replacing true and false by more comprehensible strings.
wBoolean :: WuiSpec Bool
wBoolean = wSelectBool "True" "False"

--- A WUI transformer to map WUIs into WUIs for corresponding Maybe types.
wUncheckMaybe :: Eq a => a -> WuiSpec a -> WuiSpec (Maybe a)
wUncheckMaybe defval wspec =
  wMaybe (transformWSpec (not,not) (wCheckBool [htxt "No value"]))
         wspec
         defval

--- A widget for editing floating point values.
wFloat :: WuiSpec Float
wFloat = transformWSpec (readFloat, show)
            (wString `withCondition` (\s -> readMaybeFloat s /= Nothing))
 where
   readFloat s = maybe 0.0 id (readMaybeFloat s)

-- Read a float in a string.
-- Return Nothing is this is not a float string.
readMaybeFloat :: String -> Maybe Float
readMaybeFloat s =
  if all isFloatChar s
   then case readsQTerm s of
          [(x,tail)] -> if all isSpace tail then Just x else Nothing
          _  ->  Nothing
   else Nothing
 where
   isFloatChar c = isDigit c || c == '.'

--------------------------------------------------------------------------
-- Define page layout of the application.

--- The title of this application (shown in the header).
spiceyTitle :: String
spiceyTitle = "Michaels Rezeptverwaltung"

--- The home URL and brand shown at the left top of the main page.
spiceyHomeBrand :: (String, [HtmlExp])
spiceyHomeBrand = ("?", [homeIcon, htxt " Alle Rezepte"])

--- The standard footer of the Spicey page.
spiceyFooter :: [HtmlExp]
spiceyFooter =
  [par [htxt "powered by",
        href "http://www.informatik.uni-kiel.de/~pakcs/spicey"
             [image "images/spicey-logo.png" "Spicey"]
          `addAttr` ("target","_blank"),
        htxt "Framework"]]
        
--- Transforms a view into an HTML form by adding the basic page layout.
--- If the view is an empty text or a text starting with "?",
--- generates a redirection page.
getPage :: ViewBlock -> IO HtmlPage
getPage viewblock = case viewblock of
  [HtmlText ""]          -> return $ redirectPage "spicey.cgi"
  [HtmlText ('?':route)] -> return $ redirectPage ('?':route)
  _ -> do
    cats      <- getCurrentCats
    listurl   <- getCurrentCatsURL
    routemenu <- getRouteMenu
    msg       <- getPageMessage
    login     <- getSessionLogin
    lasturl   <- getLastUrl
    withSessionCookie $ bootstrapPage "." ["bootstrap.min","spicey"]
      spiceyTitle spiceyHomeBrand routemenu (rightTopMenu login)
      0 []  [h1 [htxt spiceyTitle]]
      (messageLine msg lasturl cats listurl : viewblock ) spiceyFooter
 where
  messageLine msg lasturl cats listurl =
    if null msg
      then HtmlStruct "header" [("class","pagemessage pagemessage-empty")]
             [htxt $ "Last page: "++lasturl ++ " / Cats: " ++ unwords cats ++ " / " ++ listurl]
      else HtmlStruct "header" [("class","pagemessage")] [htxt msg]
        
  rightTopMenu login =
    [[href "?login" (maybe [loginIcon, nbsp, htxt "Anmelden"]
                           (\n -> [logoutIcon, nbsp, htxt "Abmelden"
                                  ,htxt $ " ("
                                  ,style "text-success" [userIcon]
                                  ,htxt $ " "++n++")"
                                  ])
                           login)]]

-------------------------------------------------------------------------
-- Action performed when a "cancel" button is pressed.
-- In this case, a message is shown.
cancelOperation :: IO ()
cancelOperation = do
  inproc <- isInProcess
  if inproc then removeCurrentProcess else done
  setPageMessage $ (if inproc then "Process" else "Operation") ++ " cancelled"

-- dummy-controller to display an error
displayError :: String -> Controller
displayError msg = do
  inproc <- isInProcess
  if inproc then removeCurrentProcess else done
  setPageMessage ("Error occurred!" ++
                  if inproc then " Process terminated!" else "")
  if null msg
   then return [htxt "General error (shown by function Spicey.displayError)"]
   else return [htxt msg]

--- A controller to display an URL error.
displayUrlError :: Controller
displayUrlError = displayError "Illegal URL"

-- like renderTaggedTuple from WUI Library but takes list of HtmlExp
-- instead of list of strings
renderLabels :: [[HtmlExp]] -> Rendering
renderLabels labels hexps =
  spTable (map (\(l, h) -> [l, [enlargeInput h]]) (zip labels hexps))
 where
  enlargeInput h = h `addClass` "input-xxlarge"

-- Convert standard datatype values to HTML representation
stringToHtml :: String -> HtmlExp
stringToHtml s = textstyle "type_string" s

maybeStringToHtml :: Maybe String -> HtmlExp
maybeStringToHtml s = textstyle "type_string" (maybe "" id s)

intToHtml :: Int -> HtmlExp
intToHtml i = textstyle "type_int" (show i)

maybeIntToHtml :: Maybe Int -> HtmlExp
maybeIntToHtml i = textstyle "type_int" (maybe "" show i)

floatToHtml :: Float -> HtmlExp
floatToHtml i = textstyle "type_float" (show i)

maybeFloatToHtml :: Maybe Float -> HtmlExp
maybeFloatToHtml i = textstyle "type_float" (maybe "" show i)

boolToHtml :: Bool -> HtmlExp
boolToHtml b = textstyle "type_bool" (show b)

maybeBoolToHtml :: Maybe Bool -> HtmlExp
maybeBoolToHtml b = textstyle "type_bool" (maybe "" show b)

dateToHtml :: ClockTime -> HtmlExp
dateToHtml ct = textstyle "type_calendartime" (toDayString (toUTCTime ct))

maybeDateToHtml :: Maybe ClockTime -> HtmlExp
maybeDateToHtml ct =
  textstyle "type_calendartime" (maybe "" (toDayString . toUTCTime) ct)

userDefinedToHtml :: Show a => a -> HtmlExp
userDefinedToHtml ud = textstyle "type_string" (show ud)

maybeUserDefinedToHtml :: Show a => Maybe a -> HtmlExp
maybeUserDefinedToHtml ud = textstyle "type_string" (maybe "" show ud)

--------------------------------------------------------------------------
-- Auxiliary HTML items:

--- Standard table in Spicey. Visualize it with a grid or with
--- a table if there are too many columns.
spTable :: [[[HtmlExp]]] -> HtmlExp
spTable items = table items  `addClass` "table table-hover table-condensed"

--------------------------------------------------------------------------
-- Icons:

homeIcon :: HtmlExp
homeIcon   = glyphicon "home"

loginIcon :: HtmlExp
loginIcon  = glyphicon "log-in"

logoutIcon :: HtmlExp
logoutIcon = glyphicon "log-out"

arrowIcon :: HtmlExp
arrowIcon  = glyphicon "arrow-right"

glyphicon :: String -> HtmlExp
glyphicon n = textstyle ("glyphicon glyphicon-" ++ n) ""

--------------------------------------------------------------------------
-- The page messages are implemented by a session store.
-- We define a global variable to store a message which is shown
-- in the next HTML page of a session.

--- Definition of the session state to store the page message (a string).
pageMessage :: Global (SessionStore String)
pageMessage = global emptySessionStore (Persistent (inDataDir "pageMessage"))

--- Gets the page message and delete it.
getPageMessage :: IO String
getPageMessage = do
  msg <- getSessionData pageMessage ""
  removeSessionData pageMessage
  return msg

--- Set the page message of the current session.
setPageMessage :: String -> IO ()
setPageMessage msg = putSessionData pageMessage msg

--------------------------------------------------------------------------
-- Another example for using sessions.
-- We store the list of selected URLs into  the current session.

--- Definition of the session state to store the last URL (as a string).
lastUrls :: Global (SessionStore [String])
lastUrls = global emptySessionStore (Persistent (inDataDir "lastUrls"))

--- Gets the list of URLs of the current session.
getLastUrls :: IO [String]
getLastUrls = getSessionData lastUrls []

--- Gets the last URL of the current session (or "?").
getLastUrl :: IO String
getLastUrl = do urls <- getLastUrls
                return (if null urls then "?" else head urls)

--- Saves the last URL of the current session.
saveLastUrl :: String -> IO ()
saveLastUrl url = do
  urls <- getLastUrls
  putSessionData lastUrls (url:urls)

--- Gets the URL to list the current category.
getCurrentCatsURL :: IO String
getCurrentCatsURL = do
  cats <- getCurrentCats
  return $ showControllerURL "Category" ("list" : cats)

--------------------------------------------------------------------------
