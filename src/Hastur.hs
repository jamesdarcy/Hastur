module Main
  ( main
  ) where

import Control.Monad
import Data.Bits
import Data.Int
import qualified Data.Map as Map
import Database.HDBC
import Graphics.UI.WX
import Graphics.UI.WXCore
import List (isPrefixOf, zip4)
import System.FilePath
import System.Directory
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

import Data.Dicom
import Data.Dicom.Io
import Data.Dicom.Show
import Data.Dicom.Tag

import Hastur.DB
import Paths_hastur

type ListDbMap = Map.Map Int Int64

data HasturContext = HasturCtx {
  guiFrame :: Frame (),
  guiDbTable :: ListCtrl (),
  guiSeriesList :: ListCtrl (),
  guiText :: TextCtrl (),
  guiStatus :: StatusField,
  dbStudyMap :: Var (ListDbMap),
  dbSeriesMap :: Var (ListDbMap),
  appDataDir :: Var (FilePath),
  dbFile :: Var (FilePath)
}

main :: IO ()
main = start gui

width = 1100
height = 800

--
gui :: IO ()
gui = do
  -- Main UI components
  wgFrame <- frame [text := "Hastur",
               clientSize := sz width height]
  wgHSplit <- splitterWindow wgFrame []
  wgStatus <- statusField [text := ""]
  wgDbTable <- listCtrl wgHSplit [
    columns := [("Patient",AlignLeft,250),
                ("Study",AlignLeft,250),
                ("Date",AlignRight,-1)],
    style := wxLC_SINGLE_SEL .|. wxLC_REPORT .|. wxLC_HRULES]
  wgVSplit <- splitterWindow wgHSplit []
  wgSeriesList <- listCtrl wgVSplit []
  wgText <- textCtrl wgVSplit []
  studyIdMap <- varCreate Map.empty
  seriesIdMap <- varCreate Map.empty
  appDataDir <- varCreate ""
  dbFile <- varCreate ""

  let guiCtx = HasturCtx wgFrame wgDbTable wgSeriesList wgText wgStatus studyIdMap seriesIdMap appDataDir dbFile

  -- Study "table"
  set wgDbTable [on listEvent := onDbTableEvent guiCtx]
  -- Series list
  set wgSeriesList [columns := [("Series",AlignLeft,400)],
                    on listEvent := onSeriesListEvent guiCtx]
  -- Display area
  textCtrlSetEditable wgText False

  -- Menus
  file <- menuPane [text := "&File"]
  importFiles <- menuItem file [text := "&Import...\tCtrl-I",
                                help := "Import images"]
  set importFiles [on command := onImport guiCtx]
  importFilesR <- menuItem file [text := "Import (&Recursive)...\tCtrl-R",
                                help := "Import images including sub-directories"]
  set importFilesR [on command := onImportRecurse guiCtx]
  openFile <- menuItem file [text := "&Open Image...\tCtrl-O",
                             help := "Open image"]
  set openFile [on command := onOpenFile guiCtx]
  menuLine file
  quit <- menuQuit file [help := "Quit"]
  set quit [on command := close wgFrame]

  set wgFrame [layout := fill $
                           hsplit wgHSplit 3 400 (widget wgDbTable)
                             (vsplit wgVSplit 3 400 (widget wgSeriesList) (widget wgText))]
  hasturIcon <- imageFile "blackmage.ico"
  set wgFrame [menuBar    := [file],
               statusBar  := [wgStatus],
               clientSize := sz width height]
  -- Kludgy but turns an IO Filepath into a String
  set wgFrame [picture := take (length hasturIcon) hasturIcon]
  splitterWindowSetSashPosition wgHSplit 300 True
  splitterWindowSetSashPosition wgVSplit 405 True
  
  dir <- getAppUserDataDirectory "hastur"
  varSet appDataDir dir
  createDirectoryIfMissing True dir
  initLog dir
  infoM "Hastur" "Hastur 0.1 startup"

  let dbPath = dir </> "hastur.db"
  varSet dbFile dbPath
  dbConn <- connectDb dbPath
  initDb dbConn
  showStudies dbConn studyIdMap wgDbTable
  closeDb dbConn

-- | This function takes a name and, with a little knowledge and the help of
--   cabal, returns the path of that image
imageFile :: String -> IO FilePath
imageFile img = getDataFileName $ "res" </> "icons" </> img

-- 
importDicomFile :: TextCtrl () -> FilePath -> IO ()
importDicomFile wgText path = do
  infoM "Hastur" $ "Reading DICOM file: " ++ path
  eitherDicom <- readDicomFile path
  textCtrlClear wgText
  case eitherDicom of
    Left errorMessage -> do
      infoM "Hastur" $ "Error reading DICOM file: " ++ path ++ " - " ++ errorMessage
      textCtrlAppendText wgText $ "\n*** DICOM: " ++ path ++ " ***\n"
      textCtrlAppendText wgText errorMessage
      textCtrlAppendText wgText "\n*** [End] ***\n"
      textCtrlShowPosition wgText 0
    Right dicom       -> do
      infoM "Hastur" $ "Displaying DICOM file: " ++ path
      textCtrlAppendText wgText $ "\n*** DICOM: " ++ path ++ " ***"
      textCtrlAppendText wgText (show dicom)
      textCtrlAppendText wgText "*** [End] ***\n"
      textCtrlShowPosition wgText 0

-- 
importDicomFileToDb :: IConnection conn => conn -> FilePath -> IO ()
importDicomFileToDb dbConn path = do
  eitherDicom <- readDicomFile path
  case eitherDicom of
    Left errorMessage -> infoM "Hastur" $ "Error: " ++ errorMessage ++
      " for file: " ++ path
    Right encapDicom  -> storeDicomFileToDb dbConn path $ dicom encapDicom

--
initLog :: FilePath -> IO ()
initLog dir = do
  updateGlobalLogger "Hastur" (setLevel INFO)
  let logDir = dir </> "log"
  createDirectoryIfMissing True logDir 
  handler <- fileHandler (logDir </> "hastur.log") DEBUG >>=
    \lh -> return $
      setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger rootLoggerName (setLevel DEBUG . addHandler handler)

--
noDots :: String -> Bool
noDots path = not $ (isPrefixOf "." path) || (isPrefixOf ".." path)

--
onDbTableEvent :: HasturContext -> EventList -> IO ()
onDbTableEvent HasturCtx {guiDbTable=wgDbTable, guiSeriesList=wgSeriesList, dbStudyMap=studyIdMap, dbSeriesMap=seriesIdMap, dbFile=dbf} event =
  case event of
    ListItemSelected idx -> do
      studyId <- listCtrlGetItemData wgDbTable idx
      idMap <- varGet studyIdMap
      let maybePk = Map.lookup studyId idMap
      case maybePk of
        Just studyPk -> do
          dbFile <- varGet dbf
          dbConn <- connectDb dbFile
          showSeries dbConn studyPk seriesIdMap wgSeriesList
          propagateEvent
        Nothing      -> propagateEvent
    otherwise            ->
      propagateEvent

--
onOpenFile :: HasturContext -> IO ()
onOpenFile HasturCtx {guiFrame=wgFrame, guiText=wgText} = do
  maybePath <- fileOpenDialog wgFrame True True "Open Image" [("Dicom files",["*.dcm"]),("All files",["*.*"])] "" ""
  case maybePath of
    Nothing   -> return ()
    Just path -> importDicomFile wgText path

--
onImport :: HasturContext -> IO ()
onImport HasturCtx{guiFrame=wgFrame, guiText=wgText, guiDbTable=wgDbTable, dbStudyMap=studyIdMap, dbFile=dbf} = do
  maybePath <- dirOpenDialog wgFrame False "" "E:\\User\\Plootarg\\DicomData\\MR\\DicomH"
  case maybePath of
    Nothing   -> return ()
    Just path -> do
      dbFile <- varGet dbf
      dbConn <- connectDb dbFile
      wxcBeginBusyCursor
      scanDirectory dbConn False path
      wxcEndBusyCursor
      showStudies dbConn studyIdMap wgDbTable
      disconnect dbConn
      infoM "Hastur" $ "Done scanning " ++ path

--
onImportRecurse :: HasturContext -> IO ()
onImportRecurse HasturCtx {guiFrame=wgFrame, guiText=wgText, guiDbTable=wgDbTable, dbStudyMap=studyIdMap, dbFile=db} = do
  maybePath <- dirOpenDialog wgFrame False "" "E:\\User\\Plootarg\\DicomData\\MR\\DicomH"
  case maybePath of
    Nothing   -> return ()
    Just path -> do
      dbFile <- varGet db
      dbConn <- connectDb dbFile
      wxcBeginBusyCursor
      scanDirectory dbConn True path
      wxcEndBusyCursor
      showStudies dbConn studyIdMap wgDbTable
      disconnect dbConn
      infoM "Hastur" $ "Done scanning " ++ path

--
onSeriesListEvent :: HasturContext -> EventList -> IO ()
onSeriesListEvent HasturCtx {guiSeriesList=wgSeriesList, dbSeriesMap=seriesIdMap, dbFile=dbf} event =
  case event of
    ListItemSelected idx -> do
      seriesId <- listCtrlGetItemData wgSeriesList idx
      idMap <- varGet seriesIdMap
      let maybePk = Map.lookup seriesId idMap
      case maybePk of
        Just seriesPk -> do
          dbFile <- varGet dbf
          dbConn <- connectDb dbFile
          infoM "Hastur" $ "Series Int64Id: " ++ show seriesPk
          propagateEvent
        Nothing      -> propagateEvent
    otherwise            ->
      propagateEvent

--
scanDirectory :: IConnection conn => conn -> Bool -> FilePath -> IO ()
scanDirectory dbConn recurse path = do
  infoM "Hastur" $ "Scanning " ++ path
  validContents <- liftM (filter noDots) $ getDirectoryContents path
  let fullPathContents = map (path </>) validContents
  files <- filterM doesFileExist fullPathContents
  mapM_ (importDicomFileToDb dbConn) files
  commit dbConn
  if not recurse
    then return ()
    else do
      dirs <- filterM doesDirectoryExist fullPathContents
      mapM_ (scanDirectory dbConn True) dirs
      return ()
  
--
showSeries :: IConnection conn => conn -> Int64 -> Var (ListDbMap) -> ListCtrl l -> IO ()
showSeries dbConn studyPk varMap wgSeriesList = do
  wxcBeginBusyCursor
  itemsDelete wgSeriesList
  series <- searchSeries dbConn studyPk
  let seriesPks = map seriesPk series
  let seriesIdMap = Map.fromList (zip [0..] seriesPks) :: ListDbMap
  varSet varMap seriesIdMap
  mapM_ (showSingleSeries wgSeriesList) $ zip (Map.keys seriesIdMap) series
  wxcEndBusyCursor
  return ()

--
showSingleSeries :: ListCtrl l -> (Int,DicomSeries) -> IO ()
showSingleSeries lc (seriesId,dcmSeries) = do
  count <- listCtrlGetItemCount lc
  idx <- listCtrlInsertItemWithLabel lc count 
    (show (seriesNumber dcmSeries) ++ " - " ++ seriesDescription dcmSeries) (-1)
  listCtrlSetItemData lc idx seriesId
  return ()

--
showStudies :: IConnection conn => conn -> Var (ListDbMap) -> ListCtrl l -> IO ()
showStudies dbConn varMap wgDbTable = do
  wxcBeginBusyCursor
  itemsDelete wgDbTable
  studies <- searchStudies dbConn
  let studyPks = map studyPk studies
  let studyIdMap = Map.fromList (zip [0..] studyPks) :: ListDbMap
  varSet varMap studyIdMap
  mapM_ (showStudy wgDbTable) $ zip (Map.keys studyIdMap) studies
  wxcEndBusyCursor
  return ()

--
showStudy :: ListCtrl l -> (Int,DicomStudy) -> IO ()
showStudy lc (studyId,dcmStudy) = do
  let values = [(patientName . studyPatient) dcmStudy,
                studyDescription dcmStudy,
                studyDate dcmStudy]
  count <- listCtrlGetItemCount lc
  idx <- listCtrlInsertItemWithLabel lc count (show count) (-1)
  mapM_ (\(column,columnText) -> listCtrlSetItem lc idx column columnText (-1))
    (zip [0..] values)
  listCtrlSetItemData lc idx studyId
  return ()

  