{-
Copyright James d'Arcy 2010

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of James d'Arcy nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

module Main
  ( main
  ) where

import Control.Monad
import Data.Array
import Data.Bits
import Data.Int
import Data.Word
import qualified Data.Map as Map
import qualified Data.ByteString as B
import Database.HDBC
import Graphics.UI.WX
import Graphics.UI.WXCore
import List (isPrefixOf)
import System.Exit
import System.FilePath
import System.Directory
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

import Data.Dicom
import Data.Dicom.Accessor
import Data.Dicom.Io
import Data.Dicom.Show
import Data.Dicom.Tag

import Hastur.DB
import Paths_hastur

type ListDbMap = Map.Map Int Int64
type ImageArray = Array Int DicomImage

data HasturContext = HasturContext {
  guiWidgets :: HasturWidgets,
  dbStudyMap :: Var (ListDbMap),
  dbSeriesMap :: Var (ListDbMap),
  imageArray :: Var (ImageArray),
  appDataDir :: Var (FilePath),
  dbConn :: Var (ConnWrapper)
}

data HasturWidgets = HasturWidgets {
  guiFrame :: Frame (),
  guiDbTable :: ListCtrl (),
  guiSeriesList :: ListCtrl (),
  guiImageSlider :: Slider (),
  guiText :: TextCtrl (),
  guiImage :: ScrolledWindow (),
  guiStatus :: StatusField
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
                    clientSize := sz width height,
                    visible := False]
  wgHSplit <- splitterWindow wgFrame []
  wgStatus <- statusField [text := ""]
  wgDbTable <- listCtrl wgHSplit [
    columns := [("Patient",AlignLeft,250),
                ("Study",AlignLeft,250),
                ("Date",AlignRight,-1)],
    style := wxLC_SINGLE_SEL .|. wxLC_REPORT .|. wxLC_HRULES]
  wgVSplit <- splitterWindow wgHSplit []
  wgSeriesList <- listCtrl wgVSplit []
  dispPanel <- panel wgVSplit []
  noteBook <- notebook dispPanel []
  sliderPanel <- panel dispPanel []
  imageSlider <- hslider sliderPanel False 0 0 [style := wxSL_AUTOTICKS]
  rawTab <- panel noteBook []
  imageTab <- panel noteBook []
  wgText <- textCtrl rawTab []
  wgImage <- scrolledWindow imageTab [bgcolor := white, fullRepaintOnResize := False]
  studyIdMap <- varCreate Map.empty
  seriesIdMap <- varCreate Map.empty
  imageArray <- varCreate $ listArray (0,0) []

  dir <- getAppUserDataDirectory "hastur"
  appDataDir <- varCreate dir
  createDirectoryIfMissing True dir
  initLog dir
  infoM "Hastur" "Hastur startup"
  conn <- connectDb $ dir </> "hastur.db"
  initDb conn
  dbConn <- varCreate $ ConnWrapper conn

  let widgets = HasturWidgets wgFrame wgDbTable wgSeriesList imageSlider wgText wgImage wgStatus
  let guiCtx = HasturContext widgets studyIdMap seriesIdMap imageArray appDataDir dbConn

  -- Study "table"
  set wgDbTable [on listEvent := onDbTableEvent guiCtx]
  -- Series list
  set wgSeriesList [columns := [("Series",AlignLeft,400)],
                    on listEvent := onSeriesListEvent guiCtx]
  -- Image slider
  set imageSlider [on command := onImageSlider guiCtx,
                   enabled := False]

  -- Display area
  textCtrlSetEditable wgText False
  set wgImage [on paint := onImagePaint guiCtx]

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

  set wgFrame [layout :=
                 fill $ hsplit wgHSplit 3 400 (widget wgDbTable)
                   (vsplit wgVSplit 3 400 (widget wgSeriesList) $
                     container dispPanel $ column 0
                     [ tabs noteBook $
                       [ tab "Raw" $ container rawTab $ fill (widget wgText),
                         tab "Image" $ container imageTab $ fill (widget wgImage)
                       ],
                       hfill $ minsize (sz 20 40) $ container sliderPanel $ 
                         hfill $ widget imageSlider
                     ]
                   )
              ]
  hasturIcon <- imageFile "blackmage.ico"
  set wgFrame [menuBar    := [file],
               statusBar  := [wgStatus],
               clientSize := sz width height]
  -- Kludgy but turns an IO Filepath into a String
  set wgFrame [picture := take (length hasturIcon) hasturIcon]
  splitterWindowSetSashPosition wgHSplit 300 True
  splitterWindowSetSashPosition wgVSplit 405 True

  set wgFrame [on closing := onClose guiCtx,
               visible := True]

  showStudies conn studyIdMap wgDbTable
  
-- 
clearSeriesSelection :: TextCtrl () -> Slider () -> IO ()
clearSeriesSelection wgText imageSlider = do
  textCtrlClear wgText
  sliderSetRange imageSlider 0 0
  set imageSlider [enabled := False]

-- This function takes a name and, with a little knowledge and the help of
-- cabal, returns the path of that image
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
onClose :: HasturContext -> IO ()
onClose HasturContext {dbConn=dbc} = do
  dbConn <- varGet dbc
  closeDb dbConn
  infoM "Hastur" "Hastur Shutdown"
  exitSuccess

--
onDbTableEvent :: HasturContext -> EventList -> IO ()
onDbTableEvent HasturContext {guiWidgets=hxw, dbStudyMap=studyIdMap, dbSeriesMap=seriesIdMap, dbConn=dbc} event =
  case event of
    ListItemSelected idx -> do
      clearSeriesSelection (guiText hxw) (guiImageSlider hxw)
      studyId <- listCtrlGetItemData (guiDbTable hxw) idx
      idMap <- varGet studyIdMap
      let maybePk = Map.lookup studyId idMap
      case maybePk of
        Just studyPk -> do
          dbConn <- varGet dbc
          showSeries dbConn studyPk seriesIdMap (guiSeriesList hxw)
          propagateEvent
        Nothing      -> propagateEvent
    otherwise        -> propagateEvent

--
onOpenFile :: HasturContext -> IO ()
onOpenFile HasturContext {guiWidgets=hxw} = do
  maybePath <- fileOpenDialog (guiFrame hxw) True True "Open Image" [("Dicom files",["*.dcm"]),("All files",["*.*"])] "" ""
  case maybePath of
    Nothing   -> return ()
    Just path -> importDicomFile (guiText hxw) path

--
onImagePaint :: HasturContext -> DC () -> Rect -> IO ()
onImagePaint HasturContext {guiWidgets=hxw, imageArray=ia} dc viewArea = do
  idx <- sliderGetValue $ guiImageSlider hxw
  imageArray <- varGet ia
  showImagePixels dc (imageArray ! idx)
  return ()

--
onImageSlider :: HasturContext -> IO ()
onImageSlider HasturContext {guiWidgets=hxw, imageArray=ia} = do
  idx <- sliderGetValue $ guiImageSlider hxw
  imageArray <- varGet ia
  showImage (guiText hxw) $ imageArray ! idx
  repaint (guiImage hxw)
  return ()

--
onImport :: HasturContext -> IO ()
onImport HasturContext {guiWidgets=hxw, dbStudyMap=studyIdMap, dbConn=dbc} = do
  maybePath <- dirOpenDialog (guiFrame hxw) False "" "E:\\User\\Plootarg\\DicomData\\MR\\DicomH"
  case maybePath of
    Nothing   -> return ()
    Just path -> do
      dbConn <- varGet dbc
      wxcBeginBusyCursor
      scanDirectory dbConn False path
      wxcEndBusyCursor
      showStudies dbConn studyIdMap $ guiDbTable hxw
      infoM "Hastur" $ "Done scanning " ++ path

--
onImportRecurse :: HasturContext -> IO ()
onImportRecurse HasturContext {guiWidgets=hxw, dbStudyMap=studyIdMap, dbConn=dbc} = do
  maybePath <- dirOpenDialog (guiFrame hxw) False "" "E:\\User\\Plootarg\\DicomData\\MR\\DicomH"
  case maybePath of
    Nothing   -> return ()
    Just path -> do
      dbConn <- varGet dbc
      wxcBeginBusyCursor
      scanDirectory dbConn True path
      wxcEndBusyCursor
      showStudies dbConn studyIdMap $ guiDbTable hxw
      infoM "Hastur" $ "Done scanning " ++ path

--
onSeriesListEvent :: HasturContext -> EventList -> IO ()
onSeriesListEvent HasturContext {guiWidgets=hxw, dbSeriesMap=seriesIdMap, imageArray=ia, dbConn=dbc} event =
  case event of
    ListItemSelected idx -> do
      seriesId <- listCtrlGetItemData (guiSeriesList hxw) idx
      idMap <- varGet seriesIdMap
      let maybePk = Map.lookup seriesId idMap
      case maybePk of
        Just seriesPk -> do
          dbConn <- varGet dbc
          images <- fetchImages dbConn seriesPk
          let nImages = length images
          varSet ia $ listArray (0,nImages-1) images
          let imageSlider = guiImageSlider hxw
          sliderSetRange imageSlider 0 (nImages-1)
          sliderSetValue imageSlider 0
          set imageSlider [enabled := True]
          showImage (guiText hxw) $ head images
          repaint (guiImage hxw)
          propagateEvent
        Nothing      -> propagateEvent
    otherwise        -> propagateEvent

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
fetchImages :: IConnection conn => conn -> Int64 -> IO ([DicomImage])
fetchImages dbConn seriesPk = do
  wxcBeginBusyCursor
  images <- searchImages dbConn seriesPk
  wxcEndBusyCursor
  return images

--
showEncapDicomObject :: TextCtrl t -> EncapDicomObject -> FilePath -> IO ()
showEncapDicomObject textCtl dicom path = do
  textCtrlSetValue textCtl $ "*** DICOM: " ++ path ++ " ***"
  textCtrlAppendText textCtl (show dicom)
  textCtrlAppendText textCtl "*** [End] ***"
  textCtrlShowPosition textCtl 0

--
showImage :: TextCtrl t -> DicomImage -> IO ()
showImage textCtl image = do
  let sopInst = sopInstance image
  maybeEncapDicom <- varGet $ varDicom sopInst
  case maybeEncapDicom of
    Just encapDicom -> do
      showEncapDicomObject textCtl encapDicom (sopInstancePath sopInst)
      return ()
    Nothing         -> do
      eitherDicom <- readDicomFile $ sopInstancePath sopInst 
      case eitherDicom of
        Left errorMessage -> do
          infoM "Hastur" $ "Error reading DICOM file: " ++ 
            (sopInstancePath sopInst) ++ " - " ++ errorMessage
          textCtrlSetValue textCtl $ "*** DICOM: " ++
            (sopInstancePath sopInst) ++ " ***\n"
          textCtrlAppendText textCtl errorMessage
          textCtrlAppendText textCtl "\n*** [End] ***"
          textCtrlShowPosition textCtl 0
          return ()
        Right encapDicom  -> do
          varSet (varDicom sopInst) (Just encapDicom)
          showEncapDicomObject textCtl encapDicom (sopInstancePath sopInst)
          return ()

--
createWxImage :: DicomObject -> Maybe (IO (Image ()))
createWxImage sopInst =
  getColumns sopInst >>= \nX ->
  getRows sopInst >>= \nY ->
  getPixelData sopInst >>= \pixelData ->
  Just (wxImageFromPixels nX nY pixelData)

--
wxImageFromPixels :: Word16 -> Word16 -> B.ByteString -> IO (Image ())
wxImageFromPixels nX nY pixels = do
  let rgbImage = map (\x -> rgb x x x) (extractInt16s pixels)
  imageCreateFromPixels (sz (fromIntegral nX) (fromIntegral nY)) rgbImage

--
showImagePixels :: DC () -> DicomImage -> IO ()
showImagePixels dc image = do
  let sopInst = sopInstance image
  maybeEncapDicom <- varGet $ varDicom sopInst
  case maybeEncapDicom of
    Nothing         -> return ()
    Just encapDicom -> do
      let maybeWxImage = createWxImage $ dicom encapDicom
      case maybeWxImage of
        Nothing      -> return ()
        Just ioWxImage -> do
          wxImage <- ioWxImage
          drawImage dc wxImage pointZero []
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

  