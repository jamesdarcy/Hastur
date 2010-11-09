module Hastur.DB
  (
    DicomPatient(..),
    DicomStudy(..),
    DicomSeries(..),
    DicomSopInstance(..),
    DicomImage(..),
    connectDb,
    initDb,
    closeDb,
    searchSeries,
    searchStudies,
    storeDicomFileToDb
  ) where

import Control.Monad (when)
import Data.Int
import qualified Data.Map as Map
import Database.HDBC.Sqlite3
import Database.HDBC
import System.FilePath
import System.Log.Logger

import Data.Dicom
import Data.Dicom.Accessor
import Data.Dicom.Tag

data DicomPatient = DicomPatient {
  patientName :: String,
  patientPk :: Int64
  } deriving (Show)

data DicomStudy = DicomStudy {
  studyUid :: String,
  studyDate :: String,
  studyDescription :: String,
  studyPk :: Int64,
  patientFk :: Int64,
  studyPatient :: DicomPatient
  } deriving (Show)

data DicomSeries = DicomSeries {
  seriesUid :: String,
  seriesDescription :: String,
  seriesNumber :: Int32,
  modality :: String,
  seriesPk :: Int64,
  studyFk :: Int64
  } deriving (Show)

data DicomSopInstance = DicomSopInstance {
  sopInstancePath :: String,
  sopInstanceUid :: String,
  sopInstancePk :: Int64,
  seriesFk :: Int64,
  sopInstanceFrameCount :: Int32
  } deriving (Show)

data DicomImage = DicomImage {
  imageUid :: String,
  sopInstanceFk :: Int64,
  imageFrame :: Int32
  } deriving (Show)

--------------------------------------------------------------------------------
--
connectDb :: FilePath -> IO Connection
connectDb dbFile = do
--  infoM "Hastur.DB" $ "Connecting to: " ++ dbFile
  conn <- connectSqlite3 dbFile
  return conn

--------------------------------------------------------------------------------
--
initDb :: IConnection conn => conn -> IO ()
initDb dbConn = do
  tables <- getTables dbConn

  run dbConn "CREATE TABLE IF NOT EXISTS patient (\
    \pk INTEGER PRIMARY KEY AUTOINCREMENT,\
    \name TEXT NOT NULL)" []
  run dbConn "CREATE INDEX IF NOT EXISTS patIdx1 ON patient(name)" []

  run dbConn "CREATE TABLE IF NOT EXISTS study (\
    \pk INTEGER PRIMARY KEY AUTOINCREMENT,\
    \patient_fk INTEGER DEFAULT 0,\
    \uid TEXT NOT NULL UNIQUE,\
    \desc TEXT NOT NULL,\
    \date TEXT NOT NULL)" []
  run dbConn "CREATE UNIQUE INDEX IF NOT EXISTS stuIdx1 ON study(uid)" []
  run dbConn "CREATE INDEX IF NOT EXISTS stuIdx2 ON study(date)" []
  run dbConn "CREATE INDEX IF NOT EXISTS stuIdx3 ON study(patient_fk)" []

  run dbConn "CREATE TABLE IF NOT EXISTS series (\
    \pk INTEGER PRIMARY KEY AUTOINCREMENT,\
    \study_fk INTEGER NOT NULL REFERENCES study(pk) ON DELETE CASCADE,\
    \uid TEXT NOT NULL UNIQUE,\
    \desc TEXT NOT NULL,\
    \number INTEGER NOT NULL,\
    \modality TEXT NOT NULL)" []
  run dbConn "CREATE UNIQUE INDEX IF NOT EXISTS serIdx1 ON series(uid)" []
  run dbConn "CREATE INDEX IF NOT EXISTS serIdx2 ON series(study_fk)" []
  run dbConn "CREATE INDEX IF NOT EXISTS serIdx3 ON series(modality)" []

  run dbConn "CREATE TABLE IF NOT EXISTS sopinstance (\
    \pk INTEGER PRIMARY KEY AUTOINCREMENT,\
    \series_fk INTEGER NOT NULL REFERENCES series(pk) ON DELETE CASCADE,\
    \uid TEXT NOT NULL UNIQUE,\
    \path TEXT NOT NULL,\
    \frameCount INTEGER NOT NULL)" []
  run dbConn "CREATE UNIQUE INDEX IF NOT EXISTS sopIdx1 ON sopinstance(uid)" []
  run dbConn "CREATE INDEX IF NOT EXISTS sopIdx2 ON sopinstance(series_fk)" []

  run dbConn "CREATE TABLE IF NOT EXISTS image (\
    \pk INTEGER PRIMARY KEY AUTOINCREMENT,\
    \sop_fk INTEGER NOT NULL REFERENCES sopinstance(pk) ON DELETE CASCADE,\
    \uid TEXT NOT NULL UNIQUE,\
    \frame INTEGER NOT NULL)" []
  run dbConn "CREATE UNIQUE INDEX IF NOT EXISTS imageIdx1 ON image(uid)" []
  run dbConn "CREATE INDEX IF NOT EXISTS imageIdx12 ON image(sop_fk)" []

  commit dbConn

--------------------------------------------------------------------------------
--
closeDb :: IConnection conn => conn -> IO ()
closeDb conn = disconnect conn

--------------------------------------------------------------------------------
--
getFrameCount :: DicomObject -> Int32
getFrameCount dcm =
  case getNumberOfFrames dcm of
    Just frameCount -> read frameCount
    _               -> 1

--------------------------------------------------------------------------------
--
getImageDb :: IConnection conn => conn -> String -> IO (Maybe DicomImage)
getImageDb conn uid = do
  image <- quickQuery' conn
    "SELECT uid,sop_fk,frame FROM image WHERE uid = ?"
    [toSql uid]
  case image of
    [i] -> return (Just (rowToImage i)) 
    []  -> return Nothing
    i   -> do
      emergencyM "Hastur.DB" $ "Duplicate image for UID: " ++ uid
      fail $ "Duplicate image for UID: " ++ uid

--------------------------------------------------------------------------------
--
getLastInsertId :: IConnection conn => conn -> IO (Int64)
getLastInsertId conn = do
  ids <- quickQuery' conn "SELECT last_insert_rowid()" []
  case ids of
    [s] -> return (fromSql $ head s) 
    _   -> do
      emergencyM "Hastur.DB" "getLastInsertId - Cannot retrieve most recent OID"
      fail "Cannot retrieve most recent OID"

--------------------------------------------------------------------------------
--
getPatientDb :: IConnection conn => conn -> Int64 -> IO (Maybe DicomPatient)
getPatientDb conn pk = do
  patient <- quickQuery' conn
    "SELECT name,pk FROM patient WHERE pk = ?"
    [toSql pk]
  case patient of
    [p] -> return (Just (rowToPatient p)) 
    []  -> return Nothing
    p   -> do
      emergencyM "Hastur.DB" $ "Patient fail: " ++ show pk
      fail $ "Patient fail: " ++ show pk

--------------------------------------------------------------------------------
--
getSeriesDb :: IConnection conn => conn -> String -> IO (Maybe DicomSeries)
getSeriesDb conn uid = do
  series <- quickQuery' conn
    "SELECT uid,desc,number,modality,pk,study_fk FROM series WHERE uid = ?"
    [toSql uid]
  case series of
    [s] -> return (Just (rowToSeries s)) 
    []  -> return Nothing
    s   -> do
      emergencyM "Hastur.DB" $ "Duplicate Series for UID: " ++ uid
      fail $ "Duplicate Series for UID: " ++ uid

--------------------------------------------------------------------------------
--
getSopInstanceDb :: IConnection conn => conn -> String -> IO (Maybe DicomSopInstance)
getSopInstanceDb conn uid = do
  sopInst <- quickQuery' conn
    "SELECT uid,path,pk,series_fk,frameCount FROM sopinstance WHERE uid = ?"
    [toSql uid]
  case sopInst of
    [s] -> return (Just (rowToSopInstance s)) 
    []  -> return Nothing
    s   -> do
      emergencyM "Hastur.DB" $ "Duplicate SOP instance for UID: " ++ uid
      fail $ "Duplicate SOP instance for UID: " ++ uid

--------------------------------------------------------------------------------
--
getStudyDb :: IConnection conn => conn -> String -> IO (Maybe DicomStudy)
getStudyDb conn uid = do
  studies <- quickQuery' conn
               "SELECT uid,date,desc,pk,patient_fk FROM study WHERE uid = ?"
               [toSql uid]
  case studies of
    [s] -> return (Just (rowToStudy s)) 
    []  -> return Nothing
    s   -> do
      emergencyM "Hastur.DB" $ "Duplicate Studies for UID: " ++ uid
      fail $ "Duplicate Studies for UID: " ++ uid

--------------------------------------------------------------------------------
--
imageFromDicom :: DicomObject -> Int32 -> Maybe DicomImage
imageFromDicom dcm frame =
  getSopInstanceUid dcm >>= \sopInstUid ->
  Just (DicomImage {imageUid=sopInstUid ++ "." ++ show frame, 
                    sopInstanceFk=0,
                    imageFrame=frame})

--------------------------------------------------------------------------------
--
insertImageDb :: IConnection conn => conn -> DicomObject -> Int64 -> Int32 -> IO (DicomImage)
insertImageDb conn dcm fk frame = do
  let maybeImage = imageFromDicom dcm frame
  case maybeImage of
    Just image -> do
      run conn "INSERT INTO image (uid,sop_fk,frame) values (?,?,?)"
        [toSql (imageUid image), toSql fk, toSql (imageFrame image)]
      maybeImage2 <- getImageDb conn $ imageUid image
      case maybeImage2 of
        Just newImage -> do
          debugM "Hastur.DB" $ "New image instance for UID: " ++ imageUid image
          return newImage
        Nothing       -> do
          emergencyM "Hastur.DB" $ "Can't retrieve image just inserted: " ++
            imageUid image
          fail $ "Can't retrieve image just inserted: " ++ imageUid image
    Nothing         -> do
      emergencyM "Hastur.DB" "Object has no SOP instance UID"
      fail "Object has no SOP instance UID"

--------------------------------------------------------------------------------
--
insertPatientDb :: IConnection conn => conn -> DicomPatient -> DicomStudy -> IO (DicomPatient)
insertPatientDb conn dcmPatient dcmStudy = do
  maybePatient <- getPatientDb conn $ patientFk dcmStudy
  case maybePatient of
    Just oldPatient -> return oldPatient
    Nothing        -> do
      run conn "INSERT INTO patient (name) values (?)"
        [toSql (patientName dcmPatient)]
      newPk <- getLastInsertId conn
      maybePatient2 <- getPatientDb conn newPk
      case maybePatient2 of
        Just newPatient -> do
          infoM "Hastur.DB" $ "New patient: " ++ patientName newPatient
          run conn "UPDATE study SET patient_fk=? where pk=?"
            [toSql (patientPk newPatient),toSql (studyPk dcmStudy)]
          return newPatient
        Nothing         -> do
          emergencyM "Hastur.DB" $ "Can't retrieve patient just inserted: " ++ 
            patientName dcmPatient
          fail $ "Can't retrieve patient just inserted: " ++ 
            patientName dcmPatient

--------------------------------------------------------------------------------
--
insertSeriesDb :: IConnection conn => conn -> DicomSeries -> Int64 -> IO (DicomSeries)
insertSeriesDb conn dcmSeries fk = do
  maybeSeries <- getSeriesDb conn $ seriesUid dcmSeries
  case maybeSeries of
    Just oldSeries -> return oldSeries
    Nothing        -> do
      run conn "INSERT INTO series (uid,desc,number,modality,study_fk) values (?,?,?,?,?)"
        [toSql (seriesUid dcmSeries), toSql (seriesDescription dcmSeries),
        toSql (seriesNumber dcmSeries), toSql (modality dcmSeries),
        toSql fk]
      maybeSeries2 <- getSeriesDb conn $ seriesUid dcmSeries
      case maybeSeries2 of
        Just newSeries -> do
          debugM "Hastur.DB" $ "New series for UID: " ++ (seriesUid dcmSeries)
          return newSeries
        Nothing       -> do
          emergencyM "Hastur.DB" $ "Can't retrieve series just inserted: " ++ 
            seriesUid dcmSeries
          fail $ "Can't retrieve series just inserted: " ++ 
            seriesUid dcmSeries

--------------------------------------------------------------------------------
--
insertSopInstanceDb :: IConnection conn => conn -> FilePath -> DicomObject -> Int64 -> IO (DicomSopInstance)
insertSopInstanceDb conn path dcm fk = do
  let maybeUid = getSopInstanceUid dcm
  case maybeUid of
    Nothing         -> do
      emergencyM "Hastur.DB" "Object has no SOP instance UID"
      fail "Object has no SOP instance UID"
    Just sopInstUid -> do
      maybeSopInst <- getSopInstanceDb conn sopInstUid
      case maybeSopInst of
        Just oldSopInst -> return oldSopInst
        Nothing        -> do
          run conn "INSERT INTO sopinstance (uid,path,series_fk,frameCount) values (?,?,?,?)"
            [toSql sopInstUid, toSql path, toSql fk, toSql (getFrameCount dcm)]
          maybeSopInst2 <- getSopInstanceDb conn sopInstUid
          case maybeSopInst2 of
            Just newSopInst -> do
              debugM "Hastur.DB" $ "New SOP instance for UID: " ++ sopInstUid
              return newSopInst
            Nothing       -> do
              emergencyM "Hastur.DB" $ "Can't retrieve SOP instance just inserted: " ++ 
                sopInstUid
              fail $ "Can't retrieve SOP instance just inserted: " ++ 
                sopInstUid

--------------------------------------------------------------------------------
--
insertStudyDb :: IConnection conn => conn -> DicomStudy -> IO (DicomStudy)
insertStudyDb conn dcmStudy = do
  maybeStudy <- getStudyDb conn $ studyUid dcmStudy
  case maybeStudy of
    Just oldStudy -> return oldStudy
    Nothing       -> do
      run conn "INSERT INTO study (uid,date,desc) values (?,?,?)"
        [toSql (studyUid dcmStudy),toSql (studyDate dcmStudy),
        toSql (studyDescription dcmStudy)]
      maybeStudy2 <- getStudyDb conn $ studyUid dcmStudy
      case maybeStudy2 of
        Just newStudy -> do
          infoM "Hastur.DB" $ "New study for UID: " ++ (studyUid dcmStudy)
          return newStudy
        Nothing       -> do
          emergencyM "Hastur.DB" $ "Can't retrieve study just inserted: " ++ 
            studyUid dcmStudy
          fail $ "Can't retrieve study just inserted: " ++ 
            studyUid dcmStudy

--------------------------------------------------------------------------------
--
nullPatient :: DicomPatient
nullPatient = DicomPatient { patientName="NULL", patientPk=0}

--------------------------------------------------------------------------------
--
patientFromDicom :: DicomObject -> Maybe DicomPatient
patientFromDicom dcm =
  getPatientName dcm >>= \name ->
  Just (DicomPatient {patientName=name, patientPk=0})

--------------------------------------------------------------------------------
--
rowToImage :: [SqlValue] -> DicomImage
rowToImage [svUid,svFk,svFrame] =
  DicomImage {
    imageUid = fromSql svUid,
    sopInstanceFk = fromSql svFk,
    imageFrame = fromSql svFrame}
rowToImage x = error $ "Cannot convert row to valid image"

--------------------------------------------------------------------------------
--
rowToPatient :: [SqlValue] -> DicomPatient
rowToPatient [svName,svPk] =
  DicomPatient {
    patientName = fromSql svName,
    patientPk = fromSql svPk}
rowToPatient x = error $ "Cannot convert row to valid patient"

--------------------------------------------------------------------------------
--
rowToSeries :: [SqlValue] -> DicomSeries
rowToSeries [svUid,svDesc,svNumber,svModality,svPk,svFk] =
  DicomSeries {
    seriesUid = fromSql svUid,
    seriesDescription = fromSql svDesc,
    seriesNumber = fromSql svNumber,
    modality = fromSql svModality,
    seriesPk = fromSql svPk,
    studyFk = fromSql svFk}
rowToSeries x = error $ "Cannot convert row to valid series"

--------------------------------------------------------------------------------
--
rowToSopInstance :: [SqlValue] -> DicomSopInstance
rowToSopInstance [svUid,svPath,svPk,svFk,svFrameCount] =
  DicomSopInstance {
    sopInstanceUid = fromSql svUid,
    sopInstancePath = fromSql svPath,
    sopInstancePk = fromSql svPk,
    seriesFk = fromSql svFk,
    sopInstanceFrameCount = fromSql svFrameCount}
rowToSopInstance x = error $ "Cannot convert row to valid SOP instance"

--------------------------------------------------------------------------------
--
rowToStudy :: [SqlValue] -> DicomStudy
rowToStudy [svUid,svDate,svDesc,svPk,svFk] =
  DicomStudy {
    studyUid = fromSql svUid,
    studyDate = fromSql svDate,
    studyDescription = fromSql svDesc,
    studyPk = fromSql svPk,
    patientFk = fromSql svFk,
    studyPatient = nullPatient}
rowToStudy [svUid,svDate,svDesc,svPk,svFk,svName] =
  DicomStudy {
    studyUid = fromSql svUid,
    studyDate = fromSql svDate,
    studyDescription = fromSql svDesc,
    studyPk = fromSql svPk,
    patientFk = fromSql svFk,
    studyPatient = 
      DicomPatient {
        patientName = fromSql svName,
        patientPk = fromSql svFk}}
rowToStudy x = error $ "Cannot convert row to valid study"

--------------------------------------------------------------------------------
--
searchSeries :: IConnection conn => conn -> Int64 -> IO ([DicomSeries])
searchSeries conn studyFk = do
  series <- quickQuery' conn
    "SELECT uid,desc,number,modality,pk,study_fk FROM series WHERE study_fk=? \
    \ORDER BY number"
      [toSql studyFk]
  return (map rowToSeries series)

--------------------------------------------------------------------------------
--
searchStudies :: IConnection conn => conn -> IO ([DicomStudy])
searchStudies conn = do
  studies <- quickQuery' conn
    "SELECT study.uid,study.date,study.desc,study.pk,study.patient_fk,patient.name \
    \FROM study,patient WHERE patient.pk=study.patient_fk" []
  return (map rowToStudy studies)

--------------------------------------------------------------------------------
--
seriesFromDicom :: DicomObject -> Maybe DicomSeries
seriesFromDicom dcm =
  getSeriesInstanceUid dcm >>= \uid ->
  (case (getSeriesDescription dcm) of
    Nothing -> Just ("")
    Just desc -> Just desc) >>= \desc ->
  getSeriesNumber dcm >>= \num ->
  getModality dcm >>= \m ->
  Just (DicomSeries {seriesUid=uid, seriesDescription=desc,
    seriesNumber=(read num), modality=m, seriesPk=0, studyFk=0})

--------------------------------------------------------------------------------
--
studyFromDicom :: DicomObject -> Maybe DicomStudy
studyFromDicom dcm =
  getStudyInstanceUid dcm >>= \uid ->
  getStudyDate dcm >>= \date ->
  case (getStudyDescription dcm) of
    Nothing -> Just (DicomStudy {studyUid=uid, studyDate=date,
      studyDescription="", studyPk=0, patientFk=0, studyPatient=nullPatient})
    Just desc -> Just (DicomStudy {studyUid=uid, studyDate=date,
      studyDescription=desc, studyPk=0, patientFk=0, studyPatient=nullPatient})

--
storeDicomFileToDb :: IConnection conn => conn -> FilePath -> DicomObject -> IO ()
storeDicomFileToDb dbConn path dcm = do
  let maybeStudy = studyFromDicom dcm
  case maybeStudy of
    Nothing -> return ()
    Just rawStudy -> do
      dcmStudy <- insertStudyDb dbConn rawStudy
      let maybePatient = patientFromDicom dcm
      case maybePatient of
        Nothing -> return ()
        Just rawPatient -> do
          dcmPatient <- insertPatientDb dbConn rawPatient dcmStudy
          let maybeSeries = seriesFromDicom dcm
          case maybeSeries of
            Nothing -> return ()
            Just rawSeries -> do
              dcmSeries <- insertSeriesDb dbConn rawSeries $ studyPk dcmStudy
              storeSopInstanceToDb dbConn path dcm dcmSeries
              return ()
  
--
storeSopInstanceToDb :: IConnection conn => conn -> FilePath -> DicomObject -> DicomSeries -> IO ()
storeSopInstanceToDb dbConn path dcm dcmSeries = do
  sopInst <- insertSopInstanceDb dbConn path dcm $ seriesPk dcmSeries
  let frameCount = sopInstanceFrameCount sopInst
  -- Store one image per frame of sop instance
  mapM_ (insertImageDb dbConn dcm (seriesPk dcmSeries)) [1..frameCount]
  return ()
  