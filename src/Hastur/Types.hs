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

module Hastur.Types
  (
    DicomPatient(..),
    DicomStudy(..),
    DicomSeries(..),
    DicomSopInstance(..),
    DicomImage(..)
  ) where

import Data.Int
import Graphics.UI.WX (Var)

import Data.Dicom

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
  sopInstanceFrameCount :: Int32,
  varDicom :: Var (Maybe EncapDicomObject)
  }

data DicomImage = DicomImage {
  imageUid :: String,
  sopInstanceFk :: Int64,
  sopInstance :: DicomSopInstance,
  imageFrame :: Int32
  } deriving (Show)

instance Show DicomSopInstance where
  show dsi = "DicomSopInstance { UID=\"" ++ (sopInstanceUid dsi) ++
    "\", Key=\"" ++ show (sopInstancePk dsi) ++ "\", Series Key=\"" ++
    show (seriesFk dsi) ++ "\", Frame Count=\"" ++ show (sopInstanceFrameCount dsi) ++
    "\", Path=\"" ++ (sopInstancePath dsi) ++ "\" }"

  