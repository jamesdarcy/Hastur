{-
Copyright James d'Arcy 2011

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


module Hastur.Image
  ( createWxImage,
    isRenderable
  ) where

import qualified Data.ByteString as B
import qualified Data.Set as Set
import Data.Word
import Graphics.UI.WX
import Graphics.UI.WXCore

import Data.Dicom
import Data.Dicom.Accessor
import Data.Dicom.Tag
import Data.Dicom.UID

type UidSet = Set.Set UID

--
createWxImage :: DicomObject -> Maybe (IO (Image ()))
createWxImage sopInst =
  getColumns sopInst >>= \nX ->
  getRows sopInst >>= \nY ->
  getPixelData sopInst >>= \pixelData ->
  Just (wxImageFromPixels nX nY pixelData)

--
isRenderable :: DicomObject -> Bool
isRenderable dcm = do
  let maybeUid = getSopClassUid dcm
  case maybeUid of
    Nothing  -> False
    Just uid -> Set.member uid renderableSopClassUidSet

--
renderableSopClassUidSet :: UidSet
renderableSopClassUidSet =
  Set.insert cT_IMAGE_STORAGE .
  Set.insert mR_IMAGE_STORAGE .
  Set.insert pOSITRON_EMISSION_TOMOGRAPHY_IMAGE_STORAGE .
  Set.insert uLTRASOUND_IMAGE_STORAGE
  $ Set.empty

--
wxImageFromPixels :: Word16 -> Word16 -> B.ByteString -> IO (Image ())
wxImageFromPixels nX nY pixels = do
  let rgbImage = map (\x -> rgb x x x) (extractInt16s pixels)
  imageCreateFromPixels (sz (fromIntegral nX) (fromIntegral nY)) rgbImage

