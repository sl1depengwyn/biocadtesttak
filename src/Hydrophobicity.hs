{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hydrophobicity (run) where

import Bio.PDB.Reader (fromFilePDB)
import Bio.PDB.Type (Atom (..), PDB (models))
import Data.List (groupBy, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Linear.Metric (distance)
import Linear.V3 (V3 (..))

run :: FilePath -> IO ()
run path = do
  mbPdb <- fromFilePDB path
  case mbPdb of
    Left txt -> putStrLn (T.unpack txt)
    Right (wrns, pdb) -> do
      if V.length (models pdb) < 1
        then putStrLn "ERROR: No models in PDB"
        else
          putStrLn $
          prettyReport $
          groupBy
          (\x y -> x `distance` y < distanceBetweenAA) $
          sort (V.toList (V.concatMap (V.map pointOfAtom . casHydrophobicOnTheSurface) (V.head (models pdb))))

distanceBetweenAA :: Float
distanceBetweenAA = 4

prettyReport :: [[V3 Float]] -> String
prettyReport areas = "----------" <> unlines (map printArea areas)
  where
    printArea area = "x\ty\tz\n" <> unlines (map printEntry area) <> "----------"
    printEntry (V3 x y z) = mconcat [show x, "\t", show y, "\t", show z, "\n"]

-- Temperature factors are a measure of our confidence in the location of each atom.
-- https://pdb101.rcsb.org/learn/guide-to-understanding-pdb-data/dealing-with-coordinates
isOnTheSurface :: Atom -> Bool
isOnTheSurface Atom {atomTempFactor} = atomTempFactor > 40

hydrophobic :: [Text]
hydrophobic = ["GLY", "VAL", "LEU", "ILE", "PRO", "MET", "PHE", "ALA", "TRP"]

isHydrophobic :: Atom -> Bool
isHydrophobic Atom {atomResName} = atomResName `elem` hydrophobic

isCenterOfAA :: Atom -> Bool
isCenterOfAA Atom {atomName} = atomName == " CA "

casHydrophobicOnTheSurface :: Vector Atom -> Vector Atom
casHydrophobicOnTheSurface = V.filter (\a -> isOnTheSurface a && isHydrophobic a && isCenterOfAA a)

pointOfAtom :: Atom -> V3 Float
pointOfAtom Atom {..} = V3 atomX atomY atomZ
