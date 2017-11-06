--  File    : Proj1.hs
--  Author  : Kai Zhang
--  LMS Id  : kaiz2
--  Purpose : Performer program for guessing game ChordProbe.
--  Detail  : The program makes an initial guess and calculates the next guess
--            based on current game state. It selects the next guess having
--            the most similarity with remaining guesses to leave a smallest
--            remaining list.
--            When measuring the similarity, it compares each guess with
--            remaining guesses and convert the response into a customized
--            similarity score.
--            ** Similarity Score ** = rightPitch + rightNote + rightOctave

module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List
import Data.Ord

--  | A three-pitch musical chord. Its length should be three.
--    Each element should be a pitch with a note (A-G) and a octave (1-3).
type Chord = [String]


--  | Current remaining possible guesses and their similarity scores. Pair
--    them together so finding the guess with maximum similarity score is easy.
type GameState = [(Chord, Int)]


--  | A feedback for a guess of the target. Elements are numbers of correct
--    pitches, notes and octaves respectively.
type Feedback = (Int, Int, Int)


--  | Make an initial guess. It takes no input. The output is a pair of
--    the first guess (A1, B2, C3) and an initial GameState.
initialGuess :: (Chord, GameState)
initialGuess = (["A1", "B2", "C3"], initGameState)


--  | Initialise a GameState. It takes no input. The output is a GameState
--    with all possible guesses and initial similarity scores (zero).
initGameState :: GameState
initGameState = [ ((p1:p2:p3:[]), initialScore) | p1 <- chords, p2 <- chords,
  p3 <- chords, p1 > p2, p2 > p3]
  where notes        = ['A'..'G']
        octaves      = ['1'..'3']
        chords       = [ (n:o:[]) | n<-notes, o<-octaves]
        initialScore = 0


-- | Make the next guess. The first argument is a pair previous guess and
--   previous GameState. The second argument is the feedback for previous
--   guess. The output is a pair of the next guess and new GameState. This
--   function filter impossible guesses based on feedback then select a guess
--   with the most similarity score as the next guess.
nextGuess :: (Chord, GameState) -> Feedback -> (Chord, GameState)
nextGuess (preGus,preGS) preFB = (newGuess, newGameState)
  where newGuess     = fst $ maximumBy (comparing snd)
                       [((fst x), (calScore (fst x) newGameState))
                       | x <- newGameState ]
        newGameState = [((fst gus), initialScore)
                       | gus <- preGS, (response preGus (fst gus)) == preFB]
        initialScore = 0


-- | Compute the response to a guess. The first argument is the target, the
--   second argument is the guess. The output is a tuple with numbers of
--   correct pitchs, notes and octaves.
response :: Chord -> Chord -> Feedback
response target guess = (pitch, note, octave)
  where pitch        = length (intersect guess target)
        note         = num - length (targetNote \\ guessNote) - pitch
        octave       = num - length (targetOctave \\ guessOctave) - pitch
        num          = length guess
        guessNote    = map (\x -> [x !! 0]) guess
        targetNote   = map (\x -> [x !! 0]) target
        guessOctave  = map (\x -> [x !! 1]) guess
        targetOctave = map (\x -> [x !! 1]) target


-- | Calculate similarity score for a given guess with remaining guesses. The
--   first argument is a guess and the second argument is a GameState with
--   current remaining guesses. The output is the total similarity score for
--   this guess.
calScore :: Chord -> GameState -> Int
calScore guess [] = 0
calScore guess (x:xs) = similarityScore + (calScore guess xs)
  where similarityScore = rightPitch + rightNote + rightOctave
        (rightPitch,
         rightNote,
         rightOctave)   = response guess (fst x)
