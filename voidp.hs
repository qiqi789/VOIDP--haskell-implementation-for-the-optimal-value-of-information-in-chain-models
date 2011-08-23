import Numeric.LinearAlgebra

import Dynamic

import System.Environment

-- compile: ghc --make -O2 voidp.hs -rtsopts -optl -llapack -lblas -fforce-recomp
-- run: voidp +RTS -Ksize 
-- run: voidp +RTS -H    ; make it run a little faster

disp :: Matrix Double -> IO ( )
disp = putStr . dispf 6

dispv :: Vector Double -> IO ( )
dispv = putStr . vecdisp (dispf 4)


qt :: Int -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double
qt n prior transmat obsmat
    | n==1 = prior
    | otherwise = fromColumns $ [fromList ( map (foldVector (+) 0.0) ( toColumns qtM) )]
                  where qtM = qt' <> ((1><4) [1,1,1,1::Double]) * transmat             
                        qt' = qt (n-1) prior transmat obsmat

mQT :: Int -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double
mQT n prior transmat obsmat = fromBlocks [[qt i1 prior transmat obsmat | i1<-[1..n]]]


ot :: Int -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double
ot n prior transmat obsmat  = fromColumns $ [fromList ( map (foldVector (+) 0.0) ( toColumns otM) )]
                                 where otM = ot' <> ones * obsmat             
                                       ot' = qt n prior transmat obsmat
                                       ones = repmat (ident 1) 1 10

mOT :: Int -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double
mOT n prior transmat obsmat = fromBlocks [[ot i2 prior transmat obsmat | i2<-[1..n]]]


entropy :: Vector Double -> Double
entropy v = vec' <.> logvec'
              where vec' = fromList $ filter (>0) (toList v)
                    logvec' = mapVector (logBase 2) vec'

rew_base_ary :: Int -> Matrix Double -> Matrix Double -> Matrix Double -> Vector Double
rew_base_ary n prior transmat obsmat = fromList $ [entropy v | v <- toColumns (mOT n prior transmat obsmat)]
           
                                       
exp_rew :: Vector Double -> Matrix Double -> Vector Double -> Double
exp_rew vq obsmat vo = sumElements $ zipVectorWith (*) vqo vo'
                       where vqm = ( fromColumns [vq] <> (repmat (ident 1) 1 (cols obsmat ) ) ) * obsmat
                             idx = find (>0) vo
                             vo' = fromList $ map (atIndex vo) idx
                             vqm' = trans (extractRows idx $ trans vqm)
                             vo'' = mapVector recip vo'
                             vo''m = (repmat (ident 1) (dim vq )  1) <> (fromRows [vo''])
                             mqo = vqm'  * vo''m
                             vqo = fromList $ [entropy v' | v <- toColumns mqo, let vm =  (fromColumns [v]) <> (repmat (ident 1) 1 (cols obsmat) ) * obsmat, let v' = fromList $ (map sumElements (toColumns vm)) ]
                             
            
exp_rew_ary :: Int -> Matrix Double -> Matrix Double -> Matrix Double -> Vector Double
exp_rew_ary n mq obsmat mo = fromList $ [exp_rew (vqlist!!ii) obsmat (volist!!ii)  | ii<-[0..n-1], let vqlist=toColumns mq, let volist =toColumns mo] 


type Entry = (Double,Int)
type Coord = (Int,Int,Int)


onePoint :: Int -> Int -> Int -> Int -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Vector Double
onePoint a b k n mot mqt obsmat transmat  | n == a+1 = qt1' / otvec
                                          | otherwise = (qt2' . qt2) $ (onePoint a b k (n-1) mot mqt obsmat transmat)
                                            where  qt1 = (asColumn ((toColumns mqt) !! (a-1)) ) <> (asRow $ constant 1.0 4) 
                                                   ok = (asColumn ((toColumns obsmat) !! (k-1)) ) <>  (asRow $ constant 1.0 4)
                                                   qt1' = fromList (map sumElements ( toColumns (qt1 * ok * transmat)))
                                                   otvec =  if (mot @@> (k-1,a-1))/=0 
                                                            then constant (mot @@> (k-1,a-1)) 4
                                                            else  error "zero error"
                                                   qt2 vec = (asColumn vec) <> (asRow $ constant 1.0 4) * transmat
                                                   qt2' mat = fromList (map sumElements (toColumns mat))


abPoints :: Int -> Int -> Int -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> [Vector Double]
abPoints a b k mot mqt obsmat transmat = take (b-a-1) restqt
                                          where qt1 = (asColumn ((toColumns mqt) !! (a-1)) ) <> (asRow $ constant 1.0 4) 
                                                ok = (asColumn ((toColumns obsmat) !! (k-1)) ) <>  (asRow $ constant 1.0 4)
                                                qt1' = fromList (map sumElements ( toColumns (qt1 * ok * transmat)))
                                                otvec =  if (mot @@> (k-1,a-1))/=0 
                                                         then constant (mot @@> (k-1,a-1)) 4
                                                         else  error "zero error"
                                                qt1'' = (qt1' / otvec)
                                                qt2 vec = (asColumn vec) <> (asRow $ constant 1.0 4) * transmat
                                                qt2' mat = fromList (map sumElements (toColumns mat))
                                                restqt = iterate (qt2' . qt2) qt1''
       

calc_Lab0 :: Int -> Int -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Vector Double -> Entry
calc_Lab0 a b mot mqt transmat obsmat rewBases 
  | a >= (b-1) = (0.0, 0)
  | a == 0 = let a0 = foldVector (+) (0.0 :: Double) $ subVector 0 (b-a-1) rewBases
             in (a0, 0)
  | otherwise = ((sumElements $ fromRows [oneCase ii |ii<-[1..10] ]), 0)
    where dif_n = b-a-1
          oneCase k | mot @@> (k-1,a-1) == 0 = constant 0.0 dif_n
--                    | otherwise = fromList [ cal_entropy (onePoint a b k t mot mqt obsmat transmat ) | t <- [a+1..b-1]]
                    | otherwise = fromList [ (cal_entropy v) * (mot @@> (k-1,a-1 ) ) | v <- (abPoints a b k mot mqt obsmat transmat) ]
                        where cal_entropy vec = let vm =  (asColumn vec) <> (asRow $ constant 1.0 (cols obsmat) ) * obsmat 
                                                    v' = fromList $ map sumElements (toColumns vm)
                                                in entropy v'


calc_Labk :: Int -> Int -> Int -> Vector Double -> Table Entry Coord -> Entry
calc_Labk a b k expRews t 
  | a >= (b-1) = (0.0, 0)
  | otherwise = if max_selj > sel0 then (max_selj, max_selj_i) else (sel0, -1)
                where selj j = (expRews @> (j-1)) - 0.0 + (fst $ findTable t (0,a,j)) + (fst $ findTable t (k-1,j,b)) 
                      selj_ary = fromList [selj jj |jj <- [a+1..b-1]]
                      max_selj = maxElement selj_ary
                      max_selj_i = maxIndex selj_ary + (a+1)
                      sel0 = fst $ findTable t (0,a,b)
                             
                               
                            
computeTbl :: Int -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Vector Double -> Vector Double -> Table Entry Coord -> Coord -> Entry
computeTbl budget mot mqt transmat obsmat rewBases expRews table (k,ii,j)
  | k==0 = calc_Lab0 ii j mot mqt transmat obsmat rewBases
  | k>0 && k<=budget =  calc_Labk ii j k expRews table
  | otherwise = (-100.0, -1)
    
    
bndsTbl :: Int -> Int -> ((Int,Int,Int),(Int,Int,Int))  
bndsTbl n budget = ((0,0,0),(budget,n+1,n+1))  -- n-1,n+1

selsList :: Int -> Int -> Int-> Table Entry Coord  -> [Int]
selsList k a b table =
      let sel = snd $ findTable table (k,a,b)
          a' = sel
          k' = k -1
      in sel: if k'>0 && sel>0 
              then selsList k' a' b table
              else []
           
                   

selectAlgo :: Int -> Int -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Vector Double -> Vector Double -> [Int]
selectAlgo n budget mot mqt transmat obsmat rewBases expRews = selsList budget 0 (n+1) t
  where t = dynamic (computeTbl budget mot mqt transmat obsmat rewBases expRews) (bndsTbl n budget)
        


main :: IO ( )
main = do
     args <- getArgs
     prior <- loadMatrix $ args !! 0  --"hmm_prior.txt"
     transmat <- loadMatrix $ args!!1 --"hmm_transmat.txt"
     obsmat <- loadMatrix $ args!!2 --"hmm_obsmat.txt"

--     disp $ mQT 24 prior transmat obsmat
--     disp $ mOT 24 prior transmat obsmat
  
--     dispv $ rew_base_ary 24 prior transmat obsmat
--     print $ sumElements $ rew_base_ary 24 prior transmat obsmat
     let n = read $ args!!3 --24 -- 24
         mqt = mQT n prior transmat obsmat
         mot = mOT n prior transmat obsmat
         expRews =  exp_rew_ary n mqt obsmat mot
         rewBases = rew_base_ary n prior transmat obsmat
         budget = read $ args!!4
         sels = selectAlgo n budget mot mqt transmat obsmat rewBases expRews
         --entry = calc_Lab0 2 4 mot mqt transmat obsmat rewBases
         --val = onePoint 1 4 3 mot mqt obsmat transmat
         --t = dynamic (computeTbl budget mot mqt transmat obsmat rewBases expRews) (bndsTbl n budget)
         --entry = calc_Labk 2 7 5 expRews t
     print $ sels
     --print $ val
     --dispv $ val

