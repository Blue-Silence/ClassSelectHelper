{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Simple 
import Network.HTTP.Client.TLS
import Network.HTTP.Conduit
import Data.Aeson
import Control.Monad
import Control.Concurrent

import ClassType
import Send
import Login



main = do 
    
    roundNmae<-return roundNmae1 
    selectList<-return selectList1
    remList<-return remList1
    -- Above part will be read from file later.

    cks<-login
    manager<-newManager tlsManagerSettings

    c<-isUseful "Faild to get cookies./n" cks
    let cookies=createCookieJar c

    (RoundMap roundLt)<-getRoundInfo manager cookies
    roundInfo<-isUseful "Can't find round./n" $ findRoundInfo roundNmae roundLt

    let p=SelectPkg{roundId=ClassType.id roundInfo,elecClassList=selectList,withdrawClassList=remList}
    
    forever $ do 
                s<-selectLesson manager cookies p
                (putStrLn . show) s
                threadDelay (5*(10^5))

    
    
                    
findRoundInfo :: String->[RoundInfo]->Maybe RoundInfo
findRoundInfo _ [] = Nothing 
findRoundInfo n (i:is) = if name i == n then Just i else findRoundInfo n is



selectList1 :: [Lesson]
--selectList1 = [Lesson 1111111124879074 "17018501" "170185",Lesson 1111111124879787 "14097101" "140971",Lesson 1111111124879791 "14098301" "140983"]
selectList1 = [Lesson 1111111124878748 "03054501" "030545"]
remList1 :: [Lesson]
remList1 = []
roundNmae1="新生选课"

isUseful :: String->Maybe a->IO a 
isUseful s Nothing = fail s
isUseful _ (Just a) = return a