{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Simple 
import Network.HTTP.Client.TLS
import Network.HTTP.Conduit
import Data.Aeson
import Control.Monad
import Control.Concurrent
import System.IO(openFile,IOMode(..))
import Data.Aeson.Types
import Data.String(fromString)
import Data.ByteString.Lazy(ByteString,hGetContents)

import ClassType
import Send
import Login


main = do 

    h<-openFile "config.json" ReadMode
    s<-hGetContents h :: IO ByteString

    a<-isUseful "Parse json failed." (decode s)
    config<-isUseful "Read config failed." $ (parseMaybe parseJSON a) :: IO SelectInfo

    let rN = roundName config 
        sL = selectList config
        rL = removeList config
        sT = sendTimeSlice config
        wT = waitTimeSlice config

    cks<-login
    manager<-newManager tlsManagerSettings

    c<-isUseful "Faild to get cookies." cks
    let cookies=createCookieJar c

    {-(RoundMap roundLt)<-getRoundInfo manager cookies
    roundInfo<-isUseful "Can't find round." $ findRoundInfo rN roundLt-}
    roundInfo<- let 
                    waitLoop = do
                            threadDelay wT
                            putStrLn "Wait for round to start"
                            (RoundMap roundLt)<-getRoundInfo manager cookies
                            let i = findRoundInfo rN roundLt
                            case i of 
                                Just a -> return a 
                                _->waitLoop
                            
                in waitLoop



    let p=SelectPkg{roundId=ClassType.id roundInfo,elecClassList=sL,withdrawClassList=rL}
    
    forever $ do 
                s<-selectLesson manager cookies p
                (putStrLn . show) s
                threadDelay sT

                    
findRoundInfo :: String->[RoundInfo]->Maybe RoundInfo
findRoundInfo _ [] = Nothing 
findRoundInfo n (i:is) = if name i == n then Just i else findRoundInfo n is

isUseful :: String->Maybe a->IO a 
isUseful s Nothing = fail s
isUseful _ (Just a) = return a
