{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Send(getRoundInfo,selectLesson) where 

import Network.HTTP.Simple 
import Network.HTTP.Client.TLS
import Network.HTTP.Conduit
import Data.Text
import Data.ByteString
import Data.Aeson
import qualified Data.Aeson.KeyMap as D

import ClassType

genSelectPackage :: Manager->CookieJar->SelectPkg->Request 
genSelectPackage m cookies package = let    (Just request')=parseRequest "https://1.tongji.edu.cn/api/electionservice/student/elect"
                                            requestBody=encode package
                                            request=setRequestMethod "POST"
                                                $ setRequestSecure True -- Https
                                                $ setRequestBodyLBS requestBody
                                                $ setRequestManager m
                                                $ request' {cookieJar = Just cookies}
                                        in request

genRoundIdPackage :: Manager->CookieJar->Request
genRoundIdPackage m c = let (Just request')=parseRequest "https://1.tongji.edu.cn/api/electionservice/student/getRounds?projectId=1"
                            request=setRequestMethod "POST"
                                $ setRequestSecure True -- Https
                                $ setRequestManager m
                                $ request' {cookieJar = Just c}
                                        in request

getRoundInfo :: Manager->CookieJar->IO RoundMap
getRoundInfo m c = do 
                    let request=genRoundIdPackage m c
                    fmap getResponseBody (httpJSON request)

selectLesson :: Manager -> CookieJar -> SelectPkg ->IO Value
selectLesson m c p = do
                        let request=genSelectPackage m c p
                        fmap getResponseBody (httpJSON request)