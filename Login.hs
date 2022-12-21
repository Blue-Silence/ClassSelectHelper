{-# LANGUAGE OverloadedStrings #-}
module Login where

import Web.Api.WebDriver
import Test.Tasty.WebDriver
import Control.Monad.Trans.Identity

import Test.Tasty
import qualified System.Environment as SE
import Control.Monad
import Data.Functor.Identity
import Data.Text
import qualified Network.HTTP.Client as NH
import Data.Time.Clock(getCurrentTime,NominalDiffTime(..),addUTCTime)
import System.IO.Unsafe(unsafePerformIO) 
import Data.Text.Encoding

type WebDriver eff a = WebDriverT eff a


--l :: WebDriver IO [CK]
l :: WebDriver IO [NH.Cookie]
l = do
  navigateTo "https://1.tongji.edu.cn/sslogin"
  waitFor "https://1.tongji.edu.cn/workbench"
  --a<-fmap (Prelude.map (\x->CK (_cookieName x) (_cookieValue x))) getAllCookies
  a<-fmap (Prelude.map cookiesTransfer) getAllCookies
  return a

--example1 :: IO (Maybe [CK])
login :: IO (Maybe [NH.Cookie])
login = do
    (a,_,_)<-execWebDriverT defaultWebDriverConfig
            (runIsolated defaultFirefoxCapabilities l)
    case a of
        Right b->return $ Just b
        _->return $ Nothing


waitFor t = do
        wait 500
        u<-getCurrentUrl 
        if u==t then return () else waitFor t


cookiesTransfer :: Cookie->NH.Cookie
cookiesTransfer c = let helper f c = case (f c) of 
                                        Nothing->""
                                        Just a->encodeUtf8 a
                        sec = case (_cookieSecure c) of 
                                    Just a->a 
                                    Nothing->False 
                        htp = case (_cookieHttpOnly c) of 
                                    Just a->a 
                                    Nothing->False
                        t = unsafePerformIO getCurrentTime
                        h=10^10 :: NominalDiffTime
                        in 
                            NH.Cookie {
                                    NH.cookie_name=(helper _cookieName c)
                                    ,NH.cookie_value=(helper _cookieValue c)
                                    ,NH.cookie_expiry_time=addUTCTime h t
                                    ,NH.cookie_domain=(helper _cookieDomain c)
                                    ,NH.cookie_path=(helper _cookiePath c)
                                    ,NH.cookie_creation_time=t 
                                    ,NH.cookie_last_access_time=t 
                                    ,NH.cookie_persistent=False 
                                    ,NH.cookie_host_only=False 
                                    ,NH.cookie_secure_only=sec
                                    ,NH.cookie_http_only=htp
                            }

                        
                                    

