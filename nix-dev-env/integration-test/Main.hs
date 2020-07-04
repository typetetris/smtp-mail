{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Mail.SMTP hiding (simpleMail)
import Network.Mail.Mime (simpleMail)
import System.Environment as Env
import Network.Connection
import Network.BSD (getHostName)

from       = Address Nothing "integration-test-smtp-mail@acme.test"
to         = Address (Just "alice") "alice@acme.test"
subject    = "Test Mail"
body       = "Test Mail Body Part"
html       = "<h1>This Test succeeded!</h1>"

createMail = simpleMail to from subject body html []

host = "acme.test"

doPlainSmtp = sendMail host

-- this looks so scary as we have to deactivate certificate checking for our
-- self signed certificate. It should be ok, as we registered it with 
-- the test system, but ... this would be a tls bug.
doTlsSmtp mail = do
  con <- connectSMTPWithHostNameAndTlsSettings host 465 getHostName (Just $ TLSSettingsSimple True False False)
  renderAndSend con mail
  closeSMTP con

main :: IO ()
main = do
  args <- Env.getArgs
  mail <- createMail
  case args of
    ("--plain":_) -> doPlainSmtp mail
    ("--tls":_) -> doTlsSmtp mail
