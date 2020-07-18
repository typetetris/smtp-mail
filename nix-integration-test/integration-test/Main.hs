{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Mail.SMTP hiding (simpleMail)
import Network.Mail.Mime (simpleMail, mailBcc)
import System.Environment as Env
import Test.Hspec
import Test.Hspec.Expectations

from       = Address Nothing "integration-test-smtp-mail@acme.test"
to         = Address (Just "alice") "alice@acme.test"
bcc        = Address (Just "bob") "bob@acme.test"
subject    = "Test Mail"
body       = "Test Mail Body Part"
html       = "<h1>This Test succeeded!</h1>"

createMail = fmap (\m -> m { mailBcc = [ bcc ] }) (simpleMail to from subject body html []) 

host = "acme.test"

doPlainSmtp = createMail >>= sendMail host
doTlsSmtp = createMail >>= sendMailTLS host
doSTARTTLS = createMail >>= sendMailSTARTTLS host

main :: IO ()
main = hspec $ do
  describe "Sending a mail should succeed" $ do
    it "using plain smtp" $ do
      doPlainSmtp `shouldReturn` ()
    it "using smtps" $ do
      doTlsSmtp `shouldReturn` ()
    it "using STARTTLS" $ do
      doSTARTTLS `shouldReturn` ()
