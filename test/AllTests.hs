import           Data.Functor
import qualified Slim.SlimServerTest as Server
import           Slim.SlimTest
import           Test.HUnit          hiding (Path, assert)

tests :: Test
tests = TestList [ Server.allTests
                 , TestList [ canEncodeNestedLists
                            , canEncodeInstructions
                            , canDecodeAnswer
                            , slimClientSendAndReceiveMessages
                            , testCanRenumberInstructions
                            , matchInstructionsToAnswers
                            ]
                 ]

main :: IO ()
main = void $ runTestTT tests
