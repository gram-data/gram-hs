-- | Test runner for gramref-cli.
--
-- This module uses HSpec to run all test suites.
import Test.Hspec
import qualified Spec.CLI.OutputSpec as OutputSpec
import qualified Spec.CLI.JSONSpec as JSONSpec
import qualified Spec.CLI.GenerateSuiteSpec as GenerateSuiteSpec
import qualified Spec.CLI.ConvertSpec as ConvertSpec
import qualified Spec.Properties.DeterministicSpec as DeterministicSpec

main :: IO ()
main = hspec testSpec

testSpec :: Spec
testSpec = do
  describe "gramref-cli" $ do
    OutputSpec.spec
    JSONSpec.spec
    GenerateSuiteSpec.spec
    ConvertSpec.spec
    DeterministicSpec.spec

