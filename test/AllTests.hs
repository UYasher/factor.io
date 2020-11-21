module AllTests where


import FactoryEditingTests
import GeometryTests
import MachineTests
import Test.HUnit (Assertion, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck

main :: IO ()
main = do runTestTT (TestList [])

