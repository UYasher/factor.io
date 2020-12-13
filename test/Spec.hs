import BlueprintParsingTests
import BlueprintTests
import FactoryTests
import GeometryTests
import Lib
import MachineTests
import OperatorTests
import ResourceUpdateTests
import WireTests

main :: IO ()
main = do
  geometryTests
  operatorTests
  machineTests
  blueprintTests
  wireTests
  resourceUpdateTests
  factoryTests
  blueprintParsingTests