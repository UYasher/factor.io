import BlueprintTests
import GeometryTests
import Lib
import MachineTests
import OperatorTests
import ResourceUpdateTests
import WireTests
import FactoryTests

main :: IO ()
main = do
  geometryTests
  operatorTests
  machineTests
  blueprintTests
  wireTests
  resourceUpdateTests
  factoryTests