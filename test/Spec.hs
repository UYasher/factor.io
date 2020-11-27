import FactoryEditingTests
import GeometryTests
import Lib
import MachineTests
import WireTests

main :: IO ()
main = do
  geometryTests
  machineTests
  factoryEditingTests
  wireTests