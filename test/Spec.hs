import BlueprintParsingTests
import BlueprintTests
import BudgetTests
import FactoryTests
import GeometryTests
import GraphUtilsTests
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
  graphUtilsTests
  blueprintTests
  wireTests
  resourceUpdateTests
  factoryTests
  blueprintParsingTests
  budgetTests