import BlueprintParsingTests
import BlueprintTests
<<<<<<< HEAD
=======
import BudgetTests
>>>>>>> cc2a728c64884d59e54a6d172f416e8b2058e682
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
  GraphUtilsTests
  blueprintTests
  wireTests
  resourceUpdateTests
  factoryTests
  blueprintParsingTests
  budgetTests