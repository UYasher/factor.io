module FactoryEditingTests where
import Test.QuickCheck
import FactoryEditing
import Data.Map as Map

prop_place :: Point -> Machine -> Factory -> Bool
prop_place p m f = Map.lookup p (placeMachineAt p f m) == Just m

prop_remove :: Point -> Factory -> Bool
prop_remove p f = isNothing $ Map.lookup p (removeMachineAt p f)

prop_placeRemove :: Point -> Machine -> Bool
prop_placeRemove p m = removeMachineAt p . placeMachineAt p m == id

prop_placeDisplace :: Point -> Point -> Machine -> Factory -> Bool
prop_placeDisplace p1 p2 m f =
    let f' = displaceMachineAt p1 p2 (placeMachineAt p1 m f)
    in Map.lookup (p1 + p2) f'  == Just m

prop_removePlace :: Point -> Machine -> Factory
prop_removePlace p m f =
    let f' = placeMachineAt p m (removeMachineAt p f)
    in Map.lookup p f' == Just m

prop_removeDisplace :: Point -> Point -> Bool
prop_removeDisplace p1 p2 = displaceMachineAt p1 p2 . removeMachineAt p1 == id

prop_displaceRemove :: Point -> Point -> Bool
prop_displaceRemove p1 p2 = removeMachineAt p1 . displaceMachineAt p1 p2 == displaceMachineAt p1 p2

prop_displaceRemainsInBounds :: Point -> Point -> Property
prop_displaceRemainsInBounds p1 p2 = notInBounds (p1 + p2) ==> displaceMachineAt p1 p2 == id

prop_placeRotate :: Point -> Orientation -> Factory -> Bool
prop_placeRotate p o = placeMachineAt p (rotateMachine o) == rotateMachineAt p o

prop_removeRotate :: Point -> Orientation -> Bool
prop_removeRotate p o = removeMachineAt p . rotateMachineAt p o == rotateMachineAt p o . removeMachineAt p

prop_displaceRotate :: Point -> Point -> Orientation -> Bool
prop_displaceRotate p1 p2 o = displaceMachineAt p1 p2 (rotateMachine o) == rotateMachineAt (p1 + p2) o (displaceMachineAt p1 p2)

-- Need arbitrary instance for factories -- need to be sophisticated about this
instance Arbitrary Factory where
    arbitrary = undefined
    shrink = undefined

-- Need arbitrary instance for machines -- this will be a choose 
instance Arbitrary Machine where
    arbitrary = undefined
    shrink = undefined