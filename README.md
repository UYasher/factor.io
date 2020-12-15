# project-cis552
By Maddy Adams (m20adams), Makarios Chung (chungma), and NAME (PENNKEY)

## Project overview
[Blueprint.hs](src/Blueprint.hs) defines the `Blueprint` datatype that user edits to try to solve the level.  
[Factory.hs](src/Factory.hs) provides a way of checking if a `Blueprint` solves a level by converting it into the `Factory` datatype, and then running multiple steps of the `Factory` simulation until the level is solved (or not).  
[Machine.hs](src/Machine.hs) and [Operator.hs](src/Operator.hs) define datatypes that live in `Blueprint`s, such as a `Source` that creates values, or an `Add` that sums values.  
[ResourceUpdate.hs](src/ResourceUpdate.hs) exposes a way for the `Factory` simulation to access and modify state through the `Resources` datatype.  

## Building, running, and testing

This project compiles with `stack build`. You can add any needed dependencies
and update project metadata in [package.yaml](package.yaml).

You can run the main executable with `stack run`. The entry point for the
executable is in [Main.hs](app/Main.hs). It imports [Lib.hs](src/Lib.hs),
where the bulk of your code should be written.

Write your tests in [the test directory](test/Spec.hs). You can run the tests
with `stack test`. 

Lastly, you can start a REPL with `stack ghci`.
