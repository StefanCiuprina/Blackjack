module Test.Generated.Main1220590418 exposing (main)

import Tests

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "Tests" [Tests.suite] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 387728066493642, processes = 4, globs = [], paths = ["/home/stefan/UTCN/An3Sem1/FP/assignments/project/BlackJack/tests/Tests.elm"]}