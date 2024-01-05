open Belt

type input = array<string>

let solution = inputs => {
  let direction = (3, 1)

  inputs
  ->Slope.make
  ->Toboggan.countTrees(direction)
}

let solution2 = inputs => {
  let directions = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

  directions
  ->Array.map(direction => inputs->Slope.make->Toboggan.countTrees(direction))
  ->Array.reduce(1, (acc, item) => acc * item)
}

let inputs =
  NodeJs.Fs.readFileSyncWith("input/Week1/Year2020Day3.sample.txt", {encoding: "utf8"})
  ->Js.String2.make
  ->Js.String2.split("\n")

inputs
->solution
->Js.log

inputs
->solution2
->Js.log
