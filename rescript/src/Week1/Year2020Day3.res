open Slope
open Toboggan
open Belt

let solution = inputs => {
  let direction = (3, 1)

  inputs
  ->make
  ->countTrees(direction)
}

let solution2 = inputs => {
  let directions = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

  directions
  ->Array.map(direction => inputs->make->countTrees(direction))
  ->Array.reduce(1, (acc, item) => acc * item)
}

inputs
->solution
->Js.log

inputs
->solution2
->Js.log
