open Belt
open Slope

let countTrees = (graph, (right, down)) => {
  let rec countSlope = (row, col, count) => {
    let isBounded = row >= Slope.height

    switch isBounded {
    | true => count
    | false => {
        let updatedCount =
          graph
          ->Array.getExn(row)
          ->Array.getExn(col) == "#"
            ? count + 1
            : count
        countSlope(row + down, mod(col + right, width), updatedCount)
      }
    }
  }
  countSlope(0, 0, 0)
}
