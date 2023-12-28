open Belt

let col = (0, 127)
let row = (0, 7)

let parseInput = () => {
  NodeJs.Fs.readFileSyncWith("input/Week1/Year2020Day5.sample.txt", {encoding: "utf8"})
  ->Js.String2.make
  ->Js.String2.split("\n")
}

let findSeat = (direction, (rowStart, rowEnd), (colStart, colEnd)) => {
  let rowMid = rowStart + (rowEnd - rowStart) / 2
  let colMid = colStart + (colEnd - colStart) / 2

  switch direction {
  | "F" => ((rowStart, rowMid), (colStart, colEnd))
  | "B" => ((rowMid + 1, rowEnd), (colStart, colEnd))
  | "L" => ((rowStart, rowEnd), (colStart, colMid))
  | "R" => ((rowStart, rowEnd), (colMid + 1, colEnd))
  | _ => failwith("invalid direction")
  }
}

let findRangesV2 = (directions, initRowRange, initColRange) => {
  let ranges =
    directions
    ->Js.String2.split("")
    ->Array.reduce((initRowRange, initColRange), (currentRanges, direction) => {
      let (currentRowRange, currentColRange) = currentRanges
      findSeat(direction, currentRowRange, currentColRange)
    })

  let (rowRange, colRange) = ranges
  let (row, _) = rowRange
  let (col, _) = colRange

  (row, col)
}

let findId = (row, col) => row * 8 + col

let findAllSeatIds = inputs => {
  inputs
  ->Belt.Array.map(input => findRangesV2(input, col, row))
  ->Belt.Array.map(ranges => {
    let (row, col) = ranges
    findId(row, col)
  })
}

let findHighestSeatId = seatIds => {
  seatIds
  ->Belt.SortArray.stableSortBy((a, b) => a - b)
  ->Belt.Array.getExn(Belt.Array.length(seatIds) - 1)
}

let findMissingSeat = sortedSeats => {
  let startSeat = Belt.Array.getExn(sortedSeats, 0)
  let endSeat = Belt.Array.getExn(sortedSeats, Belt.Array.length(sortedSeats) - 1)

  let rangeSum = (endSeat + startSeat) * (endSeat - startSeat + 1) / 2
  let totalSum = sortedSeats->Belt.Array.reduce(0, (sum, seatId) => {
    sum + seatId
  })

  rangeSum - totalSum
}

let findMissingId = seatIds => {
  let sortedSeatIds = seatIds->Belt.SortArray.stableSortBy((a, b) => a - b)
  let (startSeat, endSeat) = (
    Belt.Array.getExn(sortedSeatIds, 0),
    Belt.Array.getExn(sortedSeatIds, Belt.Array.length(sortedSeatIds) - 1),
  )
  let expectedSum = (endSeat + startSeat) * (endSeat - startSeat + 1) / 2
  let actualSum = sortedSeatIds->Belt.Array.reduce(0, (sum, seatId) => sum + seatId)

  expectedSum - actualSum
}

let part1V2 = () => {
  parseInput()
  ->findAllSeatIds
  ->findHighestSeatId
}

let part2V2 = () => {
  parseInput()
  ->findAllSeatIds
  ->findMissingId
}

part1V2()->Js.log
part2V2()->Js.log
