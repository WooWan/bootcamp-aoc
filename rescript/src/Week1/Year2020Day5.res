let parseInput = () => {
  NodeJs.Fs.readFileSyncWith("input/Week1/Year2020Day5.sample.txt", {encoding: "utf8"})
  ->Js.String2.make
  ->Js.String2.split("\n")
}

let col = (0, 127)
let row = (0, 7)

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

let findId = (row, col) => row * 8 + col

let solution = (directions, initRowRange, initialColRange) => {
  let ranges =
    directions
    ->Js.String2.split("")
    ->Belt.Array.reduce((initRowRange, initialColRange), (currentRanges, direction) => {
      let (currentRowRange, currentColRange) = currentRanges
      findSeat(direction, currentRowRange, currentColRange)
    })

  // let test = ranges[0]처럼 접근이 불가해서 destructuring을 사용했는데 더 깔끔하게?
  // reduce의 return이 tuple 형태여서 pipe chaining을 사용못했는데 고쳐보기
  let (rowRange, colRange) = ranges
  let (row, _) = rowRange
  let (col, _) = colRange
  findId(row, col)
}

let sortSeat = inputs => {
  inputs
  ->Belt.Array.map(input => solution(input, col, row))
  ->Belt.SortArray.stableSortBy((a, b) => a - b)
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

let part1 = inputs => {
  sortSeat(inputs)->Belt.Array.getExn(Belt.Array.length(inputs) - 1)
}

let part2 = inputs => {
  let sortedSeat = sortSeat(inputs)

  let startSeat = Belt.Array.getExn(sortedSeat, 0)
  let endSeat = Belt.Array.getExn(sortedSeat, Belt.Array.length(sortedSeat) - 1)

  let seatSum = (endSeat + startSeat) * (endSeat - startSeat + 1) / 2
  let totalSum = sortedSeat->Belt.Array.reduce(0, (sum, seatId) => {
    sum + seatId
  })

  seatSum - totalSum
}

parseInput()
->part1
->Js.log

parseInput()
->part2
->Js.log
