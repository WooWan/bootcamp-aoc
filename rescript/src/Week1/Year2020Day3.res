module Day3 = {
  let inputs =
    NodeJs.Fs.readFileSyncWith("input/Week1/Year2020Day3.sample.txt", {encoding: "utf8"})
    ->Js.String2.make
    ->Js.String2.split("\n")
    ->Belt.Array.map(x => x->Js.String2.split(""))

  let width = inputs->Belt.Array.getExn(0)->Belt.Array.length
  let height = inputs->Belt.Array.length
}

{
  //Belt 주석 해제하면 inputs 타입 추론이 변경되어서 에러. 왜?
  // open Belt
  open Day3

  inputs->Js.log
  let dir1 = (3, 1)
  let dir2 = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

  let countTrees = ((right, down)) => {
    let rec countSlope = (row, col, count) => {
      let isBounded = row >= height

      switch isBounded {
      | true => count
      | false => {
          let updatedCount = inputs[row][col] == "#" ? count + 1 : count
          countSlope(row + down, mod(col + right, width), updatedCount)
        }
      }
    }
    countSlope(0, 0, 0)
  }

  let solution = direction => {
    countTrees(direction)
  }

  let solution2 = directions => {
    directions
    ->Belt.Array.map(countTrees)
    ->Belt.Array.reduce(1, (acc, item) => acc * item)
  }

  solution(dir1)->Js.log
  solution2(dir2)->Js.log
}
