let input =
  NodeJs.Fs.readFileSyncWith("input/Week1/Year2020Day3.sample.txt", {encoding: "utf8"})
  ->Js.String2.make
  ->Js.String2.split("\n")
  ->Belt.Array.map(x => x->Js.String2.split(""))

let dir1 = (3, 1)
let dir2 = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

let isTrack = (grid, dir, index) => {
  let (right, down) = dir
  let lineLength =
    grid
    ->Belt.Array.getExn(0)
    ->Belt.Array.length

  let row = index / lineLength
  let col = mod(index, lineLength)

  //사각형의 너비
  let targetCol = right * row / down
  mod(row, down) == 0 && mod(targetCol, lineLength) == col
}

let countTrees = (grid, dir) => {
  grid
  ->Belt.Array.concatMany
  ->Belt.Array.keepWithIndex((_, index) => isTrack(grid, dir, index))
  ->Belt.Array.map(x => x == "#" ? 1 : 0)
  ->Belt.Array.reduce(0, (acc, val) => acc + val)
}

let solution = (graph, direction) => {
  countTrees(graph, direction)
}

let solution2 = (graph, directions) => {
  directions
  ->Belt.Array.map(x => countTrees(graph, x))
  ->Belt.Array.reduce(1, (acc, item) => acc * item)
}

solution(input, dir1)->Js.log
solution2(input, dir2)->Js.log
