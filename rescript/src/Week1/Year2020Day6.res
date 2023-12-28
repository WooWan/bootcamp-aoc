let parseChunk = chunk => {
  chunk
  ->Js.String2.split("\n")
  ->Belt.Array.map(x => x->Js.String2.split("")->Belt.Set.String.fromArray)
}

let parseInput = () => {
  NodeJs.Fs.readFileSyncWith("input/Week1/Year2020Day6.sample.txt", {encoding: "utf8"})
  ->Js.String2.make
  ->Js.String2.split("\n\n")
  ->Belt.Array.map(parseChunk)
}

let findUniqueElements = x => {
  x->Belt.Array.reduce(Belt.Set.String.empty, (acc, item) => acc->Belt.Set.String.union(item))
}

let intersectElements = x => {
  //누적 교집합
  x->Belt.Array.reduce(x->findUniqueElements, Belt.Set.String.intersect)
}

let solve = (input, fn) => {
  input
  ->Belt.Array.map(fn)
  ->Belt.Array.map(Belt.Set.String.size)
  ->Belt.Array.reduce(0, (acc, item) => acc + item)
}

parseInput()
->solve(findUniqueElements)
->Js.log

parseInput()
->solve(intersectElements)
->Js.log
