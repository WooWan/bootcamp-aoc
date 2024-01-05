type t = array<array<string>>

let inputs =
  NodeJs.Fs.readFileSyncWith("input/Week1/Year2020Day3.sample.txt", {encoding: "utf8"})
  ->Js.String2.make
  ->Js.String2.split("\n")

let make = input => input->Belt.Array.map(x => x->Js.String2.split(""))

let height = inputs => inputs->Belt.Array.length
let width = inputs => {
  switch inputs->Belt.Array.get(0) {
  | None => 0
  | Some(s) => s->Belt.Array.length
  }
}

let getSlopeByCoords = (slope, x, y) => {
  slope
  ->Belt.Array.get(x)
  ->Belt.Option.flatMap(x => x->Belt.Array.get(y))
}
