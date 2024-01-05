type slope = array<array<string>>

let inputs =
  NodeJs.Fs.readFileSyncWith("input/Week1/Year2020Day3.sample.txt", {encoding: "utf8"})
  ->Js.String2.make
  ->Js.String2.split("\n")

let make = input => input->Belt.Array.map(x => x->Js.String2.split(""))

let width = inputs->make->Belt.Array.getExn(0)->Belt.Array.length
let height = inputs->make->Belt.Array.length
