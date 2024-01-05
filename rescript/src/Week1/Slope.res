type t = array<array<string>>

let make = input => input->Belt.Array.map(x => x->Js.String2.split(""))

let height = inputs => inputs->Belt.Array.length
let width = inputs => {
  switch inputs->Belt.Array.get(0) {
  | Some(s) => s->Belt.Array.length
  | None => 0
  }
}

let getSlopeByCoords = (slope, x, y) => {
  slope
  ->Belt.Array.get(x)
  ->Belt.Option.flatMap(x => x->Belt.Array.get(y))
}
