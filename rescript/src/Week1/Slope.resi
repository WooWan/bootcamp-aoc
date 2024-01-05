type t

let height: t => int
let width: t => int

let inputs: array<string>
let make: array<string> => t

let getSlopeByCoords: (t, int, int) => option<string>
