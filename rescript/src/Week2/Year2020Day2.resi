type condition = {
  min: string,
  max: string,
  char: string,
}

type parsedLine = {
  condition: condition,
  password: string,
}

// type condition = (string, string, string)

// type concat 방법?
// type parsedLine = condition & string
// type parsedLine = (condition, string)
// type parsedLine = (string, string, string, string)
type generateRangeRegex = condition => Js.Re.t

let parseChunk: array<string> => parsedLine
let parseInput: unit => array<parsedLine>

let generateRangeRegex: condition => Js.Re.t

let validatePassword: (parsedLine, generateRangeRegex) => bool
let validatePasswordV2: parsedLine => bool
