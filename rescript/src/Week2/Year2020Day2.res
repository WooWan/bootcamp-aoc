// open Belt

type condition = {
  min: string,
  max: string,
  char: string,
}

type parsedLine = {
  condition: condition,
  password: string,
}

type generateRangeRegex = condition => Js.Re.t

let parseChunk = chunk => {
  let min = chunk[0]->Js.String2.split("-")->Belt.Array.get(0)->Belt.Option.getWithDefault("0")
  let max = chunk[0]->Js.String2.split("-")->Belt.Array.get(1)->Belt.Option.getWithDefault("0")
  let char = chunk[1]->Js.String2.get(0)

  {condition: {min, max, char}, password: chunk[2]}
}

let parseInput = (): array<parsedLine> => {
  NodeJs.Fs.readFileSyncWith("input/Week2/Year2020Day2.sample.txt", {encoding: "utf8"})
  ->Js.String2.make
  ->Js.String2.split("\n")
  ->Belt.Array.map(x => x->Js.String2.split(" "))
  ->Belt.Array.map(parseChunk)
}

let generateRangeRegex = ({min, max, char}: condition) => {
  `^(?:[^${char}]*${char}){${min},${max}}[^${char}]*$`->Js.Re.fromString
}

let validatePassword = ({condition, password}, validator) => {
  validator(condition)->Js.Re.test_(password)
}

let validatePasswordV2 = ({condition, password}) => {
  let {min, max, char} = condition
  let (startPos, endPos) = (
    Belt.Int.fromString(min)->Belt.Option.getWithDefault(0),
    Belt.Int.fromString(max)->Belt.Option.getWithDefault(0),
  )
  let isCharAtStart = password->Js.String2.get(startPos - 1) === char
  let isCharAtEnd = password->Js.String2.get(endPos - 1) === char

  isCharAtStart !== isCharAtEnd
}

parseInput()
->Belt.Array.map(chunk => validatePassword(chunk, generateRangeRegex))
->Belt.Array.reduce(0, (acc, x) => acc + (x ? 1 : 0))
->Js.log

parseInput()
->Belt.Array.map(validatePasswordV2)
->Belt.Array.reduce(0, (acc, x) => acc + (x ? 1 : 0))
->Js.log

//before
// let countValidPasswords = validator => {
//   parseInput()
//   ->Belt.Array.map(validator)
//   ->Belt.Array.reduce(0, (acc, x) => acc + (x ? 1 : 0))
//   ->Js.log
// }

// countValidPasswords(chunk => validatePassword(chunk, generateRangeRegex))
// countValidPasswords(validatePasswordV2)
