open Belt

type passport = {
  byr: int,
  iyr: int,
  eyr: int,
  hgt: string,
  hcl: string,
  ecl: string,
  pid: string,
  cid: option<string>,
}

let convertSplit = input => {
  input
  ->Js.String2.split(" ")
  ->Array.map(x => x->Js.String2.split(":"))
}

let input =
  NodeJs.Fs.readFileSyncWith("input/Week2/Year2020Day4.sample.txt", {encoding: "utf8"})
  ->Js.String2.make
  ->Js.String2.split("\n\n")
  ->Array.map(s => s->Js.String2.replaceByRe(%re("/\n/g"), " "))

let matchedStrings = (str, re) => {
  re
  ->Js.Re.exec_(str)
  ->Belt.Option.mapWithDefault([Js.Nullable.return("")], x => x->Js.Re.captures)
  ->Belt.Array.sliceToEnd(1)
  ->Belt.Array.map(Js.Nullable.toOption)
}

let hasPassportKey = input =>
  ["ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"]->Array.every(k => input->Map.String.has(k))

let convertValue = input =>
  input->Array.reduce(Map.String.empty, (acc, s) => {
    switch s {
    | [key, value] => acc->Map.String.set(key, value)
    | _ => acc
    }
  })

let validateHeight = height => {
  switch height {
  | [Some(height), Some(heightUnit)] => {
      let heightInt = height->Int.fromString->Option.getWithDefault(0)
      if heightUnit == "cm" {
        heightInt >= 150 && heightInt <= 193
      } else if heightUnit == "in" {
        heightInt >= 59 && heightInt <= 76
      } else {
        false
      }
    }
  | _ => false
  }
}

let validateEyeColor = eyeColor => %re("/^(amb|blu|brn|gry|grn|hzl|oth)$/")->Js.Re.test_(eyeColor)
let validateHairColor = hairColor => %re("/^#[\da-f]{6}$/")->Js.Re.test_(hairColor)
let validatePassportId = passportId => %re("/^\d{9}$/")->Js.Re.test_(passportId)

let isInRange = (value, min, max) => value >= min && value <= max
let validateBirthYear = birthYear => isInRange(birthYear, 1920, 2002)
let validateIssueYear = issueYear => isInRange(issueYear, 2010, 2020)
let validateExpirationYear = expirationYear => isInRange(expirationYear, 2020, 2030)

let validatePassport = passport => {
  let parsedHeight = passport.hgt->matchedStrings(%re("/^(\d+)(cm|in)$/"))

  validateBirthYear(passport.byr) &&
  validateIssueYear(passport.iyr) &&
  validateExpirationYear(passport.eyr) &&
  validateEyeColor(passport.ecl) &&
  validateHairColor(passport.hcl) &&
  validatePassportId(passport.pid) &&
  validateHeight(parsedHeight)
}

let parsePassport = input => {
  let passport = {
    byr: input->Map.String.getExn("byr")->Int.fromString->Belt.Option.getWithDefault(0),
    iyr: input->Map.String.getExn("iyr")->Int.fromString->Belt.Option.getWithDefault(0),
    eyr: input->Map.String.getExn("eyr")->Int.fromString->Belt.Option.getWithDefault(0),
    hgt: input->Map.String.getExn("hgt"),
    hcl: input->Map.String.getExn("hcl"),
    ecl: input->Map.String.getExn("ecl"),
    pid: input->Map.String.getExn("pid"),
    cid: input->Map.String.get("cid"),
  }

  switch validatePassport(passport) {
  | true => Some(passport)
  | false => None
  }
}

input
->Array.map(convertSplit)
->Array.map(convertValue)
->Array.keep(hasPassportKey)
->Array.length
->Js.log

input
->Array.map(convertSplit)
->Array.map(convertValue)
->Array.keep(hasPassportKey)
->Array.keepMap(parsePassport)
->Array.length
->Js.log
