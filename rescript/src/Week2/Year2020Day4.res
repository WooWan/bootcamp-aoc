open Belt

type passport = {
  byr: string,
  iyr: string,
  eyr: string,
  hgt: string,
  hcl: string,
  ecl: string,
  pid: string,
  cid?: string,
}

type hair = Hex(string)
type height = Cm(int) | In(int)
type eye = Amb | Blu | Grn | Gry | Hzl | Oth | Brn

type strictPassport = {
  hgt: height,
  hcl: hair,
  ecl: eye,
  pid: string,
  byr: int,
  iyr: int,
  eyr: int,
  cid?: string,
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

let naiveParser = (key, string) => {
  let regex = Js.Re.fromString(key ++ ":([\\w#]+)")

  switch Js.Re.exec_(regex, string) {
  | Some(result) => Js.Re.captures(result)[1]->Option.flatMap(Js.Nullable.toOption)
  | None => None
  }
}

let parse = input =>
  switch (
    "byr"->naiveParser(input),
    "iyr"->naiveParser(input),
    "eyr"->naiveParser(input),
    "hgt"->naiveParser(input),
    "hcl"->naiveParser(input),
    "ecl"->naiveParser(input),
    "pid"->naiveParser(input),
    "cid"->naiveParser(input),
  ) {
  | (Some(byr), Some(iyr), Some(eyr), Some(hgt), Some(hcl), Some(ecl), Some(pid), cid) =>
    let passport: passport = {
      byr,
      iyr,
      eyr,
      hgt,
      hcl,
      ecl,
      pid,
      ?cid,
    }
    passport->Some
  | _ => None
  }

let parseEye = eye => {
  switch eye {
  | "amb" => Some(Amb)
  | "blu" => Some(Blu)
  | "brn" => Some(Brn)
  | "gry" => Some(Gry)
  | "grn" => Some(Grn)
  | "hzl" => Some(Hzl)
  | "oth" => Some(Oth)
  | _ => None
  }
}

let parsePassportId = id => {
  let regex = %re("/^\d{9}$/")

  switch Js.Re.exec_(regex, id) {
  | Some(result) => Js.Re.captures(result)[0]->Option.flatMap(Js.Nullable.toOption)
  | None => None
  }
}

let isInRange = (value, min, max) => value >= min && value <= max

let parseHeight = height =>
  switch height->matchedStrings(%re("/^(\d+)(cm|in)$/")) {
  | [Some(height), Some(unit)] =>
    switch Int.fromString(height) {
    | Some(heightInt) if unit == "cm" && isInRange(heightInt, 150, 193) => Some(Cm(heightInt))
    | Some(heightInt) if unit == "in" && isInRange(heightInt, 59, 76) => Some(In(heightInt))
    | _ => None
    }
  | _ => None
  }

let parseBirthDateYear = birthYear =>
  switch birthYear {
  | Some(birthYear) =>
    switch isInRange(birthYear, 1920, 2002) {
    | true => Some(birthYear)
    | false => None
    }
  | _ => None
  }

let parseIssueYear = issueYear =>
  switch issueYear {
  | Some(issueYear) =>
    switch isInRange(issueYear, 2010, 2020) {
    | true => Some(issueYear)
    | false => None
    }
  | _ => None
  }

let parseExpirationYear = expirationYear =>
  switch expirationYear {
  | Some(expirationYear) =>
    switch isInRange(expirationYear, 2020, 2030) {
    | true => Some(expirationYear)
    | false => None
    }
  | _ => None
  }

let parseHairColor = hair => {
  let regex = Js.Re.fromString("#([\\da-f]{6})")

  switch Js.Re.exec_(regex, hair) {
  | Some(result) => {
      let hair = Js.Re.captures(result)[1]->Option.flatMap(Js.Nullable.toOption)
      switch hair {
      | Some(hair) => Hex(hair)->Some
      | None => None
      }
    }
  | None => None
  }
}

let strictParse = (passport: passport) => {
  let {byr, iyr, eyr, hgt, hcl, ecl, pid, ?cid} = passport

  switch (
    hcl->parseHairColor,
    pid->parsePassportId,
    ecl->parseEye,
    hgt->parseHeight,
    byr->Int.fromString->parseBirthDateYear,
    iyr->Int.fromString->parseIssueYear,
    eyr->Int.fromString->parseExpirationYear,
    cid,
  ) {
  | (Some(hcl), Some(pid), Some(ecl), Some(hgt), Some(byr), Some(iyr), Some(eyr), cid) =>
    {
      hcl,
      pid,
      ecl,
      hgt,
      byr,
      iyr,
      eyr,
      ?cid,
    }->Some
  | _ => None
  }
}

input
->Array.keepMap(parse)
->Array.length
->Js.log

input
->Array.keepMap(parse)
->Array.keepMap(strictParse)
->Array.length
->Js.log
