type passport

let convertSplit: string => array<array<string>>
let convertValue: array<array<string>> => Belt.Map.String.t<string>

let hasPassportKey: Belt.Map.String.t<'value> => bool
let validateEyeColor: string => bool
let validateHairColor: string => bool
let validatePassportId: string => bool

let isInRange: (int, int, int) => bool
let validateBirthYear: int => bool
let validateIssueYear: int => bool
let validateExpirationYear: int => bool

let validatePassport: passport => bool
