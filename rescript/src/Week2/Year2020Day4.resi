type hair
type height
type eye

type passport
type strictPassport

// input -> parse -> aggragation
let parse: string => option<passport>
let strictParse: passport => option<strictPassport>

let parseEye: string => option<eye>
let parsePassportId: string => option<string>
let parseBirthDateYear: option<int> => option<int>
let parseIssueYear: option<int> => option<int>
let parseExpirationYear: option<int> => option<int>

let parseHairColor: string => option<hair>
