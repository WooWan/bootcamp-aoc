type hair
type height
type eye

type passport
type strictPassport

let parse: string => option<passport>
let parseEye: string => option<eye>
let parsePassportId: string => option<string>
let parseBirthDateYear: option<int> => option<int>
let parseIssueYear: option<int> => option<int>
let parseExpirationYear: option<int> => option<int>

let parseHairColor: string => option<hair>
let strictParse: passport => option<strictPassport>
