module _2020.Day4

open TextFileReader

type Passport = {
  byr : bool
  iyr : bool
  eyr : bool
  hgt : bool
  hcl : bool
  ecl : bool
  pid : bool
  cid : bool   }

type PassportWithData = {
  byr : int
  iyr : int
  eyr : int
  hgt : string
  hcl : string
  ecl : string
  pid : string   }

let folder s1 s2 = (+) s1 (sprintf "%s "s2)

let cleanUpAndSplitInput stringL =
  stringL
  |> List.map (fun i -> if i = "" then "#NEWLINE" else i)// indicate which lines are the splitter
  |> List.fold (fun x -> folder x) ""// get into one list (combines the broken strings at the same time
  |> fun (string) -> string.Split "#NEWLINE" // split based on the NEWLINE
  |> List.ofArray

let checkPassport (item: string) =
    {
     byr = item.Contains "byr:"
     iyr = item.Contains "iyr:"
     eyr = item.Contains "eyr:"
     hgt = item.Contains "hgt:"
     hcl = item.Contains "hcl:"
     ecl = item.Contains "ecl:"
     pid = item.Contains "pid:"
     cid = item.Contains "cid:"
     }

let containsAllRequiredFields (ps : Passport) =
       ps.byr = true && ps.iyr = true
    && ps.eyr = true && ps.hgt = true
    && ps.hcl = true && ps.ecl = true
    && ps.pid = true

let p1 inputS =
  inputS
  |> List.ofSeq
  |> cleanUpAndSplitInput
  |> List.map checkPassport
  |> List.filter containsAllRequiredFields
  |> List.length

  |> printfn "%A"

let p2 inputS =

  let checkByr (s: string) : bool =
    s.Length = 4 && s |> System.Int32.TryParse |> fun (valid, int) -> if valid then int >=1920 && int <= 2002 else false

  let checkIyr (s: string) : bool =
    s.Length = 4 && s |> System.Int32.TryParse |> fun (valid, int) -> if valid then int >=2010 && int <= 2020 else false

  let checkEyr (s: string) : bool =
    s.Length = 4 && s |> System.Int32.TryParse |> fun (valid, int) -> if valid then int >=2020 && int <= 2030 else false

  let checkHeight (s: string) =
    match s.Substring(s.Length-2,2) with
    | "in" -> s.Substring(0,s.Length-2) |> System.Int32.TryParse |> fun (valid, int) -> if valid then int >=59 && int <= 76 else false
    | "cm" -> s.Substring(0,s.Length-2) |> System.Int32.TryParse |> fun (valid, int) -> if valid then int >=150 && int <= 193 else false
    | _ -> false

  let checkHcl (s: string) =
    let regexS = "#[0-9a-f]{6}"
    let regex = System.Text.RegularExpressions.Regex (regexS)
    regex.IsMatch(s)

  let checkEcl (s: string) =
    match s with
    | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
    | _ -> false

  let checkPid (s: string) = s.Length = 9

  inputS
  |> List.ofSeq
  |> cleanUpAndSplitInput
  |> List.map (fun x ->
      let validPassword =
        x
        |> checkPassport
        |> containsAllRequiredFields
      validPassword, x
  )
  |> List.filter (fun (complete, passportData) -> complete)
  |> List.map (fun (_, passportData ) ->
    passportData.Split " "
    |> List.ofSeq
    |> List.map (fun item ->
       if item.Length > 0 then item.Substring(0,4) else ""
       , if item.Length > 0 then item.Substring(4,item.Length - 4) else ""
      )
    )
  |> List.map
       (List.map  (fun (field, value) ->
        match field with
        | "byr:" -> field, value, checkByr value
        | "iyr:" -> field, value, checkIyr value
        | "eyr:" -> field, value, checkEyr value
        | "hgt:" -> field, value, checkHeight value
        | "hcl:" -> field, value, checkHcl value
        | "ecl:" -> field, value, checkEcl value
        | "pid:" -> field, value, checkPid value
        | _ -> field, value, false
        )
       )
  |> List.map (fun innerL ->
        innerL
        |> List.filter (fun (field, value, isValid) -> isValid)
        )
  |> List.filter (fun innerList -> innerList |> List.length >= 7)
  |> List.length


  |> printfn "RESULT: %A"

let day4() =
  let inputS = readFileContents "..\..\..\Day4Input.txt"


  p1 inputS

  p2 inputS