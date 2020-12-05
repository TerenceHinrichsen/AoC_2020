module _2020.Day2
open System
open System.Text.RegularExpressions
open TextFileReader

let removeBlanks string =
  let firstBlankRemoved =
    match string with
    | x::tail -> tail
  let x =
    match firstBlankRemoved |> List.rev with
    | x::tail -> tail
  x |> List.rev

type lineRule = {
  LowerLimit: int
  UpperLimit: int
  Char : string
  Password : string
}

let getElements (stringL: List<string>) =
  let lowerLimit = stringL |> List.take 1 |> List.head |> Int32.Parse
  let upperLimit = stringL  |> List.take 2 |> List.rev |> List.take 1 |> List.head |> Int32.Parse
  let char = stringL  |> List.take 3 |> List.rev |> List.take 1 |> List.head
  let password = stringL  |> List.take 4 |> List.rev |> List.take 1 |> List.head
  {
    LowerLimit = lowerLimit
    UpperLimit = upperLimit
    Char = char
    Password = password
  }

let checkElement (lineRule: lineRule) =
  let passwordRegexString = sprintf "%s{%i,%i}" lineRule.Char lineRule.LowerLimit lineRule.UpperLimit
  let regex = Regex(passwordRegexString)
  let isMatch = regex.IsMatch lineRule.Password
  printfn "Regex: %A for %A is %A" passwordRegexString lineRule isMatch
  isMatch

let checkElementWithRegex2 (lineRule: lineRule) =
  let passwordRegexString = sprintf "^(?:[^,]*%s){%i,%i}[^,]*$" lineRule.Char lineRule.LowerLimit lineRule.UpperLimit
  let regex = Regex(passwordRegexString)
  let isMatch = regex.IsMatch lineRule.Password
  printfn "Regex: %A for %A is %A" passwordRegexString lineRule isMatch
  isMatch

let checkElement2 (lineRule : lineRule) =
  let countOfChar =
    lineRule.Password
    |> Seq.sort
    |> Seq.fold (fun counter y -> if y = (lineRule.Char |> Char.Parse) then counter + 1 else counter + 0) 0
  countOfChar >= lineRule.LowerLimit && countOfChar <= lineRule.UpperLimit

let d2p1 inputS =
    let regexString = "(\d+)-(\d+) (\w): (\D+)"
    let regex = Regex(regexString)

    let x =
      inputS
      |> Seq.map ( fun line ->
          regex.Split line
          |> List.ofSeq
        )
      |> List.ofSeq

      |> List.map ( removeBlanks )
      |> List.map ( getElements )
      |> List.filter checkElement2
      |> List.length
      |> printfn "Part 1: %A"
    x

let checkElements3 (lineRule : lineRule) =
  let passwordS =
    lineRule.Password
    |> Seq.map (id)
  let char1 = passwordS |> Seq.item (lineRule.LowerLimit - 1)
  let char2 = passwordS |> Seq.item (lineRule.UpperLimit - 1)

  let test = (lineRule.Char |> Char.Parse = char1 || lineRule.Char |> Char.Parse = char2) && not (lineRule.Char |> Char.Parse = char1 && lineRule.Char |> Char.Parse = char2)
//  printfn "lineRule: %A was %A- char1: %A - char2: %A" lineRule test char1 char2
  test


let d2p2 inputS =
    let regexString = "(\d+)-(\d+) (\w): (\D+)"
    let regex = Regex(regexString)

    let x =
      inputS
      |> Seq.map ( fun line ->
          regex.Split line
          |> List.ofSeq
        )
      |> List.ofSeq

      |> List.map ( removeBlanks )
      |> List.map ( getElements )
      |> List.filter checkElements3
      |> List.length
      |> printfn "Part 2: %A"
    x

let day2() =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let inputS = readFileContents "..\..\..\Day2Input.txt"
    do d2p1 inputS

    stopwatch.Stop()
    printfn "That took %f ms" stopwatch.Elapsed.TotalMilliseconds

    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let inputS = readFileContents "..\..\..\Day2Input.txt"
    do d2p2 inputS

    stopwatch.Stop()
    printfn "That took %f ms" stopwatch.Elapsed.TotalMilliseconds

