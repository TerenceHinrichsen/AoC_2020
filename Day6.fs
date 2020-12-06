module _2020.Day6
open TextFileReader

let folder s1 s2 = (+) s1 (sprintf "%s "s2)

let cleanUpAndSplitInput stringL =
  stringL
  |> List.map (fun i -> if i = "" then "#NEWLINE" else i)// indicate which lines are the splitter
  |> List.fold (fun x -> folder x) ""// get into one list (combines the broken strings at the same time
  |> fun (string) -> string.Split "#NEWLINE" // split based on the NEWLINE
  |> List.ofArray
  |> List.map (fun x -> x.Split " ")
  |> List.map List.ofArray
  |> id

let p1 l  =
    l
    |> List.map (fun x ->
      x
      |> List.filter (fun y -> y <> "")
      |> List.map (fun x -> x |> Seq.map (id) |> List.ofSeq)
      |> List.concat
      |> List.distinct
      |> List.sort
      |> List.length
    )
  |> List.sum

let doesListContainX x l = l |> List.contains (x |> System.Char.Parse)

let countOccurences groupSize list =
  list
  |> Seq.countBy id
  |> Seq.filter (fun (_, count) -> count = groupSize)

let listCheck (completeSet: List<string>) =
  let groupSize = completeSet.Length

  let singleString =
    completeSet
    |> List.sort
    |> List.fold (+) ""

  singleString
  |> Seq.map id
  |> List.ofSeq
  |> countOccurences groupSize

let p2 l =
  l
  |> List.map (fun groupList ->
      groupList
      |> List.filter (fun y -> y <> "")
      |> listCheck
      |> Seq.length
      )
  |> List.sum

let day6() =
  let inputS = readFileContents "../../../Day6Input.txt"
  // part one - count distinct answers
  inputS
  |> List.ofSeq
  |> cleanUpAndSplitInput
  |> p1
  |> printfn "Part 1 Result : %A"

  // part two - count answers which exist in all lists
  inputS
  |> List.ofSeq
  |> cleanUpAndSplitInput
  |> p2
  |> printfn "Part 2 Result : %A"

//  printfn "%A" (test())