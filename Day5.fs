module _2020.Day5
open TextFileReader

let rowCount = 128
let colCount = 7

type Ticket = {
    RowNumber : List<char>
    ColNumber : List<char>
  }

let calc range half =
  let upperLimit = range |> snd
  let lowerLimit = range |> fst
  if half = ("F" |> System.Char.Parse)
  then lowerLimit, (upperLimit - ((upperLimit - lowerLimit) / 2) - 1)
  else lowerLimit + ((upperLimit - lowerLimit) / 2) + 1, upperLimit

let calcC range half =
  let upperLimit = range |> snd
  let lowerLimit = range |> fst
  if half = ("L" |> System.Char.Parse)
  then lowerLimit, (upperLimit - ((upperLimit - lowerLimit) / 2) - 1)
  else lowerLimit + ((upperLimit - lowerLimit) / 2) + 1, upperLimit

let getRowNumber ticket =
  let x = ticket.RowNumber
  x
  |> List.fold (fun x -> calc x) (0,127)

let getColNumber ticket =
  let x = ticket.ColNumber
  x |> List.fold (fun x -> calcC x) (0,7)

let test () =
  let x = {
    RowNumber = "FBFBBFF" |> Seq.map id |> List.ofSeq
    ColNumber = []
  }
  getRowNumber x

let day5() =
  let inputS = readFileContents "../../../Day5Input.txt"
  inputS
  |> List.ofSeq
  |> List.map (fun s -> {
      RowNumber = s.Substring(0,7) |> Seq.map (id) |> List.ofSeq
      ColNumber = s.Substring(7,3) |> Seq.map (id) |> List.ofSeq
    } )
  |> List.map (fun ticket ->
      getRowNumber ticket |> fun (x,_) -> x
      , getColNumber ticket |> fun (x,_) -> x)
  |> List.map (fun (row, col) -> row * 8 + col)
  |> List.sort
  |> List.pairwise
  |> List.map (fun (x,y) -> y - x, x)
  |> List.filter (fun (variance, seat) -> variance <> 1 )
  |> printfn "RESULT : %A"

//  printfn "%A" (test())