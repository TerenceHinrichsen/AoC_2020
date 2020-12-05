module _2020.Day3
open TextFileReader
open System


let isTree = (=) ("#" |> Char.Parse)

let day3() =
  printfn "Loading file..."
  let stopwatch = System.Diagnostics.Stopwatch.StartNew()
  let inputS = readFileContents "..\..\..\Day3Input.txt"

  printfn "File contains %i rows" (inputS |> Seq.length)

  let myMap =
    inputS
    |> Seq.map (fun innerS ->
      innerS
      |> Seq.map id )
    |> Seq.indexed
    |> List.ofSeq

  let modulo n m = ((n % m) + m) % m

  let x right down =
    myMap
    |> List.map (fun (row, detailS) ->
      detailS
      |> List.ofSeq
      |> List.replicate 100
      |> List.concat
      |> List.indexed
      |> List.map (fun (col, char) ->
          if (modulo (row + 1) down) = 0 then
            let r = row
            let inPath = r * right = col
            let c = if inPath && isTree char then true else false
            c
          else
            false
       )
      )
    |> List.concat
    |> List.filter (fun x -> x = true)
    |> List.length
    |> printfn "%A"


//  printfn "%A" (x 1 1)
//  printfn "%A" (x 3 1)
//  printfn "%A" (x 5 1)
//  printfn "%A" (x 7 1)
  printfn "%A" (x 1 2)

// > 30 < 37 and not 33

  stopwatch.Stop()
  printfn "That took %f ms" stopwatch.Elapsed.TotalMilliseconds
