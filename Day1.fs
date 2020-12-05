module _2020.Day1
open System


let day1 () =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let inputS = TextFileReader.readFileContents "..\..\..\input.txt"

    let inputL_I =
      inputS
      |> Seq.map (Int32.Parse)
      |> List.ofSeq

    let y =
      inputL_I
      |> List.map (fun outerValue ->
        inputL_I
        |> List.map ( fun innerValue ->
          inputL_I
          |> List.filter (fun innermostValue -> innerValue + outerValue + innermostValue = 2020) )
        )
      |> List.concat
      |> List.concat
      |> List.distinct

    printfn "Values which add up to 2020 are %A" y

    let answer = y |> List.fold (fun c v -> v * c) 1
    printfn "Multiplied they are: %A" answer
    stopwatch.Stop()
    printfn "That took %f ms" stopwatch.Elapsed.TotalMilliseconds
