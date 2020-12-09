// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
let read_lines (filePath:String) = seq {
    use sr = new StreamReader(filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let rec check (numbers: int64[]) (preamble:int) (curr:int) = 
    let num = numbers.[curr]    
    let prev = { 1 .. preamble }
            |> Seq.map (fun x -> curr - x)
            |> Seq.map (fun x -> numbers.[x]) |> Seq.toArray 
    let is_ok a b = a + b = num
    let has_partner num =
        prev |> Seq.filter (is_ok num) |> Seq.length > 0
    let found_pairs = prev |> Seq.filter has_partner |> Seq.length > 0
    // printfn "found pairs for %d %A" curr found_pairs
    if found_pairs then
        check numbers preamble (curr + 1)
    else curr 

let rec find_range (numbers: int64[]) (invalid:int64) (start_index:int) : int*int =
    printfn "find_range numbers  %d %d" invalid start_index
    let small = numbers.[start_index]
    let rec sum_to (remaining:int64) (curr_index:int) : Option<int> =
        printfn "sum to: %d %d" remaining curr_index
        if remaining = 0L then Some(curr_index)
        else if remaining < 0L then None
        else sum_to (remaining - numbers.[curr_index]) (curr_index+1)
    let next_index = sum_to (invalid - numbers.[start_index]) (start_index+1)
    match next_index with
        | Some(last_index) -> (start_index,last_index-1)
        | None -> find_range numbers invalid (start_index+1)

let find_max (numbers:int64[]) (first:int) (last:int) : int64 =
    {first..last}
              |> Seq.map (fun x -> numbers.[x])
              |> Seq.max
    
let find_min (numbers:int64[]) (first:int) (last:int) : int64 =
    {first..last}
              |> Seq.map (fun x -> numbers.[x])
              |> Seq.min


[<EntryPoint>]
let main argv =
    let input = read_lines("/Users/xeno/projects/aoc2020/day9_fs/input.txt")
    let x = input |> Seq.map (fun x -> int64(x.ToString())) |> Seq.toArray
    let invalid = check x 25 25
    let answer1 = x.[invalid]
    printfn "Answer 1 = %A" answer1
    let (first,last) = find_range x x.[invalid] 0
    printfn "first:last = %A" (first,last)
    let max = find_max x first last
    let min = find_min x first last 
    printfn "Answer 2 = %d %d -> %d" min max (min+max) 
    0 // return an integer exit code