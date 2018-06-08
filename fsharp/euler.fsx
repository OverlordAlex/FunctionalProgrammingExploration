printfn "1: %d" (Array.filter (fun x -> x % 5 = 0 || x % 3 = 0) [|1..999|] |> Array.sum)

let rec fib a b =
    match (a + b) < 4000000 with
    | true ->
        let curr = a + b
        let total = fib b curr
        curr :: total
    | false -> []

printfn "2: %d" (1 :: 2 :: (fib 1 2) |> List.filter (fun x -> x % 2 = 0) |> List.sum)
let rec isPalindrome (x:string) : bool =
    let len = String.length x

    match len with
    | 0 -> true
    | 1 -> true
    | _ -> (x.[0] = x.[len-1]) && isPalindrome (x.[1..(len - 2)])

printfn "4: %A" (seq { for i in 100 .. 999 do 
                        for j in 100..999 do 
                            if isPalindrome (string (i * j)) then yield (i * j) }  
                |> Seq.max)

printfn "6: %A" (([1..100] |> List.sum |> fun x -> x * x) - ([1..100] |> List.map (fun x -> x * x) |> List.sum))

let rec collatz n steps =
    int64 n |>
    match n with
    | 1 -> steps
    | n when (n % 2) = 0 -> collatz (n / 2) (steps + 1)
    | n when (n % 2) = 1 -> collatz (3*n + 1) (steps + 1)
    | _ -> raise (System.ArgumentException "Invalid value for n")

// oof, overflow?
//printfn "14: %A" (List.map (fun x -> collatz x 0) [1..999999] |> List.max)