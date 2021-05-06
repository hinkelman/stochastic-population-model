// Learn more about F# at http://fsharp.org

open MathNet.Numerics.Distributions

[<EntryPoint>]
let main argv =
    let yinit = 1.0            // initial population size
    let r = 1.4                // maximum population growth rate
    let k = 20.0               // carrying capacity
    let thetasd = 0.1          // standard deviation for adding noise to the population
    let t = int argv.[0]       // number of years of growth to simulate
    let seedType = argv.[1]    // "fixed" or "random" for setting deterministic behavior
    let funType = argv.[2]     // "for", "recif", "recmatch", "unfold"
    let reps = int argv.[3]    // number of replications

    let logmodFor t y r k thetasd seedType = 
        let random = System.Random()
        let seed = if seedType = "fixed" then 2005 else random.Next()
        let normalDist = Normal(0.0, thetasd, RandomSource = System.Random(seed))
        let theta = Array.init (t - 1) (fun _ -> normalDist.Sample())
        let ys: float array = Array.zeroCreate t
        Array.set ys 0 y
        for i in 1 .. (t - 1) do
            Array.set ys i (ys.[i-1] * (r - r * (ys.[i-1] / k)) * exp(theta.[i-1]))
        ys

    let logmodRecIf t y r k thetasd seedType =
        let random = System.Random()
        let seed = if seedType = "fixed" then 2005 else random.Next()
        let normalDist = Normal(0.0, thetasd, RandomSource = System.Random(seed))
        let calc y = y * (r - r * (y / k)) * exp(normalDist.Sample())
        let rec loop acc t = 
            if t = 1 then List.rev acc
            else loop ((calc acc.Head) :: acc) (t - 1)
        loop [y] t

    let logmodRecMatch t y r k thetasd seedType = 
        let random = System.Random()
        let seed = if seedType = "fixed" then 2005 else random.Next()
        let normalDist = Normal(0.0, thetasd, RandomSource = System.Random(seed))
        let calc y = y * (r - r * (y / k)) * exp(normalDist.Sample())
        let rec loop acc t = 
            match t with 
            | 1 -> List.rev acc
            | _ -> loop ((calc acc.Head) :: acc) (t - 1)
        loop [y] t

    let logmodUnfold t y r k thetasd seedType = 
        let random = System.Random()
        let seed = if seedType = "fixed" then 2005 else random.Next()
        let normalDist = Normal(0.0, thetasd, RandomSource = System.Random(seed))
        let calc y = 
            let result = y * (r - r * (y / k)) * exp(normalDist.Sample())
            Some (y, result)
        Seq.unfold calc y |> Seq.take t

    // printfn "for loop"
    // logmodFor t yinit r k thetasd seedType |> printfn "%A"
    // printfn "---"
    // printfn "recursion (if)"
    // logmodRecIf t yinit r k thetasd seedType |> printfn "%A"
    // printfn "---"
    // printfn "recursion (match)"
    // logmodRecMatch t yinit r k thetasd seedType |> printfn "%A"
    // printfn "---"
    // printfn "unfold"
    // logmodUnfold t yinit r k thetasd seedType |> printfn "%A"

    let repeat n f =
        let rec loop n acc =
            match n with
            | 0 -> acc
            | _ -> loop (n - 1) (f() :: acc)
        loop n []

    if funType = "for" then
        repeat reps (fun () -> logmodFor t yinit r k thetasd seedType) |> ignore
    elif funType = "recif" then
        repeat reps (fun () -> logmodRecIf t yinit r k thetasd seedType) |> ignore
    elif funType = "recmatch" then
        repeat reps (fun () -> logmodRecMatch t yinit r k thetasd seedType) |> ignore
    elif funType = "unfold" then
        repeat reps (fun () -> logmodUnfold t yinit r k thetasd seedType) |> ignore
    else
        invalidArg "funType" "must be one of for, recif, recmatch, unfold"

    0 // return an integer exit code
