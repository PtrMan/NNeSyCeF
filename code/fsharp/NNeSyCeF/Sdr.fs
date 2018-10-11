module Sdr
  type Sdr = struct
    val arr : bool[]

    new(arr_) = {arr=arr_}
  end

  let SDR_SIZE = 2048

  let and_ (a:Sdr) (b:Sdr) =
    Sdr [| for i in 0 .. ((Array.length a.arr) - 1) -> ((a.arr.[i]) && (b.arr.[i])) |] 

  let union (a:Sdr) (b:Sdr) =
    Sdr [| for i in 0 .. ((Array.length a.arr) - 1) -> (a.arr.[i] || b.arr.[i]) |] 
  
  let overlapBitcount (a:Sdr) (b:Sdr) =
    // we need to count the number of overlapping bits of both
    let commonSdr = and_ a b
    let countOfOverlappingBits = (Array.filter (fun x -> x) commonSdr.arr) |> Array.length
    countOfOverlappingBits

  let zero = Sdr(Array.create SDR_SIZE false)

  // checks a for complete overlap with b
  let checkOverlap (a:Sdr) (b:Sdr) =
    // we need to count the number of overlapping bits of both
    let countOfOverlappingBits = overlapBitcount a b
  
    let countOfBBits = b.arr |> Array.filter (fun x -> x) |> Array.length
  
    countOfOverlappingBits = countOfBBits
  


  
  // TODO< create file for compact SDR >
  (*
  working with compact SDR's

  // returns if two compact sdr's overlap
  let sdrOverlap a b = List.exists (fun x -> (List.exists (fun y -> y = x) b)) a


  let sdrUnion a b = 
    let aSet = Set.ofList a
    let bSet = Set.ofList b
    let unionSet = Set.union aSet bSet
    Set.toArray unionSet
    
  *)
