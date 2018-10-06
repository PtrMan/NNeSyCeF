module Sdr
  type Sdr = struct
    val arr : bool[]

    new(arr_) = {arr=arr_}
  end

  let SDR_SIZE = 2048

  let sdrAnd(a:Sdr, b:Sdr) =
    Sdr [| for i in 0 .. ((Array.length a.arr) - 1) -> ((a.arr.[i]) && (b.arr.[i])) |] 

  let sdrUnion a b =
    [| for i in 0 .. ((List.length a) - 1) -> ((List.nth a i) || (List.nth b i)) |] 

  let sdrZero = Sdr(Array.zeroCreate SDR_SIZE)

  // checks a for complete overlap with b
  let sdrCheckOverlap(a:Sdr, b:Sdr) =
    // we need to count the number of overlapping bits of both
    let commonSdr = sdrAnd(a, b)
    let countOfOverlappingBits = (Array.filter (fun x -> x) commonSdr.arr) |> Array.length
  
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
