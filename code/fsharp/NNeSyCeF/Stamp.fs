module Stamp
  type Stamp = struct
    val arr : int64[]
    new(arr_)={arr=arr_}
  end

  let STAMP_LENGTH = 200

  let merge (a:Stamp) (b:Stamp) =
    let commonLength = min (Array.length a.arr) (Array.length b.arr)

    // we need the subarrays of the lowest common length
    let zipArrA = Array.sub a.arr 0 commonLength
    let zipArrB = Array.sub b.arr 0 commonLength
  
    // and the remaining arrays
    let remainArrA = Array.sub a.arr commonLength ((Array.length a.arr) - commonLength)
    let remainArrB = Array.sub b.arr commonLength ((Array.length b.arr) - commonLength)

    let zipped = Array.zip zipArrA zipArrB |> Array.map (fun(a, b) -> [|a; b|]) |> Array.concat

    let completeZipped = [|zipped;remainArrA;remainArrB|] |> Array.concat

    // limit length to maximum stamp size
    let result =  Array.sub completeZipped 0 (min STAMP_LENGTH (Array.length completeZipped))

    Stamp(result)

  let checkOverlap (a:Stamp) (b:Stamp) =
    let aAsSet = Set.ofArray a.arr
    Array.exists (fun x -> Set.contains x aAsSet) b.arr
