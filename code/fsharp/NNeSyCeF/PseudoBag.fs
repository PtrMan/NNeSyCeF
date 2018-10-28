module PseudoBag
  open Datastructures

  type Item = struct
    val value : Task
    val mutable priority : float

    new(value_, priority_) = {value=value_;priority=priority_}
  end

  type PseudoBag = struct
    val mutable items : Item[]
  end

  // scans for items which have a to low priority and a observation counter below a limit
  // returns the indices
  let scanForgetCandidates (bag:PseudoBag) (maxPriority:float) =
    let items = bag.items;

    let mutable res:int[] = [||]
  
    // TODO< probabilistic selection based on count >
    let isInRange (x:uint64) =
      match x with
      | 1UL -> true
      | 2UL -> false // don't forget twice observed things
      | 3UL -> false // don't forget three times observed things
      | _ -> true
  
    for idx in 0..items.Length-1 do
      let iItem = items.[idx]
    
      if iItem.priority < maxPriority && isInRange iItem.value.observationCount then
        res <- Array.append res [|idx|]
  
    res

  