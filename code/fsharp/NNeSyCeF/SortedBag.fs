module SortedBag
  open Datastructures
  open C5
  open System.Collections.Generic

  // comparer for IntervalHeap
  type ItemComparer() =
    //inherit IComparer<Item>

    interface IComparer<Item> with 
        member this.Compare(l, r) = 
            if l.priority > r.priority then 1
            elif l.priority = r.priority then 0
            else -1
  
  type SortedBag = struct
    val heap: C5.IntervalHeap<Item>

    new(capacity) = {
      heap = new IntervalHeap<Item>(capacity, new ItemComparer())
    }
  end

  
  let retCount (b:SortedBag) =
    b.heap.Count
  

  // TODO< functions to add items and get lowest priority elements >
