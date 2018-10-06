module Truth
  type Value = struct
    val f : float32
    val c : float32
    new(f_,c_)={f=f_;c=c_}
  end

  let TRUTH_EVIDENTAL_HORIZON = 1.0f

  let truthAnd(a: float32) (b: float32) = a*b
  let truthAnd3(a: float32) (b: float32) (c: float32) = a*b*c
  let truthAnd4(a: float32) (b: float32) (c: float32) (d: float32) = a*b*c*d
  let truthOr(a: float32) (b: float32) = 1.0f - ((1.0f - a) * (1.0f - b))

  let truthWToC(w: float32) = w / (w + TRUTH_EVIDENTAL_HORIZON)
  let truthCToW(c: float32) = TRUTH_EVIDENTAL_HORIZON * c / (1.0f - c)


  let rec calcBinaryTruth(fn:string) (v1:Value) (v2:Value) =
    let negation(v1:Value) =
      let f = 1.0f - v1.f
      let c = v1.c
      Value(f, c)
  
    let deduction(v1:Value) (v2:Value) = 
      let f1 = v1.f
      let f2 = v2.f
      let c1 = v1.c
      let c2 = v2.c

      Value(truthAnd f1 f2, truthAnd (truthAnd c1 c2) (truthAnd f1 f2))
  
    let deductionWithReliance(v1:Value) (reliance: float32) =
      let f1 = v1.f
      let c1 = v1.c
      let c = truthAnd3 f1 c1 reliance
      Value(f1, c)

  
  
    let f1 = v1.f
    let f2 = v2.f
    let c1 = v1.c
    let c2 = v2.c

    let truthFunctions = dict[
      "deduction", fun() -> deduction v1 v2;
      "abduction", fun() -> 
        let c = truthAnd3 f2 c1 c2 |> truthWToC
        Value(f1, c);
      "induction", fun() -> calcBinaryTruth "abduction" v2 v1;
      "exemplification", fun() ->
        let c = truthAnd4 f1 f2 c1 c2 |> truthWToC
        Value(1.0f, c);
      "intersection", fun() ->
        let f = truthAnd f1 f2
        let c = truthAnd c1 c2
        Value(f, c);
      "reduce-conjunction", fun() ->
        let v0 = calcBinaryTruth "intersection" (negation v1) v2
        deductionWithReliance v0 1.0f |> negation;
      "comparison", fun() ->
        let f0 = truthOr f1 f2
        let f = if (f0 = 0.0f) then 0.0f else ((truthAnd f1 f2) / f0)
        let c = truthAnd3 f0 c1 c2 |> truthWToC
        Value(f, c);
      "revision", fun() -> 
        let w1 = truthCToW v1.f
        let w2 = truthCToW v2.f
        let w = w1 + w2
        let f = (w1 * f1 + w2 * f2) / w
        let c = truthWToC w
        Value(f, c);
      "analogy", fun() ->
        let f = truthAnd f1 f2
        let c = truthAnd3 c1 c2 f2
        Value(f, c)
      ]

  
    truthFunctions.[fn]()
