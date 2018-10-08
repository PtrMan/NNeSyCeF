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
 
  let abduction(v1:Value) (v2:Value) = 
    let f1 = v1.f
    let f2 = v2.f
    let c1 = v1.c
    let c2 = v2.c

    let c = truthAnd3 f2 c1 c2 |> truthWToC
    Value(f1, c);
  
  let exemplification(v1:Value) (v2:Value) = 
    let f1 = v1.f
    let f2 = v2.f
    let c1 = v1.c
    let c2 = v2.c

    let c = truthAnd4 f1 f2 c1 c2 |> truthWToC
    Value(1.0f, c);
  
  let intersection(v1:Value) (v2:Value) = 
    let f1 = v1.f
    let f2 = v2.f
    let c1 = v1.c
    let c2 = v2.c

    let f = truthAnd f1 f2
    let c = truthAnd c1 c2
    Value(f, c);
  
  let analogy(v1:Value) (v2:Value) = 
    let f1 = v1.f
    let f2 = v2.f
    let c1 = v1.c
    let c2 = v2.c

    let f = truthAnd f1 f2
    let c = truthAnd3 c1 c2 f2
    Value(f, c)
  
  let revision(v1:Value) (v2:Value) = 
    let f1 = v1.f
    let f2 = v2.f
    let c1 = v1.c
    let c2 = v2.c

    let w1 = truthCToW v1.f
    let w2 = truthCToW v2.f
    let w = w1 + w2
    let f = (w1 * f1 + w2 * f2) / w
    let c = truthWToC w
    Value(f, c);
  
  let comparison(v1:Value) (v2:Value) = 
    let f1 = v1.f
    let f2 = v2.f
    let c1 = v1.c
    let c2 = v2.c

    let f0 = truthOr f1 f2
    let f = if (f0 = 0.0f) then 0.0f else ((truthAnd f1 f2) / f0)
    let c = truthAnd3 f0 c1 c2 |> truthWToC
    Value(f, c);
  
  let reduceConjunction(v1:Value) (v2:Value) = 
    let f1 = v1.f
    let f2 = v2.f
    let c1 = v1.c
    let c2 = v2.c

    let v0 = intersection (negation v1) v2
    deductionWithReliance v0 1.0f |> negation;
  
  let truthFunctions = dict[
    "deduction", deduction;
    "abduction", abduction;
    "exemplification",  exemplification;
    "intersection",  intersection;
    "reduce-conjunction", reduceConjunction;
    "comparison", comparison;
    "revision",  revision;
    "analogy",  analogy
    // TODO  resemblance
    ]
  
  let rec calcBinaryTruth(fn:string) (v1:Value) (v2:Value) =
    if fn = "induction" then
      calcBinaryTruth "abduction" v2 v1;
    else
      truthFunctions.[fn] v1 v2
