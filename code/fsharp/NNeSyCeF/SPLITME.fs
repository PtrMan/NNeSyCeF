module main

  open Truth
  open Term
  open Sdr
  
  open Datastructures
  open System.Collections.Generic
  
  open DeriverHelper

  // done rules
  //     nal1-nal2-inheritance-related-syllogisms
  // half done
  //     nal1-nal2-nal3-equivalence-and-implication
  //          (just a few and a lot s commented because we haven't worked out a good representation for sets)
  //     nal4-structural-inference
  //          (last part is done)
  //     nal5-implication-based-syllogisms
  //          (first easy part is done)

  let derive (premiseA: DualSentence) (premiseAStamp: Stamp.Stamp) (premiseB: DualSentence) (premiseBStamp: Stamp.Stamp) =
    let left = premiseA.termWithSdr
    let right = premiseB.termWithSdr

    let leftTruth = premiseA.truth
    let rightTruth = premiseB.truth
  
    let leftTerm = premiseA.termWithSdr.term
    let rightTerm = premiseB.termWithSdr.term

    let mutable derived : Derived[] = [||]
  
    let deriveTasks = match leftTerm, rightTerm with

      | Sentence((' ', '-', '-', '>', ' '), a, b1), Sentence((' ', '-', '-', '>', ' '), b2, c)
        when b1 = b2
          ->
            // ;;Inheritance-Related Syllogisms
            // #R[(A --> B) (B --> C) |- (A --> C) :pre ((:!= A C)) :post (:t/deduction :d/strong :allow-backward)]
            if not (a = c) then
              derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   LEFTSUBJECT (' ', '-', '-', '>', ' ') RIGHTPREDICATE  "deduction" "strong" |]
          
            // ;;Inheritance-Related Syllogisms
            // #R[(A --> B) (B --> C) |- (C --> A) :pre ((:!= C A)) :post (:t/exemplification :d/weak :allow-backward)]
            if not (c = a) then
              derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   RIGHTPREDICATE (' ', '-', '-', '>', ' ') LEFTSUBJECT  "exemplification" "weak" |]
    
    
      | Sentence((' ', '-', '-', '>', ' '), a1, b), Sentence((' ', '-', '-', '>', ' '), a2, c)
        when a1 = a2 && not (b = c)
          -> 
            // ;;Inheritance-Related Syllogisms
            // #R[(A --> B) (A --> C) |- (C --> B) :pre ((:!= B C)) :post (:t/abduction :d/weak :allow-backward)]
            derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   RIGHTPREDICATE (' ', '-', '-', '>', ' ') LEFTPREDICATE  "abduction" "weak" |]


            // ; similarity-based syllogism
            // ; If P and S are a special case of M then they might be similar (weak)
            // ; also if P and S are a general case of M
            // #R[(M --> P) (M --> S) |- (S <-> P) :post (:t/comparison :d/weak :allow-backward) :pre ((:!= S P))]
            derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   RIGHTPREDICATE (' ', '-', '<', '>', ' ') LEFTPREDICATE  "comparison" "weak" |]
    
    
      | Sentence((' ', '-', '-', '>', ' '), a, c1), Sentence((' ', '-', '-', '>', ' '), b, c2)
        when c1 = c2 && not (a = b)
          ->
            // ;;Inheritance-Related Syllogisms
            // #R[(A --> C) (B --> C) |- (B --> A) :pre ((:!= A B)) :post (:t/induction :d/weak :allow-backward)]
            derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   RIGHTSUBJECT (' ', '-', '-', '>', ' ') LEFTSUBJECT  "induction" "weak" |]


            // ; similarity-based syllogism
            // ; If P and S are a special case of M then they might be similar (weak)
            // ; also if P and S are a general case of M
            // #R[(P --> M) (S --> M) |- (S <-> P) :post (:t/comparison :d/weak :allow-backward) :pre ((:!= S P))]
            derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   RIGHTSUBJECT (' ', '-', '<', '>', ' ') LEFTSUBJECT  "comparison" "weak" |]
    
      // ; similarity from inheritance
      // ; If S is a special case of P and P is a special case of S then S and P are similar
      // #R[(S --> P) (P --> S) |- (S <-> P) :post (:t/intersection :d/strong :allow-backward)]
      | Sentence((' ', '-', '-', '>', ' '), s0, p0), Sentence((' ', '-', '-', '>', ' '), p1, s1)
        when s0 = s1 && p0 = p1
          ->
            derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   LEFTSUBJECT (' ', '-', '-', '>', ' ') LEFTPREDICATE  "intersection" "strong" |]





      // ; inheritance from similarty
      // #R[(S <-> P) (P --> S) |- (S --> P) :post (:t/reduce-conjunction :d/strong :allow-backward)]
      | Sentence((' ', '-', '<', '>', ' '), s0, p0), Sentence((' ', '-', '-', '>', ' '), p1, s1)
        when s0 = s1 && p0 = p1
          ->
            derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   LEFTSUBJECT (' ', '-', '-', '>', ' ') LEFTPREDICATE  "reduce-conjunction" "strong" |]



      // ; If M is a special case of P and S and M are similar then S is also a special case of P (strong)
      // #R[(M --> P) (S <-> M) |- (S --> P) :pre ((:!= S P)) :post (:t/analogy :d/strong :allow-backward)]
      | Sentence((' ', '-', '-', '>', ' '), m0, p), Sentence((' ', '-', '<', '>', ' '), s, m1)
        when m0 = m1  && not (s = p)
          ->
            derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   RIGHTSUBJECT (' ', '-', '<', '>', ' ') LEFTPREDICATE  "analogy" "strong" |]
    
      // ; If M is a special case of P and S and M are similar then S is also a special case of P (strong)
      // #R[(P --> M) (S <-> M) |- (P --> S) :pre ((:!= S P)) :post (:t/analogy :d/strong :allow-backward)]
      | Sentence((' ', '-', '-', '>', ' '), p, m0), Sentence((' ', '-', '<', '>', ' '), s, m1)
        when m0 = m1  && not (s = p)
          ->
            derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   LEFTSUBJECT (' ', '-', '<', '>', ' ') RIGHTSUBJECT  "analogy" "strong" |]
    
      // ; If M is a special case of P and S and M are similar then S is also a special case of P (strong)
      // #R[(M <-> P) (S <-> M) |- (S <-> P) :pre ((:!= S P)) :post (:t/resemblance :d/strong :allow-backward)]
      | Sentence((' ', '-', '-', '>', ' '), m0, p), Sentence((' ', '-', '<', '>', ' '), s, m1)
        when m0 = m1  && not (s = p)
          ->
            derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   RIGHTSUBJECT (' ', '-', '<', '>', ' ') LEFTPREDICATE  "analogy" "strong" |]
    





      // ;Set Definition Similarity to Inheritance
      //      #R[(S <-> {P}) S |- (S --> {P}) :post (:t/identity :d/identity :allow-backward)]
      | Sentence((' ', '-', '<', '>', '{'), s0, p), s1  when s0 = s1 ->
        derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   LEFTSUBJECT (' ', '-', '-', '>', '{') LEFTPREDICATE  "identity" "identity" |]
    
      // ;Set Definition Similarity to Inheritance
      // #R[(S <-> {P}) {P} |- (S --> {P}) :post (:t/identity :d/identity :allow-backward)]
      // TODO TODO TODO
    

      // ;Set Definition Similarity to Inheritance
      // #R[([S] <-> P) [S] |- ([S] --> P) :post (:t/identity :d/identity :allow-backward)]
      // TODO TODO TODO


      // ;Set Definition Similarity to Inheritance
      //      #R[([S] <-> P) P |- ([S] --> P) :post (:t/identity :d/identity :allow-backward)]
      | Sentence(('[', '-', '<', '>', ' '), s, p0), p1  when p0 = p1 ->
        derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   LEFTSUBJECT ('[', '-', '-', '>', ' ') LEFTPREDICATE  "identity" "identity" |]
    


      // ;Set Definition Similarity to Inheritance
      // #R[({S} <-> {P}) {S} |- ({P} --> {S}) :post (:t/identity :d/identity :allow-backward)]
      // TODO TODO TODO



      // ;Set Definition Similarity to Inheritance
      // #R[({S} <-> {P}) {P} |- ({P} --> {S}) :post (:t/identity :d/identity :allow-backward)]
      // TODO TODO TODO


      // ;Set Definition Similarity to Inheritance
      // #R[([S] <-> [P]) [S] |- ([P] --> [S]) :post (:t/identity :d/identity :allow-backward)]
      // TODO TODO TODO

      // ;Set Definition Similarity to Inheritance
      // #R[([S] <-> [P]) [P] |- ([P] --> [S]) :post (:t/identity :d/identity :allow-backward)]
      // TODO TODO TODO
    



      (*
      TODO TODO TODO

      ;Set Definition Unwrap
            #R[({S} <-> {P}) {S} |- (S <-> P) :post (:t/identity :d/identity :allow-backward)]
            #R[({S} <-> {P}) {P} |- (S <-> P) :post (:t/identity :d/identity :allow-backward)]
            #R[([S] <-> [P]) [S] |- (S <-> P) :post (:t/identity :d/identity :allow-backward)]
            #R[([S] <-> [P]) [P] |- (S <-> P) :post (:t/identity :d/identity :allow-backward)]

            ; Nothing is more specific than a instance so it's similar
            #R[(S --> {P}) S |- (S <-> {P}) :post (:t/identity :d/identity :allow-backward)]
            #R[(S --> {P}) {P} |- (S <-> {P}) :post (:t/identity :d/identity :allow-backward)]

            ; nothing is more general than a property so it's similar
            #R[([S] --> P) [S] |- ([S] <-> P) :post (:t/identity :d/identity :allow-backward)]
  #R[([S] --> P) P |- ([S] <-> P) :post (:t/identity :d/identity :allow-backward)]
    
    
      *)

      | _ ->
        derived <- derived

  


  


    // relation introduction rules 

    // TODO< find a way how to apply inference for sets with more than one member >

    let deriveTasks2 = match leftTerm, rightTerm with

      | Sentence((' ', '-', '-', '>', ' '), a0, c), Sentence((' ', '-', '-', '>', ' '), a1, d)
        when a0 = a1
          ->
            // ; relation introduction rule:
            // #R[(A --> C) (A --> D) |- ((* A A) --> (* C D)) :post (:t/intersection)]
            derived <- Array.append derived [| derivedProductSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   NONE LEFTSUBJECT NONE LEFTSUBJECT (' ', '-', '-', '>', ' ') NONE LEFTPREDICATE NONE RIGHTPREDICATE  "intersection" "?" |]
    
      | Sentence((' ', '-', '-', '>', ' '), a, c0), Sentence((' ', '-', '-', '>', ' '), b, c1)
        when c0 = c1
          ->
            // ; relation introduction rule:
            // #R[(A --> C) (B --> C) |- ((* A B) --> (* C C)) :post (:t/intersection)]
            derived <- Array.append derived [| derivedProductSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   NONE LEFTSUBJECT NONE RIGHTSUBJECT (' ', '-', '-', '>', ' ') NONE RIGHTPREDICATE NONE RIGHTPREDICATE  "intersection" "?" |]
    

      | Sentence(('{', '-', '-', '>', ' '), Set(GENERIC, [|a0|]), c), Sentence((' ', '-', '-', '>', ' '), a1, d)
        when a0 = a1
          ->
            // ; relation introduction rule:
            // #R[({A} --> C) (A --> D) |- ((* {A} A) --> (* C D)) :post (:t/intersection)]
            derived <- Array.append derived [| derivedProductSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth  EXT LEFTSUBJECT NONE LEFTSUBJECT (' ', '-', '-', '>', ' ') NONE LEFTPREDICATE NONE RIGHTPREDICATE  "intersection" "?" |]


      | Sentence((' ', '-', '-', '>', ' '), a0, c0), Sentence(('{', '-', '-', '>', ' '), Set(GENERIC, [|a1|]), d)
        when a0 = a1
          ->
            // ; relation introduction rule:
            // #R[(A --> C) ({A} --> D) |- ((* A {A}) --> (* C D)) :post (:t/intersection)]
            derived <- Array.append derived [| derivedProductSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth  NONE LEFTSUBJECT EXT LEFTSUBJECT (' ', '-', '-', '>', ' ') NONE LEFTPREDICATE NONE RIGHTPREDICATE  "intersection" "?" |]


      | Sentence((' ', '-', '-', '>', '['), a, Set(GENERIC, [|c0|])), Sentence((' ', '-', '-', '>', ' '), b, c1)
        when c0 = c1
          ->
            // ; relation introduction rule:
            // #R[(A --> [C]) (B --> C) |- ((* A B) --> (* [C] C)) :post (:t/intersection)]
            derived <- Array.append derived [| derivedProductSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth  NONE LEFTSUBJECT NONE RIGHTSUBJECT (' ', '-', '-', '>', ' ') INT LEFTPREDICATE NONE LEFTPREDICATE  "intersection" "?" |]


      | Sentence((' ', '-', '-', '>', ' '), a, c0), Sentence((' ', '-', '-', '>', '['), b, Set(GENERIC, [|c1|]))
        when c0 = c1
          ->
            // ; relation introduction rule:
            // #R[(A --> C) (B --> [C]) |- ((* A B) --> (* C [C])) :post (:t/intersection)]
            derived <- Array.append derived [| derivedProductSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth  NONE LEFTSUBJECT NONE RIGHTSUBJECT (' ', '-', '-', '>', ' ') NONE LEFTPREDICATE INT LEFTPREDICATE  "intersection" "?" |]

      | _ ->
        derived <- derived
         



    
    // used to simplify NAL-6 matching rules
    let checkCopula1 a b =
        match a, b with
        | '|', '|' -> true
        | '/', '/' -> true
        | '\\', '\\' -> true
        | _ -> false
  
    // ; implication-based syllogism

    let deriveTasks3 = match leftTerm, rightTerm with
        | Sentence((' ', '=', '=', '>', ' '), m0, p), Sentence((' ', '=', '=', '>', ' '), s, m1)
          when m0 = m1 && s <> p
            ->
              // #R[(M ==> P) (S ==> M) |- (S ==> P) :post (:t/deduction :order-for-all-same :allow-backward) :pre ((:!= S P))]
              derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   RIGHTSUBJECT (' ', '=', '=', '>', ' ') LEFTPREDICATE  "deduction" "?" |]
    

        | Sentence((' ', '=', '=', '>', ' '), p, m0), Sentence((' ', '=', '=', '>', ' '), s, m1)
          when m0 = m1 && s <> p
            ->
              // #R[(P ==> M) (S ==> M) |- (S ==> P) :post (:t/induction :allow-backward) :pre ((:!= S P))]
              derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   RIGHTSUBJECT (' ', '=', '=', '>', ' ') LEFTSUBJECT  "induction" "?" |]
    
        | Sentence((' ', '=', c0, '>', ' '), p, m0), Sentence((' ', '=', c1, '>', ' '), s, m1)
          when m0 = m1 && s <> p && checkCopula1 c0 c1
            ->
              (*
              #R[(P =|> M) (S =|> M) |- (S =|> P) :post (:t/induction :allow-backward) :pre ((:!= S P))]
              #R[(P =/> M) (S =/> M) |- (S =|> P) :post (:t/induction :allow-backward) :pre ((:!= S P))]
              #R[(P =\> M) (S =\> M) |- (S =|> P) :post (:t/induction :allow-backward) :pre ((:!= S P))]
              *)
              derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   RIGHTSUBJECT (' ', '=', '|', '>', ' ') LEFTSUBJECT  "induction" "?" |]
    

        | Sentence((' ', '=', '=', '>', ' '), m0, p), Sentence((' ', '=', '=', '>', ' '), m1, s)
          when m0 = m1 && s <> p
            ->
              // #R[(M ==> P) (M ==> S) |- (S ==> P) :post (:t/abduction :allow-backward) :pre ((:!= S P))]
              derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   RIGHTPREDICATE (' ', '=', '=', '>', ' ') LEFTPREDICATE  "abduction" "?" |]

        | Sentence((' ', '=', c0, '>', ' '), m0, p), Sentence((' ', '=', c1, '>', ' '), m1, s)
          when m0 = m1 && s <> p && checkCopula1 c0 c1
            ->
              (*
              #R[(M =/> P) (M =/> S) |- (S =|> P) :post (:t/abduction :allow-backward) :pre ((:!= S P))]
              #R[(M =|> P) (M =|> S) |- (S =|> P) :post (:t/abduction :allow-backward) :pre ((:!= S P))]
              #R[(M =\> P) (M =\> S) |- (S =|> P) :post (:t/abduction :allow-backward) :pre ((:!= S P))]
              *)
              derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   RIGHTPREDICATE (' ', '=', '|', '>', ' ') LEFTPREDICATE  "abduction" "?" |]


        | Sentence((' ', '=', '=', '>', ' '), p, m0), Sentence((' ', '=', '=', '>', ' '), s, m1)
          when m0 = m1 && s <> p
            ->
              // #R[(P ==> M) (M ==> S) |- (S ==> P) :post (:t/exemplification :allow-backward) :pre ((:!= S P))]
              derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   RIGHTPREDICATE (' ', '=', '=', '>', ' ') LEFTSUBJECT  "exemplification" "?" |]
      
        | Sentence((' ', '=', '/', '>', ' '), p, m0), Sentence((' ', '=', '/', '>', ' '), s, m1)
          when m0 = m1 && s <> p
            ->
              // #R[(P =/> M) (M =/> S) |- (S =\> P) :post (:t/exemplification :allow-backward) :pre ((:!= S P))]
              derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   RIGHTPREDICATE (' ', '=', '\\', '>', ' ') LEFTSUBJECT  "exemplification" "?" |]
      
        | Sentence((' ', '=', '\\', '>', ' '), p, m0), Sentence((' ', '=', '\\', '>', ' '), s, m1)
          when m0 = m1 && s <> p
            ->
              // #R[(P =\> M) (M =\> S) |- (S =/> P) :post (:t/exemplification :allow-backward) :pre ((:!= S P))]
              derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   RIGHTPREDICATE (' ', '=', '/', '>', ' ') LEFTSUBJECT  "exemplification" "?" |]
      
        | Sentence((' ', '=', '|', '>', ' '), p, m0), Sentence((' ', '=', '|', '>', ' '), s, m1)
          when m0 = m1 && s <> p
            ->
              // #R[(P =|> M) (M =|> S) |- (S =|> P) :post (:t/exemplification :allow-backward) :pre ((:!= S P))]
              derived <- Array.append derived [| derivedSentence premiseAStamp premiseBStamp left right  leftTruth rightTruth   RIGHTPREDICATE (' ', '=', '|', '>', ' ') LEFTSUBJECT  "exemplification" "?" |]



    deriveTasks
    deriveTasks2
    deriveTasks3

    derived





  


  // helper for the priority queue 
  // TODO< implement efficient prority queue from ANSNA >
  // TODO< refactor into own generic struct >
  type ConceptPriorityQueue = struct
    val mutable content: List<Concept>
  
    val mutable map_ : Dictionary<Term, Concept>

    val size : int

    new(size_:int) = {
      size=size_;

      content = new List<Concept>();
      map_ = new Dictionary<Term, Concept>();
    }
  end

  // TODO< add ref cell for task and so on >

  // TODO< use refactored class >
  type TaskPriorityQueue = struct
    val content: List<Task>

    val size : int

    new(size_:int) = {
      content = List<Task>();
      size=size_
    }
  end

  // function to add a element to the queue
  // TODO< implement magic of queue >
  let add (queue: ConceptPriorityQueue) (element: Concept) =
    queue.content.Add(element)

    printfn "add concept =%A" (convToString element.name.term)

    queue.map_.Add(element.name.term, element)

  let queryByTerm (queue: ConceptPriorityQueue) (query: Term): Concept Option =
    match queue.map_.TryGetValue(query) with
    | (true, x) -> Some x
    | (false, _) -> None


  let has (queue: ConceptPriorityQueue) (query: Term) =
    match queue.map_.TryGetValue(query) with
    | (true, x) -> true
    | (false, _) -> false
  


  // returns n elements which have the highest priority
  let retHighestPriorityN (queue: TaskPriorityQueue) (n: int)  =
    let mutable result : Task[] = [||]
  
    // collect highest n
    for i in 0 .. min n ((queue.content.Count)-1) do
      result <- Array.append result [|queue.content.[i]|]

    result

  type Reasoner = class
    // TODO< pull into attention system >
    val concepts: ConceptPriorityQueue

    val tasks: TaskPriorityQueue

    val taskSelectionAmount: int

    // function which is used for derivation
    val derivationFn: (DualSentence) -> (Stamp.Stamp) -> (DualSentence) -> (Stamp.Stamp) -> Derived[]

    val mutable stampCounter : int64

    new(taskSelectionAmount_:int,derivationFn_)={
      concepts = new ConceptPriorityQueue 50;
      tasks=TaskPriorityQueue 50;
      taskSelectionAmount=10;
      derivationFn=derivationFn_;

      stampCounter = int64(0)
    }

    // public just for ease of adding functionality
    member self.addJudgmentAsBeliefAndTask (sparseTerm: SparseTerm) (truth: Truth.Value) (stamp: Stamp.Stamp) =
      self.conceptualize sparseTerm

      match self.concepts.map_.TryGetValue sparseTerm.term with
      | (True, v) ->
        // add belief
        v.beliefs.Add (Task(DualSentence(truth, SparseTerm(sdrZero, sparseTerm.term)), DERIVED, JUDGMENT, stamp))

        // add task
        self.tasks.content.Add (Task(DualSentence(truth, SparseTerm(sdrZero, sparseTerm.term)), DERIVED, JUDGMENT, stamp))
  
    // creates new concepts for all involved (sub)terms if they don't exist
    //
    // public for testing
    member self.conceptualize(t: SparseTerm) =
      let rec conceptualizeRec(t: SparseTerm, current: Term) =
        if not (has self.concepts current) then
          printfn "DOESNT HAVE concept for %s" (convToString current)

          let mutable c = Concept(SparseTerm(sdrZero, current))
        
          add self.concepts c
          |> ignore
      
        match current with
          | Sentence(_, termSubject, termPredicate) ->
            conceptualizeRec(t, termSubject)
            conceptualizeRec(t, termPredicate) |>
            ignore
          | _ ->
            ignore |> ignore
    
      conceptualizeRec(t, t.term)
  
    member private self.processTask(task: Task) =
      // queries all concepts by (sub) terms of term
      // similar to the functionality implemented by ALANN
      //
      // /return returns all concepts which match a term
      let rec queryConceptsByTerm(queryTerm: Term) =
        printfn "queryConceptsByTerm() %s" (convToString queryTerm)

        let queryResultOfThisQuery = queryByTerm self.concepts queryTerm

        printfn "result=%A" queryResultOfThisQuery
      
        // we need to transform this result into a array
        let queryResultOfThisQueryAsList = match queryResultOfThisQuery with
          | Some(resultConcept) -> [|resultConcept|]
          | None -> [||]

        // we need to query the components recursivly
        let queryResultOfSentence = match queryTerm with
          | Sentence(_, termSubject, termPredicate) ->
            let queryResultOfSubject = queryConceptsByTerm termSubject
            let queryResultOfPredicate = queryConceptsByTerm termPredicate

            Array.concat [ queryResultOfSubject ; queryResultOfPredicate ]
          | _ -> [||]
      
        Array.concat [ queryResultOfThisQueryAsList ; queryResultOfSentence ]


      let processQuestion() =
        // TODO< call into Q&A handlers >
      
        // TODO< functionality - we need to try to answer the question with beliefs >
        ignore |> ignore
    
      let processJudgment() =
        printfn "processJudgment called"

        self.conceptualize task.sentence.termWithSdr

        printfn "Number of concepts after conceptualize  =%i" self.concepts.content.Count
        printfn "Number of concepts after conceptualize  =%i" self.concepts.map_.Count

      
        // only the revision
        // /param concept in which the task is revised
        // /param beliefIdx index of revised belief
        let revise (c:Concept) (beliefIdx:int) =
          printfn "REVISE"
        
          let truthOfBelief = c.beliefs.[beliefIdx].sentence.truth
          let revisedTruth = Truth.calcBinaryTruth "revision" truthOfBelief task.sentence.truth
          let termWithSdr = c.beliefs.[beliefIdx].sentence.termWithSdr
        
          let mutable x = c.beliefs.[beliefIdx]
          x.sentence <- new DualSentence(revisedTruth, termWithSdr)

          c.beliefs.[beliefIdx] <- x

          ignore



        let conceptsToConsult = queryConceptsByTerm task.sentence.termWithSdr.term

        // required to collect derivtion results to avoid unwanted feedback
        let derived = List<Derived[]>()

        //printfn "HRE %d" (conceptsToConsult.Length)

        for iConcept in conceptsToConsult do
          printfn "Consult concept"

          // we process all beliefs
          // TODO< should we select the beliefs just out of a bag instead? >
          for beliefIdx in 0.. iConcept.beliefs.Count-1 do
            let iBelief = iConcept.beliefs.[beliefIdx]

            let isOverlapping = Stamp.checkOverlap iBelief.stamp task.stamp

            if not isOverlapping then
              let isRevisable = iBelief.sentence.termWithSdr.term = task.sentence.termWithSdr.term
              
              if isRevisable then
                // revision

                revise iConcept beliefIdx |>

                ignore
              else
                // normal inference

                let thisderived = self.derivationFn task.sentence task.stamp iBelief.sentence iBelief.stamp
                derived.Add thisderived |>

                ignore

      
        for i in derived do
          for j in i do
            convToString j.term |> printfn "derived: %s"



        // me:
        // >say we have a-->b as task and b-->c as belief
        // >what will the derived result ( a --> c ) be? belief or task or both?
        // patham9:
        // >Both
        // It will cycle around as event, but also be in the belief table

        // add derived to knowledgebase without loopback
        // TODO< refactor >
        for i in derived do
          for j in i do
            let sparseTerm = SparseTerm(Sdr.sdrZero, j.term)
            self.addJudgmentAsBeliefAndTask sparseTerm j.truth j.stamp


      let deriveTasks = match task.type_ with
      | JUDGMENT -> processJudgment()
      | QUESTION -> processQuestion()

      deriveTasks

    member self.step =
      // we need to fetch the tasks which currently have the highest priority
      let highestPriorityTasks = retHighestPriorityN self.tasks self.taskSelectionAmount

      // TODO< process tasks >
      for iTask in highestPriorityTasks do
        self.processTask iTask
  end



  let addJudgement (r:Reasoner) (sentence:DualSentence) =
    let stamp = Stamp.Stamp([|r.stampCounter|])
    r.stampCounter <- r.stampCounter+int64(1);

    r.addJudgmentAsBeliefAndTask sentence.termWithSdr sentence.truth stamp

  