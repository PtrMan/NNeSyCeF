module main

  open Truth

  open Term





  
  


  open Sdr


  // we store a dual representation of SDR and a symbolic description of a Term
  type SparseTerm = struct
    val sdr : Sdr.Sdr
    val term : Term

    new(sdr_, term_) = {sdr=sdr_;term=term_}
  end

  // we store a dual representation of SDR and a symbolic description of a Term
  type DualSentence = struct
    val truth: Truth.Value

    val termWithSdr: SparseTerm

    // TODO< other stuff of sentence >

    new(truth_: Truth.Value, termWithSdr_: SparseTerm) = { truth = truth_; termWithSdr=termWithSdr_ }
  end


  type SentenceDatabaseBySdr = struct
    val dualSentences : DualSentence list
  end

  // TODO< let the tak scan the memory with each timeslice >
  // (syncronously) queries the database for (dual) sentences which (symetrically) match the SDR
  let queryDatabaseBySdr(db:SentenceDatabaseBySdr, query:Sdr.Sdr) =
    // function which returns true if the SDR matches up
    let filterFn(x:DualSentence) =
      sdrCheckOverlap(x.termWithSdr.sdr, query)

    List.filter filterFn db.dualSentences



  // check if two terms are equal
  // TODO< check hash and check content if hash matches >
  let isEqual(a:Term, b:Term) =
    a = b

  type EnumAddress =
    | LEFTPREDICATE
    | LEFTSUBJECT
    | RIGHTPREDICATE
    | RIGHTSUBJECT


  // called when something was derived
  type Derived = struct
    val sdr: Sdr.Sdr
    val term: Term
    val truth: Truth.Value

    new(sdr_, term_, truth_) = {sdr=sdr_; term=term_; truth=truth_}
  end



  // "dereferences" the premisses by address
  let derefTermByAddress (left:SparseTerm) (right:SparseTerm) (address:EnumAddress) =
    match address with
    | LEFTPREDICATE ->
      match left.term with
      | Sentence(_, _, p) -> p
    | LEFTSUBJECT ->
      match left.term with
      | Sentence(_, s, _) -> s
    | RIGHTPREDICATE ->
      match right.term with
      | Sentence(_, _, p) -> p
    | RIGHTSUBJECT ->
      match right.term with
      | Sentence(_, s, _) -> s


  // derives a Sentence out of premise sentences
  // with computing the coresponding SDR's
  let derivedSentence(
    left: SparseTerm,
    right: SparseTerm,

    leftTruth: Truth.Value,
    rightTruth: Truth.Value,

    subjectAddress:EnumAddress,
    copula:FusedCopula,
    predicateAddress:EnumAddress,
  
    truthFn:string,
    attentionFn:string) =
      // "dereferences" the premisses by address
      let derefTermByAddress2 = derefTermByAddress left right

      // TODO< demangle left side of SDR >
      // TODO< demangle right side of SDR >

      // TODO< build result SDR >
      let derivedSdr = sdrZero



      let subjectTerm = derefTermByAddress2 subjectAddress
      let predicateTerm = derefTermByAddress2 predicateAddress
    
      let conclusionSentenceTerm = Sentence(copula, subjectTerm, predicateTerm)

      let conclusionTruth = Truth.calcBinaryTruth truthFn leftTruth rightTruth

      // return a intermediate structure to decouple the derivation
      Derived(derivedSdr, conclusionSentenceTerm, conclusionTruth)



  type EnumBuildSetType =
  | EXT
  | INT
  | NONE

  let derivedProductSentence(
    left: SparseTerm,
    right: SparseTerm,

    leftTruth: Truth.Value,
    rightTruth: Truth.Value,
  

    subject0SetType:EnumBuildSetType,
    subject0Address:EnumAddress,
  
    subject1SetType:EnumBuildSetType,
    subject1Address:EnumAddress,

    copula:FusedCopula,
  
    predicate0SetType:EnumBuildSetType,
    predicate0Address:EnumAddress,
  
    predicate1SetType:EnumBuildSetType,
    predicate1Address:EnumAddress,
  
  
    truthFn:string,
    attentionFn:string) =
      // TODO< build result SDR >
      let derivedSdr = sdrZero


      // TODO< build result >
      let conclusionSentenceTerm = Name "X"

      let conclusionTruth = Truth.calcBinaryTruth truthFn leftTruth rightTruth

      // return a intermediate structure to decouple the derivation
      Derived(derivedSdr, conclusionSentenceTerm, conclusionTruth)




  // done rules
  //     nal1-nal2-inheritance-related-syllogisms
  // half done
  //     nal1-nal2-nal3-equivalence-and-implication
  //          (just a few and a lot s commented because we haven't worked out a good representation for sets)
  //     nal4-structural-inference
  //          (last part is done)
  //     nal5-implication-based-syllogisms
  //          (first easy part is done)

  let renameme0(premiseA: DualSentence) (premiseB: DualSentence) =
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
            if not (isEqual(a, c)) then
              derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   LEFTSUBJECT, (' ', '-', '-', '>', ' '), RIGHTPREDICATE,  "deduction", "strong") |]
          
            // ;;Inheritance-Related Syllogisms
            // #R[(A --> B) (B --> C) |- (C --> A) :pre ((:!= C A)) :post (:t/exemplification :d/weak :allow-backward)]
            if not (isEqual(c, a)) then
              derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   RIGHTPREDICATE, (' ', '-', '-', '>', ' '), LEFTSUBJECT,  "exemplification", "weak") |]
    
    
      | Sentence((' ', '-', '-', '>', ' '), a1, b), Sentence((' ', '-', '-', '>', ' '), a2, c)
        when a1 = a2 && not (isEqual(b, c))
          -> 
            // ;;Inheritance-Related Syllogisms
            // #R[(A --> B) (A --> C) |- (C --> B) :pre ((:!= B C)) :post (:t/abduction :d/weak :allow-backward)]
            derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   RIGHTPREDICATE, (' ', '-', '-', '>', ' '), LEFTPREDICATE,  "abduction", "weak") |]


            // ; similarity-based syllogism
            // ; If P and S are a special case of M then they might be similar (weak)
            // ; also if P and S are a general case of M
            // #R[(M --> P) (M --> S) |- (S <-> P) :post (:t/comparison :d/weak :allow-backward) :pre ((:!= S P))]
            derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   RIGHTPREDICATE, (' ', '-', '<', '>', ' '), LEFTPREDICATE,  "comparison", "weak") |]
    
    
      | Sentence((' ', '-', '-', '>', ' '), a, c1), Sentence((' ', '-', '-', '>', ' '), b, c2)
        when c1 = c2 && not (isEqual(a, b))
          ->
            // ;;Inheritance-Related Syllogisms
            // #R[(A --> C) (B --> C) |- (B --> A) :pre ((:!= A B)) :post (:t/induction :d/weak :allow-backward)]
            derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   RIGHTSUBJECT, (' ', '-', '-', '>', ' '), LEFTSUBJECT,  "induction", "weak") |]


            // ; similarity-based syllogism
            // ; If P and S are a special case of M then they might be similar (weak)
            // ; also if P and S are a general case of M
            // #R[(P --> M) (S --> M) |- (S <-> P) :post (:t/comparison :d/weak :allow-backward) :pre ((:!= S P))]
            derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   RIGHTSUBJECT, (' ', '-', '<', '>', ' '), LEFTSUBJECT,  "comparison", "weak") |]
    
      // ; similarity from inheritance
      // ; If S is a special case of P and P is a special case of S then S and P are similar
      // #R[(S --> P) (P --> S) |- (S <-> P) :post (:t/intersection :d/strong :allow-backward)]
      | Sentence((' ', '-', '-', '>', ' '), s0, p0), Sentence((' ', '-', '-', '>', ' '), p1, s1)
        when s0 = s1 && p0 = p1
          ->
            derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   LEFTSUBJECT, (' ', '-', '-', '>', ' '), LEFTPREDICATE,  "intersection", "strong") |]





      // ; inheritance from similarty
      // #R[(S <-> P) (P --> S) |- (S --> P) :post (:t/reduce-conjunction :d/strong :allow-backward)]
      | Sentence((' ', '-', '<', '>', ' '), s0, p0), Sentence((' ', '-', '-', '>', ' '), p1, s1)
        when s0 = s1 && p0 = p1
          ->
            derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   LEFTSUBJECT, (' ', '-', '-', '>', ' '), LEFTPREDICATE,  "reduce-conjunction", "strong") |]



      // ; If M is a special case of P and S and M are similar then S is also a special case of P (strong)
      // #R[(M --> P) (S <-> M) |- (S --> P) :pre ((:!= S P)) :post (:t/analogy :d/strong :allow-backward)]
      | Sentence((' ', '-', '-', '>', ' '), m0, p), Sentence((' ', '-', '<', '>', ' '), s, m1)
        when m0 = m1  && not (isEqual(s, p))
          ->
            derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   RIGHTSUBJECT, (' ', '-', '<', '>', ' '), LEFTPREDICATE,  "analogy", "strong") |]
    
      // ; If M is a special case of P and S and M are similar then S is also a special case of P (strong)
      // #R[(P --> M) (S <-> M) |- (P --> S) :pre ((:!= S P)) :post (:t/analogy :d/strong :allow-backward)]
      | Sentence((' ', '-', '-', '>', ' '), p, m0), Sentence((' ', '-', '<', '>', ' '), s, m1)
        when m0 = m1  && not (isEqual(s, p))
          ->
            derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   LEFTSUBJECT, (' ', '-', '<', '>', ' '), RIGHTSUBJECT,  "analogy", "strong") |]
    
      // ; If M is a special case of P and S and M are similar then S is also a special case of P (strong)
      // #R[(M <-> P) (S <-> M) |- (S <-> P) :pre ((:!= S P)) :post (:t/resemblance :d/strong :allow-backward)]
      | Sentence((' ', '-', '-', '>', ' '), m0, p), Sentence((' ', '-', '<', '>', ' '), s, m1)
        when m0 = m1  && not (isEqual(s, p))
          ->
            derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   RIGHTSUBJECT, (' ', '-', '<', '>', ' '), LEFTPREDICATE,  "analogy", "strong") |]
    





      // ;Set Definition Similarity to Inheritance
      //      #R[(S <-> {P}) S |- (S --> {P}) :post (:t/identity :d/identity :allow-backward)]
      | Sentence((' ', '-', '<', '>', '{'), s0, p), s1  when s0 = s1 ->
        derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   LEFTSUBJECT, (' ', '-', '-', '>', '{'), LEFTPREDICATE,  "identity", "identity") |]
    
      // ;Set Definition Similarity to Inheritance
      // #R[(S <-> {P}) {P} |- (S --> {P}) :post (:t/identity :d/identity :allow-backward)]
      // TODO TODO TODO
    

      // ;Set Definition Similarity to Inheritance
      // #R[([S] <-> P) [S] |- ([S] --> P) :post (:t/identity :d/identity :allow-backward)]
      // TODO TODO TODO


      // ;Set Definition Similarity to Inheritance
      //      #R[([S] <-> P) P |- ([S] --> P) :post (:t/identity :d/identity :allow-backward)]
      | Sentence(('[', '-', '<', '>', ' '), s, p0), p1  when p0 = p1 ->
        derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   LEFTSUBJECT, ('[', '-', '-', '>', ' '), LEFTPREDICATE,  "identity", "identity") |]
    


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
            derived <- Array.append derived [| derivedProductSentence(left, right,  leftTruth, rightTruth,   NONE, LEFTSUBJECT, NONE, LEFTSUBJECT, (' ', '-', '-', '>', ' '), NONE, LEFTPREDICATE, NONE, RIGHTPREDICATE,  "intersection", "?") |]
    
      | Sentence((' ', '-', '-', '>', ' '), a, c0), Sentence((' ', '-', '-', '>', ' '), b, c1)
        when c0 = c1
          ->
            // ; relation introduction rule:
            // #R[(A --> C) (B --> C) |- ((* A B) --> (* C C)) :post (:t/intersection)]
            derived <- Array.append derived [| derivedProductSentence(left, right,  leftTruth, rightTruth,   NONE, LEFTSUBJECT, NONE, RIGHTSUBJECT, (' ', '-', '-', '>', ' '), NONE, RIGHTPREDICATE, NONE, RIGHTPREDICATE,  "intersection", "?") |]
    

      | Sentence(('{', '-', '-', '>', ' '), Set(GENERIC, [|a0|]), c), Sentence((' ', '-', '-', '>', ' '), a1, d)
        when a0 = a1
          ->
            // ; relation introduction rule:
            // #R[({A} --> C) (A --> D) |- ((* {A} A) --> (* C D)) :post (:t/intersection)]
            derived <- Array.append derived [| derivedProductSentence(left, right,  leftTruth, rightTruth,  EXT, LEFTSUBJECT, NONE, LEFTSUBJECT, (' ', '-', '-', '>', ' '), NONE, LEFTPREDICATE, NONE, RIGHTPREDICATE,  "intersection", "?") |]


      | Sentence((' ', '-', '-', '>', ' '), a0, c0), Sentence(('{', '-', '-', '>', ' '), Set(GENERIC, [|a1|]), d)
        when a0 = a1
          ->
            // ; relation introduction rule:
            // #R[(A --> C) ({A} --> D) |- ((* A {A}) --> (* C D)) :post (:t/intersection)]
            derived <- Array.append derived [| derivedProductSentence(left, right,  leftTruth, rightTruth,  NONE, LEFTSUBJECT, EXT, LEFTSUBJECT, (' ', '-', '-', '>', ' '), NONE, LEFTPREDICATE, NONE, RIGHTPREDICATE,  "intersection", "?") |]


      | Sentence((' ', '-', '-', '>', '['), a, Set(GENERIC, [|c0|])), Sentence((' ', '-', '-', '>', ' '), b, c1)
        when c0 = c1
          ->
            // ; relation introduction rule:
            // #R[(A --> [C]) (B --> C) |- ((* A B) --> (* [C] C)) :post (:t/intersection)]
            derived <- Array.append derived [| derivedProductSentence(left, right,  leftTruth, rightTruth,  NONE, LEFTSUBJECT, NONE, RIGHTSUBJECT, (' ', '-', '-', '>', ' '), INT, LEFTPREDICATE, NONE, LEFTPREDICATE,  "intersection", "?") |]


      | Sentence((' ', '-', '-', '>', ' '), a, c0), Sentence((' ', '-', '-', '>', '['), b, Set(GENERIC, [|c1|]))
        when c0 = c1
          ->
            // ; relation introduction rule:
            // #R[(A --> C) (B --> [C]) |- ((* A B) --> (* C [C])) :post (:t/intersection)]
            derived <- Array.append derived [| derivedProductSentence(left, right,  leftTruth, rightTruth,  NONE, LEFTSUBJECT, NONE, RIGHTSUBJECT, (' ', '-', '-', '>', ' '), NONE, LEFTPREDICATE, INT, LEFTPREDICATE,  "intersection", "?") |]

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
              derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   RIGHTSUBJECT, (' ', '=', '=', '>', ' '), LEFTPREDICATE,  "deduction", "?") |]
    

        | Sentence((' ', '=', '=', '>', ' '), p, m0), Sentence((' ', '=', '=', '>', ' '), s, m1)
          when m0 = m1 && s <> p
            ->
              // #R[(P ==> M) (S ==> M) |- (S ==> P) :post (:t/induction :allow-backward) :pre ((:!= S P))]
              derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   RIGHTSUBJECT, (' ', '=', '=', '>', ' '), LEFTSUBJECT,  "induction", "?") |]
    
        | Sentence((' ', '=', c0, '>', ' '), p, m0), Sentence((' ', '=', c1, '>', ' '), s, m1)
          when m0 = m1 && s <> p && checkCopula1 c0 c1
            ->
              (*
              #R[(P =|> M) (S =|> M) |- (S =|> P) :post (:t/induction :allow-backward) :pre ((:!= S P))]
              #R[(P =/> M) (S =/> M) |- (S =|> P) :post (:t/induction :allow-backward) :pre ((:!= S P))]
              #R[(P =\> M) (S =\> M) |- (S =|> P) :post (:t/induction :allow-backward) :pre ((:!= S P))]
              *)
              derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   RIGHTSUBJECT, (' ', '=', '|', '>', ' '), LEFTSUBJECT,  "induction", "?") |]
    

        | Sentence((' ', '=', '=', '>', ' '), m0, p), Sentence((' ', '=', '=', '>', ' '), m1, s)
          when m0 = m1 && s <> p
            ->
              // #R[(M ==> P) (M ==> S) |- (S ==> P) :post (:t/abduction :allow-backward) :pre ((:!= S P))]
              derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   RIGHTPREDICATE, (' ', '=', '=', '>', ' '), LEFTPREDICATE,  "abduction", "?") |]

        | Sentence((' ', '=', c0, '>', ' '), m0, p), Sentence((' ', '=', c1, '>', ' '), m1, s)
          when m0 = m1 && s <> p && checkCopula1 c0 c1
            ->
              (*
              #R[(M =/> P) (M =/> S) |- (S =|> P) :post (:t/abduction :allow-backward) :pre ((:!= S P))]
              #R[(M =|> P) (M =|> S) |- (S =|> P) :post (:t/abduction :allow-backward) :pre ((:!= S P))]
              #R[(M =\> P) (M =\> S) |- (S =|> P) :post (:t/abduction :allow-backward) :pre ((:!= S P))]
              *)
              derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   RIGHTPREDICATE, (' ', '=', '|', '>', ' '), LEFTPREDICATE,  "abduction", "?") |]


        | Sentence((' ', '=', '=', '>', ' '), p, m0), Sentence((' ', '=', '=', '>', ' '), s, m1)
          when m0 = m1 && s <> p
            ->
              // #R[(P ==> M) (M ==> S) |- (S ==> P) :post (:t/exemplification :allow-backward) :pre ((:!= S P))]
              derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   RIGHTPREDICATE, (' ', '=', '=', '>', ' '), LEFTSUBJECT,  "exemplification", "?") |]
      
        | Sentence((' ', '=', '/', '>', ' '), p, m0), Sentence((' ', '=', '/', '>', ' '), s, m1)
          when m0 = m1 && s <> p
            ->
              // #R[(P =/> M) (M =/> S) |- (S =\> P) :post (:t/exemplification :allow-backward) :pre ((:!= S P))]
              derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   RIGHTPREDICATE, (' ', '=', '\\', '>', ' '), LEFTSUBJECT,  "exemplification", "?") |]
      
        | Sentence((' ', '=', '\\', '>', ' '), p, m0), Sentence((' ', '=', '\\', '>', ' '), s, m1)
          when m0 = m1 && s <> p
            ->
              // #R[(P =\> M) (M =\> S) |- (S =/> P) :post (:t/exemplification :allow-backward) :pre ((:!= S P))]
              derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   RIGHTPREDICATE, (' ', '=', '/', '>', ' '), LEFTSUBJECT,  "exemplification", "?") |]
      
        | Sentence((' ', '=', '|', '>', ' '), p, m0), Sentence((' ', '=', '|', '>', ' '), s, m1)
          when m0 = m1 && s <> p
            ->
              // #R[(P =|> M) (M =|> S) |- (S =|> P) :post (:t/exemplification :allow-backward) :pre ((:!= S P))]
              derived <- Array.append derived [| derivedSentence(left, right,  leftTruth, rightTruth,   RIGHTPREDICATE, (' ', '=', '|', '>', ' '), LEFTSUBJECT,  "exemplification", "?") |]



    deriveTasks
    deriveTasks2
    deriveTasks3

    derived






  // basic datastructures of NARS

  type EnumTaskType =
    | QUESTION
    | JUDGMENT
    //| GOAL
    //| QUEST

  type EnumTaskSource =
  | INPUT
  | DERIVED

  type Task = struct
    val mutable sentence: DualSentence

    val source: EnumTaskSource

    val type_: EnumTaskType

    new(sentence_:DualSentence, source_:EnumTaskSource, type__:EnumTaskType) = {sentence=sentence_;source=source_;type_=type__}
  end

  open System.Collections.Generic

  type Concept = struct
    // The term is the unique ID of the concept
    val name: SparseTerm

    // TODO< questions >

    val mutable beliefs: List<Task>

    new(name_:SparseTerm)={name=name_; beliefs = new List<Task>()}
  end


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

    printfn "ADD %A" (convToString element.name.term)

    queue.map_.Add(element.name.term, element)

    printfn "HERE555"

  let queryByTerm (queue: ConceptPriorityQueue) (query: Term): Concept Option =
    match queue.map_.TryGetValue(query) with
    | (True, x) -> Some x
    | (False, _) -> None
  
    //let result = queue.map_.TryGetValue(query)

    //printfn "%d" queue.map_.Count
    //printfn "queryByTerm() res=%A" result

    //result

  let has (queue: ConceptPriorityQueue) (query: Term) =
    match queue.map_.TryGetValue(query) with
    | (True, x) -> True
    | (False, _) -> False
  
    //queue.map_.TryFind(query) <> None


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

    new(taskSelectionAmount_:int)={
      concepts = new ConceptPriorityQueue 50;
      tasks=TaskPriorityQueue 50;
      taskSelectionAmount=10;}
  
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

        printfn "HRE %d" (conceptsToConsult.Length)

        for iConcept in conceptsToConsult do
          printfn "Consult concept"

          // we process all beliefs
          // TODO< should we select the beliefs just out of a bag instead? >
          for beliefIdx in 0.. iConcept.beliefs.Count-1 do
            let iBelief = iConcept.beliefs.[beliefIdx]

            let isRevisable = iBelief.sentence.termWithSdr.term = task.sentence.termWithSdr.term

            if isRevisable then
              // revision

              revise iConcept beliefIdx |>

              ignore
            else
              // normal inference

              let thisderived = renameme0 task.sentence iBelief.sentence
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
            self.conceptualize sparseTerm

            match self.concepts.map_.TryGetValue j.term with
            | (True, v) ->
              // add belief
              v.beliefs.Add (Task(DualSentence(j.truth, SparseTerm(sdrZero, j.term)), DERIVED, JUDGMENT))

              // add task
              self.tasks.content.Add (Task(DualSentence(j.truth, SparseTerm(sdrZero, j.term)), DERIVED, JUDGMENT))



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















  let t1 = Name "b"
  let t2 = Sentence(('{', '-', '-', '>', '['), Name "a", Name "b")

  convToString t2 |> printfn "%s"







  // test

  let a0 = DualSentence(
    Truth.Value(0.5f, 0.5f), 

    SparseTerm(
      sdrZero,
      Sentence((' ', '-', '-', '>', ' '), Name("a"), Name("b"))
      )
    )


  let a1 = DualSentence(
    Truth.Value(0.5f, 0.5f), 

    SparseTerm(
      sdrZero,
      Sentence((' ', '-', '-', '>', ' '), Name("b"), Name("c"))
      )
    )

  let derived = renameme0 a0 a1

  //printfn "%f;%f" derived.truth.f derived.truth.c

  printfn "numberOfDerived=%i" (Array.length derived)
  printfn "derived=%A" derived



  let r = new Reasoner 10

  // add test task
  let t5000 = Sentence((' ', '-', '-', '>', ' '), (Name "a"), (Name "b"))
  let st = SparseTerm(Sdr.sdrZero, t5000)
  let sentence = DualSentence(Truth.Value(0.5f, 0.5f), st)
  let mutable newTask = Task(sentence, INPUT, JUDGMENT)
  r.tasks.content.Add(newTask)


  // add test belief


  let mutable concept0 = Concept(SparseTerm(Sdr.sdrZero, Name("b")))

  concept0.beliefs.Add(Task(DualSentence(Truth.Value(0.5f, 0.5f), SparseTerm(Sdr.sdrZero, Sentence((' ', '-', '-', '>', ' '), (Name "b"), (Name "c")))), DERIVED, JUDGMENT))


  r.concepts.map_.Add(Name("b"), concept0)


  r.step

  printfn "%i" r.tasks.content.Count


  r.step

  printfn "%i" r.tasks.content.Count