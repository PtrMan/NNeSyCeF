module DeriverHelper


  // called when something was derived
  type Derived = struct
    val sdr: Sdr.Sdr
    val term: Term.Term
    val truth: Truth.Value
    val stamp: Stamp.Stamp

    val finalObservationCount: uint64

    new(finalObservationCount_, sdr_, term_, truth_, stamp_) = {finalObservationCount=finalObservationCount_;sdr=sdr_; term=term_; truth=truth_; stamp=stamp_}
  end


  type EnumAddress =
    | LEFTPREDICATE
    | LEFTSUBJECT
    | RIGHTPREDICATE
    | RIGHTSUBJECT

  // "dereferences" the premisses by address
  let derefTermByAddress (left:Datastructures.SparseTerm) (right:Datastructures.SparseTerm) (address:EnumAddress) =
    match address with
    | LEFTPREDICATE ->
      match left.term with
      | Term.Sentence(_, _, p) -> p
    | LEFTSUBJECT ->
      match left.term with
      | Term.Sentence(_, s, _) -> s
    | RIGHTPREDICATE ->
      match right.term with
      | Term.Sentence(_, _, p) -> p
    | RIGHTSUBJECT ->
      match right.term with
      | Term.Sentence(_, s, _) -> s
  

  
  type EnumBuildSetType =
  | EXT
  | INT
  | NONE


  // derives a Sentence out of premise sentences
  // with computing the coresponding SDR's
  let derivedSentenceNegation
    (negateConclusion:bool)
    (negateLeftPremise:bool)
    (negateRightPremise:bool)

    (finalObservationCount: uint64)

    (leftStamp: Stamp.Stamp)
    (rightStamp: Stamp.Stamp)

    (left: Datastructures.SparseTerm)
    (right: Datastructures.SparseTerm)

    (leftTruth: Truth.Value)
    (rightTruth: Truth.Value)

    (subjectAddress:EnumAddress)
    (copula:Term.FusedCopula)
    (predicateAddress:EnumAddress)
  
    (truthFn:string)
    (attentionFn:string) =
      // "dereferences" the premisses by address
      let derefTermByAddress2 = derefTermByAddress left right

      // TODO< demangle left side of SDR >
      // TODO< demangle right side of SDR >

      // TODO< build result SDR >
      let derivedSdr = Sdr.sdrZero



      let subjectTerm = derefTermByAddress2 subjectAddress
      let predicateTerm = derefTermByAddress2 predicateAddress
    
      let conclusionSentenceTerm = Term.Sentence(copula, subjectTerm, predicateTerm)

      let conclusionTruth = Truth.calcBinaryTruth truthFn leftTruth rightTruth

      // return a intermediate structure to decouple the derivation
      Derived(finalObservationCount, derivedSdr, conclusionSentenceTerm, conclusionTruth, Stamp.merge leftStamp rightStamp)
  
  let derivedSentence = derivedSentenceNegation false false false

  let derivedProductSentence
    (finalObservationCount: uint64)

    (leftStamp: Stamp.Stamp)
    (rightStamp: Stamp.Stamp)

    (left: Datastructures.SparseTerm)
    (right: Datastructures.SparseTerm)

    (leftTruth: Truth.Value)
    (rightTruth: Truth.Value)
  

    (subject0SetType:EnumBuildSetType)
    (subject0Address:EnumAddress)
  
    (subject1SetType:EnumBuildSetType)
    (subject1Address:EnumAddress)

    (copula:Term.FusedCopula)
  
    (predicate0SetType:EnumBuildSetType)
    (predicate0Address:EnumAddress)
  
    (predicate1SetType:EnumBuildSetType)
    (predicate1Address:EnumAddress)
  
  
    (truthFn:string)
    (attentionFn:string) =
      // TODO< build result SDR >
      let derivedSdr = Sdr.sdrZero


      // TODO< build result >
      let conclusionSentenceTerm = Term.Name "X"

      let conclusionTruth = Truth.calcBinaryTruth truthFn leftTruth rightTruth

      // return a intermediate structure to decouple the derivation
      Derived(finalObservationCount, derivedSdr, conclusionSentenceTerm, conclusionTruth, Stamp.merge leftStamp rightStamp)
