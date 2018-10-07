// basic datastructures of NARS

module Datastructures
  // we store a dual representation of SDR and a symbolic description of a Term
  type SparseTerm = struct
    val sdr : Sdr.Sdr
    val term : Term.Term

    new(sdr_, term_) = {sdr=sdr_;term=term_}
  end

  // we store a dual representation of SDR and a symbolic description of a Term
  type DualSentence = struct
    val truth: Truth.Value

    val termWithSdr: SparseTerm

    // TODO< other stuff of sentence >

    new(truth_: Truth.Value, termWithSdr_: SparseTerm) = { truth = truth_; termWithSdr=termWithSdr_ }
  end

  
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

    // was the input task observed and account for by the ATTENTION mechanism?
    val mutable wasObserved: bool

    // how ofter was the belief observed
    val mutable observationCount: uint64

    val source: EnumTaskSource

    val type_: EnumTaskType

    val stamp: Stamp.Stamp
    

    new(sentence_:DualSentence, source_:EnumTaskSource, type__:EnumTaskType, stamp_:Stamp.Stamp) = {
      sentence=sentence_;
      source=source_;
      type_=type__;
      stamp=stamp_;
      wasObserved=false;
      observationCount=uint64(1)
    }
  end

  open System.Collections.Generic

  type Concept = struct
    // The term is the unique ID of the concept
    val name: SparseTerm

    // TODO< questions >

    val mutable beliefs: List<Task>

    new(name_:SparseTerm)={name=name_; beliefs = new List<Task>()}
  end