﻿// basic datastructures of NARS

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

  type Question = struct
    val task: Task

    // For Question and Goal: best solution found so far
    val mutable bestSolution : DualSentence option

    new(task_) = {
      task=task_;
      bestSolution=None
    }
  end

  type Concept = struct
    // The term is the unique ID of the concept
    val name: SparseTerm

    val mutable beliefs: List<Task>

    val mutable questions: Question[]

    new(name_:SparseTerm) = {
      name=name_;
      beliefs = new List<Task>();
      questions = [||]
    }
  end

  // items for bags - a item has a priority
  type Item = struct
    val value : Task
    val mutable priority : float
    
    new(value_, priority_) = {value=value_;priority=priority_}
  end