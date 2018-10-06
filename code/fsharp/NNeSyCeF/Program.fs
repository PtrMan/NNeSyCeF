[<EntryPoint>]
let main argv = 
  
  let r = new main.Reasoner(10, main.derive)

  let sentence = Datastructures.DualSentence(Truth.Value(0.5f, 0.5f), Datastructures.SparseTerm(Sdr.sdrZero, Term.Sentence((' ', '-', '-', '>', ' '), (Term.Name "b"), (Term.Name "c"))))

  main.addJudgement r sentence
  
  r.step
  r.step
  r.step
  r.step
  r.step
  r.step


  printfn "%A" argv
  0 // return an integer exit code

