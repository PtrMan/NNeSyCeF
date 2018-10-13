open System

let mutable rng = Random()

let randomAlphanumericString() =
    let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    seq {
        for i in {0..7} do
            yield chars.[rng.Next(chars.Length)]
    } |> Seq.toArray |> (fun x -> new String(x))

[<EntryPoint>]
let main argv = 
  
  let r = new main.Reasoner(10, main.derive, [|main.defaultQuestionAndAnswerHandler|])

  for i in 0 .. 2 do

    let sentence = Datastructures.DualSentence(Truth.Value(0.5f, 0.5f), Datastructures.SparseTerm(Sdr.zero, Term.Sentence((' ', '-', '-', '>', ' '), (Term.Name (randomAlphanumericString())), (Term.Name (randomAlphanumericString())))))

    main.addJudgement r sentence
  
    r.step

    printfn "[i ] cycle=%d" i

  r.step
  r.step
  r.step
  r.step
  r.step


  printfn "%A" argv
  0 // return an integer exit code

