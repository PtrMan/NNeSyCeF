module Term

  type EnumSetType = 
  | GENERIC // generic set which interpretation (intension /extension) depends implicitly on the side of the statement
  | INT // intersional set
  | EXT // extensional set

  // copula with meta information for sets
  // left and right char's are '{' and ']' for the representation of sets
  // else ' ' is used
  type FusedCopula = char   * char * char * char *   char
  type Term =
  | Name of string
  | Sentence of FusedCopula * Term * Term
  | Set of EnumSetType * Term[]




  let mapCharToReverse x =
    match x with
    | '[' -> ']'
    | '{' -> '}'
    | ' ' -> ' '

  let rec convToString t =
    match t with
    | Name n -> sprintf "%s" n
    | Sentence((p, c0,c1,c2, s), a, b) -> (sprintf "<%c%A%c%c%c%c%c%A%c>" p  (convToString a)   (mapCharToReverse p)  c0 c1 c2 s   (convToString b)  (mapCharToReverse s)  )
