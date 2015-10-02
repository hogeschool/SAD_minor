type State<'s,'a> = 's -> 'a * 's

let ret x = fun s -> x,s
let (>>=) (p:State<'s,'a>) (k:'a->State<'s,'b>) : State<'s,'b> =
  fun s0 ->
    let x,s1 = p s0
    k x s1

type StateBuilder() =
  member this.Return x = ret x
  member this.Bind(p,k) = p >>= k
  member this.Zero() = fun s -> (),s
let st = StateBuilder()

////

type Memory = { A : int; B : int }
  with static member Zero = { A = 0; B = 0 }
let getA : State<Memory, int> = fun m -> m.A, m
let getB : State<Memory, int> = fun m -> m.B, m
let setA (v:int) : State<Memory, Unit> = fun m -> (), { m with A = v }
let setB (v:int) : State<Memory, Unit> = fun m -> (), { m with B = v }

let inline (!) ((<+>):'a->'b->'c) =
  fun (p1:State<'s,'a>) (p2:State<'s,'b>) ->
     st{
      let! x = p1
      let! y = p2
      return x <+> y
     }

let (.&) (p1:State<'s,'a>) (p2:State<'s,'b>) : State<'s,'a * 'b> =
  st{
    let! x = p1
    let! y = p2
    return x,y
  }

type Either<'a,'b> = First of 'a | Second of 'b
let (.|) (p1:State<'s,'a>) (p2:State<'s,'b>) (choice:('a*'s) -> ('b*'s) -> Either<'a,'b>*'s) : State<'s,Either<'a,'b>> =
  fun s0 ->
    let x,s1 = p1 s0
    let y,s1' = p2 s0
    choice (x,s1) (y,s1')

let program =
  st{
    let! a,b = getA .& getB
    do! setA (a+b+1)
    do! setB (a*b+2)
    return "I am done!"
  }

[<EntryPoint>]
let main argv = 
  printfn "%A" (program Memory.Zero)
  0
