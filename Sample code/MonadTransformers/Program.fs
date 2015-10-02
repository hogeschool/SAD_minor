type MaybeResult<'a> = Done of 'a | Error of string
type StateMaybe<'s,'a> = 's -> MaybeResult<'a * 's>
type MaybeBuilder() = 
  member this.Bind (a : MaybeResult<'a>, b : 'a -> MaybeResult<'b>) =
    match a with
    | Done c -> b c
    | Error e -> Error e
  member this.Return (a : 'a) = Done a
  member this.ReturnFrom (x) = x

let opt = MaybeBuilder()

let ret x = fun s -> Done(x,s)
let (>>=) (p:StateMaybe<'s,'a>) (k:'a->StateMaybe<'s,'b>) : StateMaybe<'s,'b> =
  fun s0 ->
    opt{
      let! x,s1 = p s0
      return! k x s1
    }
type StateMaybeBuilder() =
  member this.Return(x) = ret x
  member this.Bind(p,k) = p >>= k
  member this.ReturnFrom(x) = x
let st = StateMaybeBuilder()

let fail msg = fun s -> Error msg

type Memory = { A : int; B : int }
  with static member Zero = { A = 0; B = 0 }
let getA : StateMaybe<Memory, int> = fun m -> opt{ return m.A, m }
let getB : StateMaybe<Memory, int> = fun m -> opt{ return m.B, m }
let setA (v:int) : StateMaybe<Memory, Unit> = fun m -> opt{ return (), { m with A = v } }
let setB (v:int) : StateMaybe<Memory, Unit> = fun m -> opt{ return (), { m with B = v } }

let program =
  st{
    let! a = getA
    let! b = getB
    if b = 0 then 
      return! fail "Cannot divide by zero"
    else
      do! setA (a/b)
  }

type StateMaybeList<'s,'a> = 's -> MaybeResult<List<'a> * 's>
type ListStateMaybe<'s,'a> = List<'s> -> MaybeResult<List<'a * 's>>

[<EntryPoint>]
let main argv = 
  printfn "%A" (program { A = 100; B = 0 })
  0
