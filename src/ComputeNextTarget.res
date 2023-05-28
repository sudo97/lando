let calculateDistance = ({x, y}: Types.coords): float => Math.sqrt(x *. x +. y *. y)

let cmp = (a, b) =>
  if a > b {
    1
  } else if a < b {
    -1
  } else {
    0
  }

let cmpDist = (a: Types.scan, b: Types.scan) =>
  Fn.on(calculateDistance, cmp, a.coordinates, b.coordinates)

let cmpAllies = (a: Types.scan, b: Types.scan) =>
  Fn.on(Fn.flip(Option.getWithDefault)(0), cmp, a.allies, b.allies)

let cmpMech = (a: Types.scan, b: Types.scan): int => Fn.on(({enemies}: Types.scan) =>
    switch enemies {
    | {type_: #mech, number} => number
    | _ => 0
    }
  , cmp, a, b)

let cmpTargets = (protocols: array<Types.protocol>, a: Types.scan, b: Types.scan): int =>
  protocols->Js.Array2.reduce((score, p) =>
    score +
    switch p {
    | #"furthest-enemies" => cmpDist(a, b)
    | #"closest-enemies" => cmpDist(b, a)
    | #"assist-allies" => cmpAllies(a, b)
    | #"avoid-crossfire" => cmpAllies(b, a) * protocols->Js.Array2.length // avoid-cross fire is stricter
    | #"prioritize-mech" => cmpMech(a, b)
    | #"avoid-mech" => cmpMech(b, a) * protocols->Js.Array2.length // so as avoid-mech
    }
  , 0)

let computeNextTarget = (inp: Types.t): option<Types.coords> =>
  inp.scan->Js.Array2.reduce((best, current) =>
    if calculateDistance(current.coordinates) > 100.0 {
      best
    } else {
      switch best {
      | None => Some(current)
      | Some(best') if cmpTargets(inp.protocols, best', current) > 0 => best
      | _ => Some(current)
      }
    }
  , None)->Option.map(item => item.coordinates)
