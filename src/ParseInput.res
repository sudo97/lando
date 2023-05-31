open JsonParser

let parseInt = with(number)(v =>
  try Ok(Int.fromFloat(v)) catch {
  | _ => Error("not an int")
  }
)

let parseCoords = with(object)(obj =>
  pure(Types.coords)->apply(obj->field("x", number))->apply(obj->field("y", number))
)

let parseProtocol = with(string)(s =>
  switch s {
  | "closest-enemies" => Ok(#"closest-enemies")
  | "furthest-enemies" => Ok(#"furthest-enemies")
  | "assist-allies" => Ok(#"assist-allies")
  | "avoid-crossfire" => Ok(#"avoid-crossfire")
  | "prioritize-mech" => Ok(#"prioritize-mech")
  | "avoid-mech" => Ok(#"avoid-mech")
  | _ => Error("unknown protocol: " ++ s)
  }
)

let parseEnemyType = with(string)(s =>
  switch s {
  | "soldier" => Ok(#soldier)
  | "mech" => Ok(#mech)
  | _ => Error(`unknown enemy type: ${s}`)
  }
)

let parseEnemies = with(object)(obj =>
  pure(Types.enemies)
  ->apply(obj->field("type", parseEnemyType))
  ->apply(obj->field("number", parseInt))
)

let parseScan = with(object)(obj =>
  pure(Types.scan)
  ->apply(obj->field("coordinates", parseCoords))
  ->apply(obj->field("enemies", parseEnemies))
  ->apply(obj->optional("allies", parseInt))
)

let parseInp = with(object)(obj =>
  pure(Types.make)
  ->apply(obj->field("protocols", array(parseProtocol)))
  ->apply(obj->field("scan", array(parseScan)))
)

let parse = (s: string): JsonParser.t<Types.t> => initParse(s)->flatMap(parseInp)
