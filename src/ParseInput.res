module ParseResult = Applicative.Result({
  type t = string
})

type parseResult<'a> = ParseResult.t<'a>

let parseNumber = (json: Js.Json.t): parseResult<float> =>
  switch Js.Json.classify(json) {
  | Js.Json.JSONNumber(n) => Ok(n)
  | _ => Error("not a number")
  }

let parseInt = (json: Js.Json.t): parseResult<int> =>
  switch Js.Json.classify(json) {
  | Js.Json.JSONNumber(n) => Ok(int_of_float(n))
  | _ => Error("not a number")
  }

let getField = (obj, key: string): parseResult<Js.Json.t> => {
  switch Js.Dict.get(obj, key) {
  | Some(value) => Ok(value)
  | None => Error(`missing field "${key}"`)
  }
}

let parseCoords = (json: Js.Json.t): parseResult<Types.coords> =>
  switch Js.Json.classify(json) {
  | JSONObject(obj) => {
      open ParseResult
      pure(Types.coords)
      ->apply(obj->getField("x")->Result.flatMap(parseNumber))
      ->apply(obj->getField("y")->Result.flatMap(parseNumber))
    }
  | _ => Error("Coords is not an object")
  }

let parseProtocol = (json: Js.Json.t): parseResult<Types.protocol> =>
  switch Js.Json.classify(json) {
  | Js.Json.JSONString(s) =>
    switch s {
    | "closest-enemies" => Ok(#"closest-enemies")
    | "furthest-enemies" => Ok(#"furthest-enemies")
    | "assist-allies" => Ok(#"assist-allies")
    | "avoid-crossfire" => Ok(#"avoid-crossfire")
    | "prioritize-mech" => Ok(#"prioritize-mech")
    | "avoid-mech" => Ok(#"avoid-mech")
    | _ => Error("unknown protocol: " ++ s)
    }
  | _ => Error("protocol is not a string")
  }

let parseEnemy = (json: Js.Json.t): parseResult<Types.enemy> =>
  switch Js.Json.classify(json) {
  | JSONString(s) =>
    switch s {
    | "soldier" => Ok(#soldier)
    | "mech" => Ok(#mech)
    | _ => Error(`unknown enemy type: ${s}`)
    }
  | _ => Error("enemy is not a string")
  }

let parseEnemies = (json: Js.Json.t): parseResult<Types.enemies> =>
  switch Js.Json.classify(json) {
  | JSONObject(obj) => {
      open ParseResult
      pure(Types.enemies)
      ->apply(obj->getField("type")->Result.flatMap(parseEnemy))
      ->apply(obj->getField("number")->Result.flatMap(parseInt))
    }
  | _ => Error("enemies is not an object")
  }

let parseScan = (json: Js.Json.t): parseResult<Types.scan> =>
  switch Js.Json.classify(json) {
  | JSONObject(obj) => {
      open ParseResult
      pure(Types.scan)
      ->apply(obj->getField("coordinates")->Result.flatMap(parseCoords))
      ->apply(obj->getField("enemies")->Result.flatMap(parseEnemies))
      ->apply(
        switch Js.Dict.get(obj, "allies") {
        | Some(allies) => parseInt(allies)->Result.map(x => Some(x))
        | None => pure(None)
        },
      )
    }
  | _ => Error("scan is not an object")
  }

let parseProtocolsArr = (json: Js.Json.t): parseResult<array<Types.protocol>> =>
  switch Js.Json.classify(json) {
  | JSONArray(arr) => arr->Js.Array2.map(parseProtocol)->ParseResult.sequence
  | _ => Error("protocols is not an array")
  }

let parseScanArr = (json: Js.Json.t): parseResult<array<Types.scan>> =>
  switch Js.Json.classify(json) {
  | JSONArray(arr) => arr->Js.Array2.map(parseScan)->ParseResult.sequence
  | _ => Error("scans is not an array")
  }

let parseInp = (s: string): parseResult<Types.t> => {
  let data = try Ok(s->Js.Json.parseExn) catch {
  | _ => Error("invalid json")
  }
  switch data {
  | Error(e) => Error(e)
  | Ok(obj) =>
    switch obj->Js.Json.classify {
    | JSONObject(obj) => {
        open ParseResult
        pure(Types.make)
        ->apply(obj->getField("protocols")->Result.flatMap(parseProtocolsArr))
        ->apply(obj->getField("scan")->Result.flatMap(parseScanArr))
      }
    | _ => Error("input is not an object")
    }
  }
}
