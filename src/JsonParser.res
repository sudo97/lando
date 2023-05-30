type t<'a> = result<'a, string>

let pure = x => Ok(x)

let apply = (f, x) =>
  switch (f, x) {
  | (Ok(f), Ok(x)) => Ok(f(x))
  | (Error(e), _) => Error(e)
  | (_, Error(e)) => Error(e)
  }

let sequence = (xs: array<result<'a, string>>): result<array<'a>, string> =>
  xs->Js.Array2.reducei((acc, curr, i) =>
    switch (acc, curr) {
    | (Ok(acc), Ok(curr)) => {
        acc->Js.Array2.push(curr)->ignore
        Ok(acc)
      }
    | (Error(e), _) => Error(e)
    | (_, Error(e)) => Error(`Error \`${e}\` at index ${i->Int.toString}`)
    }
  , Ok([]))

let map = Result.map

let flatMap = Result.flatMap

let number = (json: Js.Json.t): t<float> =>
  switch Js.Json.classify(json) {
  | Js.Json.JSONNumber(n) => Ok(n)
  | _ => Error("not a number")
  }

let withNumber = (p: float => t<'a>, json: Js.Json.t): t<'a> => json->number->flatMap(p)

let string = (json: Js.Json.t): t<string> =>
  switch Js.Json.classify(json) {
  | Js.Json.JSONString(s) => Ok(s)
  | _ => Error("not a string")
  }

let withString = (p: string => t<'a>, json: Js.Json.t): t<'a> => json->string->flatMap(p)

let boolean = (json: Js.Json.t): t<bool> =>
  switch Js.Json.classify(json) {
  | Js.Json.JSONFalse => Ok(false)
  | Js.Json.JSONTrue => Ok(true)
  | _ => Error("not a boolean")
  }

let array = (p: Js.Json.t => t<'a>, json: Js.Json.t): t<array<'a>> =>
  switch Js.Json.classify(json) {
  | Js.Json.JSONArray(a) => a->Js.Array2.map(p)->sequence
  | _ => Error("not an array")
  }

let nullable = (p: Js.Json.t => t<'a>, json: Js.Json.t): t<option<'a>> =>
  switch Js.Json.classify(json) {
  | Js.Json.JSONNull => Ok(None)
  | _ => p(json)->map(x => Some(x))
  }

let object = (json: Js.Json.t): t<Js.Dict.t<Js.Json.t>> =>
  switch Js.Json.classify(json) {
  | Js.Json.JSONObject(o) => Ok(o)
  | _ => Error("not an object")
  }

let withObject = (p: Js.Dict.t<Js.Json.t> => t<'a>, json: Js.Json.t): t<'a> =>
  json->object->flatMap(p)

let field = (obj: Js.Dict.t<Js.Json.t>, key: string, p: Js.Json.t => t<'a>): t<'a> =>
  switch Js.Dict.get(obj, key) {
  | Some(value) =>
    switch Ok(value)->flatMap(p) {
    | Ok(x) => Ok(x)
    | Error(e) => Error(`field "${key}": ${e}`)
    }
  | None => Error(`missing field "${key}"`)
  }

let optional = (obj, k, p) =>
  switch obj->Js.Dict.get(k) {
  | Some(allies) => p(allies)->map(x => Some(x))
  | None => Ok(None)
  }

let initParse = (s): t<Js.Json.t> =>
  try Ok(s->Js.Json.parseExn) catch {
  | _ => Error("invalid json")
  }
