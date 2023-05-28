open Test

let testWithCoords = (s, tester) => {
  let setup = (): (Js.Dict.t<Js.Json.t>, Types.coords) => {
    (
      Js.Dict.fromList(list{("x", Js.Json.number(1.0)), ("y", Js.Json.number(2.0))}),
      {x: 1.0, y: 2.0},
    )
  }
  testWith(~setup, s, tester)
}

let defaultAssert = (x, y, ~operator, ~message) => {
  let f = (a, b) => a == b
  assertion(f, x, y, ~operator, ~message)
}

testWithCoords("getField", ((input, output)) => {
  let happyOutput = output.x->Js.Json.number

  let result = input->ParseInput.getField("x")

  defaultAssert(
    result,
    Ok(happyOutput),
    ~operator="getField",
    ~message="Get field should return Ok(field)",
  )

  let sadResult = input->ParseInput.getField("z")

  defaultAssert(
    sadResult,
    Error(`missing field "z"`),
    ~operator="getField",
    ~message="Get field should return Error(message)",
  )
})

testWithCoords("Parse coordinates", ((o, output)) => {
  let result = ParseInput.parseCoords(o->Js.Json.object_)

  defaultAssert(
    result,
    Ok(output),
    ~operator="parseCoords",
    ~message="Parse coordinates should return Ok(coords)",
  )

  let sadResult = ParseInput.parseCoords(1.0->Js.Json.number)

  defaultAssert(
    sadResult,
    Error("Coords is not an object"),
    ~operator="parseCoords",
    ~message="Parse coordinates should return Error(message)",
  )
})

test("parseProtocol", () => {
  let happyInputs =
    [
      "closest-enemies",
      "furthest-enemies",
      "assist-allies",
      "avoid-crossfire",
      "prioritize-mech",
      "avoid-mech",
    ]->Js.Array2.map(s => s->Js.Json.string->ParseInput.parseProtocol)

  let happyOutputs: array<result<Types.protocol, string>> = [
    Ok(#"closest-enemies"),
    Ok(#"furthest-enemies"),
    Ok(#"assist-allies"),
    Ok(#"avoid-crossfire"),
    Ok(#"prioritize-mech"),
    Ok(#"avoid-mech"),
  ]

  defaultAssert(
    happyInputs,
    happyOutputs,
    ~operator="parseProtocol",
    ~message="Parse protocol should return Ok(protocol)",
  )

  let sadResult = ParseInput.parseProtocol(Js.Json.string("ftp"))

  defaultAssert(
    sadResult,
    Error("unknown protocol: ftp"),
    ~operator="parseProtocol",
    ~message="Parse protocol should return Error(message)",
  )
})

test("parse enemy type", () => {
  let happyInputs =
    ["soldier", "mech"]->Js.Array2.map(s => s->Js.Json.string->ParseInput.parseEnemy)

  let happyOutputs: array<result<Types.enemy, string>> = [Ok(#soldier), Ok(#mech)]

  defaultAssert(
    happyInputs,
    happyOutputs,
    ~operator="parseEnemy",
    ~message="Parse enemy should return Ok(enemy)",
  )

  let sadResult = ParseInput.parseEnemy(Js.Json.string("stormtrooper"))

  defaultAssert(
    sadResult,
    Error("unknown enemy type: stormtrooper"),
    ~operator="parseEnemy",
    ~message="Parse enemy should return Error(message)",
  )
})

test("Parse Enemy Payload", () => {
  let s = `{"type":"soldier","number":10}`
  let o = s->Js.Json.parseExn
  defaultAssert(
    ParseInput.parseEnemies(o),
    Ok({type_: #soldier, number: 10}),
    ~operator="parseEnemyPayload",
    ~message="Parse enemy payload should return Ok(enemyPayload)",
  )
})

test("parseScan", () => {
  let withoutAllies = `
  {
    "coordinates": {
      "x":1.0,
      "y":2.0
    },
    "enemies": {
      "type":"mech",
      "number":1
    }
  }`

  let withoutAlliesOutput: Types.scan = {
    coordinates: {x: 1.0, y: 2.0},
    enemies: {type_: #mech, number: 1},
    allies: None,
  }

  defaultAssert(
    ParseInput.parseScan(withoutAllies->Js.Json.parseExn),
    Ok(withoutAlliesOutput),
    ~operator="parseScan",
    ~message="Parse scan should return Ok(scan), with allies as None",
  )

  let withAllies = `
  {
    "coordinates": {
      "x": 1.0,
      "y": 2.0
    },
    "enemies": {
      "type": "mech",
      "number": 1
    },
    "allies": 2
  }
  `

  let withAlliesOutput: Types.scan = {
    coordinates: {x: 1.0, y: 2.0},
    enemies: {type_: #mech, number: 1},
    allies: Some(2),
  }

  defaultAssert(
    ParseInput.parseScan(withAllies->Js.Json.parseExn),
    Ok(withAlliesOutput),
    ~operator="parseScan",
    ~message="Parse scan should return Ok(scan), with allies as Some(allies)",
  )
})

test("paseInput", () => {
  let s = `{"protocols":["avoid-mech"],"scan":[{"coordinates":{"x":0,"y":40},"enemies":{"type":"soldier","number":10}},{"coordinates":{"x":0,"y":80},"allies":5,"enemies":{"type":"mech","number":1}}]}`

  defaultAssert(
    ParseInput.parseInp(s),
    Ok({
      protocols: [#"avoid-mech"],
      scan: [
        {
          coordinates: {x: 0.0, y: 40.0},
          enemies: {type_: #soldier, number: 10},
          allies: None,
        },
        {
          coordinates: {x: 0.0, y: 80.0},
          enemies: {type_: #mech, number: 1},
          allies: Some(5),
        },
      ],
    }),
    ~operator="parseInput",
    ~message="Parse input should return Ok(input)",
  )
})
