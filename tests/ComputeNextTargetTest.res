open Test

let {defaultAssert} = module(ParserTest)

test("distance", () => {
  let c: Types.coords = {
    x: 3.0,
    y: 4.0,
  }
  defaultAssert(
    5.0,
    ComputeNextTarget.calculateDistance(c),
    ~operator="calculateDistance",
    ~message="distance is correct",
  )
})

test("cmp", () => {
  defaultAssert(ComputeNextTarget.cmp(1.0, 2.0), -1, ~operator="cmp", ~message="cmp is correct")
  defaultAssert(ComputeNextTarget.cmp(3.0, 2.0), 1, ~operator="cmp", ~message="cmp is correct")
  defaultAssert(ComputeNextTarget.cmp(3.0, 3.0), 0, ~operator="cmp", ~message="cmp is correct")
})

test("cmpDist", () => {
  defaultAssert(
    ComputeNextTarget.cmpDist(
      {
        enemies: {
          number: 0,
          type_: #mech,
        },
        allies: None,
        coordinates: {
          x: 3.0,
          y: 4.0,
        },
      },
      {
        enemies: {
          number: 0,
          type_: #mech,
        },
        allies: None,
        coordinates: {
          x: 3.0,
          y: 4.0,
        },
      },
    ),
    0,
    ~operator="cmpDist",
    ~message="cmpDist is correct",
  )
  defaultAssert(
    ComputeNextTarget.cmpDist(
      {
        enemies: {
          number: 0,
          type_: #mech,
        },
        allies: None,
        coordinates: {
          x: 4.0,
          y: 4.0,
        },
      },
      {
        enemies: {
          number: 0,
          type_: #mech,
        },
        allies: None,
        coordinates: {
          x: 3.0,
          y: 4.0,
        },
      },
    ),
    1,
    ~operator="cmpDist",
    ~message="cmpDist is correct",
  )
  defaultAssert(
    ComputeNextTarget.cmpDist(
      {
        enemies: {
          number: 0,
          type_: #mech,
        },
        allies: None,
        coordinates: {
          x: 3.0,
          y: 4.0,
        },
      },
      {
        enemies: {
          number: 0,
          type_: #mech,
        },
        allies: None,
        coordinates: {
          x: 4.0,
          y: 4.0,
        },
      },
    ),
    -1,
    ~operator="cmpDist",
    ~message="cmpDist is correct",
  )
})

test("cmpAllies", () => {
  let scan1: Types.scan = {
    enemies: {
      number: 0,
      type_: #mech,
    },
    allies: Some(1),
    coordinates: {
      x: 3.0,
      y: 4.0,
    },
  }

  let scan2: Types.scan = {
    enemies: {
      number: 0,
      type_: #mech,
    },
    allies: Some(2),
    coordinates: {
      x: 3.0,
      y: 4.0,
    },
  }

  let scan3: Types.scan = {
    enemies: {
      number: 0,
      type_: #mech,
    },
    allies: None,
    coordinates: {
      x: 3.0,
      y: 4.0,
    },
  }

  defaultAssert(
    ComputeNextTarget.cmpAllies(scan1, scan2),
    -1,
    ~operator="cmpAllies",
    ~message="cmpAllies is correct",
  )

  defaultAssert(
    ComputeNextTarget.cmpAllies(scan2, scan1),
    1,
    ~operator="cmpAllies",
    ~message="cmpAllies is correct",
  )

  defaultAssert(
    ComputeNextTarget.cmpAllies(scan1, scan3),
    1,
    ~operator="cmpAllies",
    ~message="cmpAllies is correct",
  )

  defaultAssert(
    ComputeNextTarget.cmpAllies(scan3, scan1),
    -1,
    ~operator="cmpAllies",
    ~message="cmpAllies is correct",
  )

  defaultAssert(
    ComputeNextTarget.cmpAllies(scan3, scan3),
    0,
    ~operator="cmpAllies",
    ~message="cmpAllies is correct",
  )
  defaultAssert(
    ComputeNextTarget.cmpAllies(scan2, scan2),
    0,
    ~operator="cmpAllies",
    ~message="cmpAllies is correct",
  )
})

test("cmpMech", () => {
  let scan1: Types.scan = {
    enemies: {
      number: 1,
      type_: #mech,
    },
    allies: Some(1),
    coordinates: {
      x: 3.0,
      y: 4.0,
    },
  }

  let scan2: Types.scan = {
    enemies: {
      number: 2,
      type_: #soldier,
    },
    allies: Some(1),
    coordinates: {
      x: 3.0,
      y: 4.0,
    },
  }

  let scan3: Types.scan = {
    enemies: {
      number: 5,
      type_: #mech,
    },
    allies: None,
    coordinates: {
      x: 3.0,
      y: 4.0,
    },
  }

  defaultAssert(
    ComputeNextTarget.cmpMech(scan1, scan2),
    1,
    ~operator="cmpMech",
    ~message="cmpMech is correct",
  )

  defaultAssert(
    ComputeNextTarget.cmpMech(scan1, scan3),
    -1,
    ~operator="cmpMech",
    ~message="cmpMech is correct",
  )

  defaultAssert(
    ComputeNextTarget.cmpMech(scan2, scan2),
    0,
    ~operator="cmpMech",
    ~message="cmpMech is correct",
  )
})
