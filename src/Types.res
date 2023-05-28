type protocol = [
  | #"closest-enemies"
  | #"furthest-enemies"
  | #"assist-allies"
  | #"avoid-crossfire"
  | #"prioritize-mech"
  | #"avoid-mech"
]

type enemy = [
  | #soldier
  | #mech
]

type coords = {x: float, y: float}

let coords = (x, y) => {x, y}

type enemies = {
  type_: enemy,
  number: int,
}

let enemies = (type_, number) => {
  type_,
  number,
}

type scan = {
  enemies: enemies,
  allies: option<int>,
  coordinates: coords,
}

let scan = (coordinates, enemies, allies) => {
  coordinates,
  enemies,
  allies,
}

type t = {
  protocols: array<protocol>,
  scan: array<scan>,
}

let make = (protocols, scan) => {
  protocols,
  scan,
}
