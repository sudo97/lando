module type Applicative = {
  type t<'a>
  let pure: 'a => t<'a>
  let apply: (t<'a => 'b>, t<'a>) => t<'b>
  let sequence: array<t<'a>> => t<array<'a>>
}

module Result = (
  SecondType: {
    type t
  },
): (Applicative with type t<'a> = result<'a, SecondType.t>) => {
  type b = SecondType.t
  type t<'a> = result<'a, b>
  let pure = x => Ok(x)
  let apply = (f, x) =>
    switch (f, x) {
    | (Ok(f), Ok(x)) => Ok(f(x))
    | (Error(e), _) => Error(e)
    | (_, Error(e)) => Error(e)
    }
  let sequence = (xs: array<result<'a, b>>): result<array<'a>, b> =>
    xs->Js.Array2.reduce((acc, curr) =>
      switch (acc, curr) {
      | (Ok(acc), Ok(curr)) => {
          acc->Js.Array2.push(curr)->ignore
          Ok(acc)
        }
      | (Error(e), _) => Error(e)
      | (_, Error(e)) => Error(e)
      }
    , Ok([]))
}
