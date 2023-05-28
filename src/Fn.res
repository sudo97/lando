let on: ('a => 'b, ('b, 'b) => 'c, 'a, 'a) => 'c = (g, f, a, b) => f(g(a), g(b))
let flip: (('a, 'b) => 'c, 'b, 'a) => 'c = (f, a, b) => f(b, a)
