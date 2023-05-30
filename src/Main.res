open Express

let app = expressCjs()

app->use(textMiddlewareWithOptions({"type": ["text/*", "application/json"]}))

app->post("/radar", (req, res) => {
  let body = req->body
  let inp = ParseInput.parse(body)
  let _ = switch inp {
  | Ok(i) => res->status(200)->json(ComputeNextTarget.computeNextTarget(i))
  | Error(s) => res->status(400)->json({"error": `Unable to parse input, ${s}`})
  }
})

app->useWithError((err, _req, res, _next) => {
  Console.error(err)
  let _ = res->status(500)->endWithData("An error occured")
})

let port = 8888
let _ = app->listenWithCallback(port, _ => {
  Console.log(`Listening on http://localhost:${port->Belt.Int.toString}`)
})
