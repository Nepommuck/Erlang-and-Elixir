["hello", "world"]
|> Enum.map( fn(word) -> word |> String.capitalize() end )
|> Kernel.then(fn([a, b]) -> a <> ", " <> b <> "!" end)
|> IO.inspect()
