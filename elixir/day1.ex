intervals =
  "inputs/day1.txt"
  |> File.read!()
  |> String.splitter(["\n"], trim: true)
  |> Enum.map(&String.to_integer/1)

Enum.sum(intervals)

seek_dup = fn val, set ->
  if MapSet.member?(set, val) do
    {:halt, val}
  else
    {:cont, MapSet.put(set, val)}
  end
end

intervals
|> Stream.cycle()
|> Stream.scan(&+/2)
|> Enum.reduce_while(MapSet.new([0]), seek_dup)
