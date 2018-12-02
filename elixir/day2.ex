defmodule Day2 do
  def inc_entry(val, map) do
    Map.update(map, val, 1, fn x -> x + 1 end)
  end

  def frequencies(coll) do
    Enum.reduce(coll, Map.new(), &inc_entry/2)
  end

  def freq_set(word) do
    word
    |> String.to_charlist()
    |> frequencies()
    |> Map.values()
    |> MapSet.new()
  end

  def words_with_n_same(words, n) do
    words
    |> Enum.map(&freq_set/1)
    |> Enum.count(fn set -> MapSet.member?(set, n) end)
  end

  def distance(word1, word2) do
    [word1, word2]
    |> Enum.map(&String.to_charlist/1)
    |> Enum.zip()
    |> Enum.map(fn {a, b} -> if a == b, do: 0, else: 1 end)
    |> Enum.sum()
  end

  def single_distance(words) do
    [match_pair | _] =
      for x <- words,
          y <- words,
          distance(x, y) == 1 do
        [x, y]
      end

    match_pair
  end

  def same_chars(word_pair) do
    word_pair
    |> Enum.map(&String.to_charlist/1)
    |> Enum.zip()
    |> Enum.filter(fn {a, b} -> a == b end)
    |> Enum.map(fn {a, _} -> a end)
    |> to_string()
  end
end

words =
  "inputs/day2.txt"
  |> File.read!()
  |> String.splitter(["\n"], trim: true)

checksum = Day2.words_with_n_same(words, 2) * Day2.words_with_n_same(words, 3)

box_ids =
  words
  |> Day2.single_distance()
  |> Day2.same_chars()
