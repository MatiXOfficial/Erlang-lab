defmodule PollutionDataStream do

  def importLinesFromCSV(fileName) do
    File.read!(fileName) |> String.split("\n")
  end

  def convertOneLine(line) do
    [date, time, cx, cy, level] = String.split(line, ",")
    date = date |> String.split("-") |> Enum.reverse |> Stream.map(fn(x) -> elem(Integer.parse(x), 0) end) |> Enum.reduce([], fn x, acc -> [x] ++ acc end)
    time = time |> String.split(":") |> Stream.map(fn(x) -> elem(Integer.parse(x), 0) end) |> Enum.reduce([], fn x, acc -> [x] ++ acc end)
    time = time ++ [0]
    time = List.to_tuple(time)
    cx = elem(Float.parse(cx), 0)
    cy = elem(Float.parse(cy), 0)
    level = elem(Integer.parse(level), 0)
    %{:datetime => {date, time}, :location => {cx, cy}, :pollutionLevel => level}
  end

  def identifyStations(list) do
    list |> Stream.map(fn(x) -> x.location end) |> Stream.uniq()
  end

  def startServer() do
    :pollution_server_sup.start_link()
  end

  def loadStations(fileName) do
    importLinesFromCSV(fileName) |>
      Stream.map(fn(x) -> convertOneLine(x) end) |>
      identifyStations |>
      Enum.each(fn({x, y}) -> :pollution_gen_server.addStation("station_#{x}_#{y}", {x, y}) end)
  end

  def loadValues(fileName) do
    importLinesFromCSV(fileName) |>
      Stream.map(fn(x) -> convertOneLine(x) end) |>
      Enum.each(fn(x) -> :pollution_gen_server.addValue(x.location, x.datetime, "PM10", x.pollutionLevel) end)
  end

  def getStationMean(coords, what) do
    :pollution_gen_server.getStationMean(coords, what)
  end

  def getDayMean(day, what) do
    :pollution_gen_server.getDayMean(what, day)
  end

end
