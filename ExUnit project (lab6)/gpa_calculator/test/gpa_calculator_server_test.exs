defmodule GPACalculatorServerTest do
  use ExUnit.Case
  import ExUnit.CaptureIO

  test "state" do
    GPACalculatorServer.add_grade("Analiza", 3, 5.0)
    GPACalculatorServer.add_grade("ASD", 6, 4.0)
    assert GPACalculatorServer.calculate == {:ok, 4.33}
    GPACalculatorServer.remove_grade("ASD")
    GPACalculatorServer.calculate_and_send(self())
    assert_received {:ok, 5.0}
    assert GPACalculatorServer.get_list == {:ok, [{"Analiza", 3, 5.0}]}
    GPACalculatorServer.reset
    assert GPACalculatorServer.get_list == {:ok, []}
  end

  test "init" do
    assert capture_io(fn -> GPACalculatorServer.init([]) end) == "Witamy w kalkulatorze\n"
  end

  test "terminate" do
    assert capture_io(fn -> GPACalculatorServer.terminate(:normal, []) end) == "KONIEC\n"
  end
end