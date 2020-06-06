defmodule GpaCalculatorTest do
  use ExUnit.Case
  doctest GPACalculator

  setup_all do
    {:ok, example1: [{"WDI", 5, 4.0}, {"sysopy", 4, 2.0}, {"TAIJF", 3, 4.5}, {"Python", 3, 5.0}],
          example2: [{"Analiza", 6, 3.0}, {"ASD", 6, 2.0}, {"Elixir", 5, 5.0}]}
  end

  test "add_grade", state do
    assert GPACalculator.add_grade("Analiza", 6, 5.0, state[:example2]) == 
      {:error, :course_already_in_the_list}
    assert GPACalculator.add_grade("Analiza", 6, 5.0, state[:example1]) == 
      {:ok, [{"Analiza", 6, 5.0}, {"WDI", 5, 4.0}, {"sysopy", 4, 2.0}, {"TAIJF", 3, 4.5}, {"Python", 3, 5.0}]}
  end

  test "remove_grade", state do
    assert GPACalculator.remove_grade("Python", state[:example2]) == 
      {:error, :course_not_in_the_list}
    assert GPACalculator.remove_grade("sysopy", state[:example1]) ==
      {:ok, [{"WDI", 5, 4.0}, {"TAIJF", 3, 4.5}, {"Python", 3, 5.0}]}
  end

  test "calculate", state do
    assert GPACalculator.calculate([]) == {:error, :no_grades_in_the_list}
    assert GPACalculator.calculate(state[:example1]) == {:ok, 3.77}
  end

  test "calculate_and_send", state do
    GPACalculator.calculate_and_send(self(), [])
    assert_received {:error, _}
    GPACalculator.calculate_and_send(self(), state[:example2])
    assert_received {:ok, 3.24}
  end
end
