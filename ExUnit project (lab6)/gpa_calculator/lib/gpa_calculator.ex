defmodule GPACalculator do
  @moduledoc """
  GPA calculator with a few additional features.
  """

  @doc """
  Creates an empty list.

  ## Examples

    iex> GPACalculator.init
    {:ok, []}
    
  """
  def init do
    {:ok, []}
  end

  @doc """
  Adds the grade to the list of grades.

  ## Examples

    iex> GPACalculator.add_grade("ASD", 6, 2.0, [])
    {:ok, [{"ASD", 6, 2.0}]}

  """
  def add_grade(course, weight, grade, grade_list) do
    if Enum.any?(grade_list, fn({c, _, _}) -> c == course end) do
      {:error, :course_already_in_the_list}
    else
      {:ok, [{course, weight, grade} | grade_list]}
    end
  end

  @doc """
  Removes the grade from the list of grades.

  #Examples

    iex> GPACalculator.remove_grade("Analiza", [{"Analiza", 6, 2.0}])
    {:ok, []}

  """
  def remove_grade(course, grade_list) do
    if Enum.all?(grade_list, fn({c, _, _}) -> c != course end) do
      {:error, :course_not_in_the_list}
    else
      {:ok, Enum.filter(grade_list, fn({c, _, _}) -> c != course end)}
    end
  end

  @doc """
  Resets the list of grades.

  ##Examples

    iex> GPACalculator.reset([{"Whatever", 1, 3.5}])
    {:ok, []}

  """
  def reset(_) do
    {:ok, []}
  end

  @doc """
  Calculates the GPA from the grades in the list.

  ##Examples

    iex> GPACalculator.calculate([{"Analiza", 6, 3.0}, {"ASD", 6, 2.0}, {"Elixir", 5, 5.0}])
    {:ok, 3.24}

  """
  def calculate([]) do
    {:error, :no_grades_in_the_list}
  end

  def calculate(grade_list) do
    {weight_sum, sum} = Enum.reduce(grade_list, {0, 0}, fn({_, w, g}, {w_acc, g_acc}) -> {w + w_acc, g * w + g_acc} end)
    {:ok, Float.round(sum / weight_sum, 2)}
  end

  @doc """
  Calculates the GPA from the grades in the list
  and sends it to the process with the specified pid.
  """
  def calculate_and_send(pid, grade_list) do
    result = calculate grade_list
    send(pid, result)
    case result do
      {:ok, _} -> :ok
      error -> error
    end
  end
end
