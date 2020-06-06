defmodule GPACalculatorServer do
	use GenServer
  
	##### START #####
	def start_link(_) do
	  {:ok, state} = GPACalculator.init
	  GenServer.start_link(__MODULE__, state, name: __MODULE__)
	end
  
	def init(state) do 
	  IO.puts("Witamy w kalkulatorze")
	  {:ok, state}
	end
  
  ##### INTERFACE #####
  @doc """
  Stops the server.
  """
	def stop do
	  GenServer.call(__MODULE__, :terminate)
  end
  
  @doc """
  Returns the list of grades.
  """
  def get_list do
    GenServer.call(__MODULE__, :get)
  end

  @doc """
  Adds the grade to the list of grades.
  """
	def add_grade(course, weight, grade) do 
	  GenServer.call(__MODULE__, {:add, {course, weight, grade}})
	end
  
  @doc """
  Removes the grade from the list of grades.
  """
	def remove_grade(course) do
	  GenServer.call(__MODULE__, {:remove, {course}})
	end
  
  @doc """
  Resets the list of grades.
  """
	def reset do
	  GenServer.call(__MODULE__, {:reset, {}})
	end
  
  @doc """
  Calculates the GPA from the grades in the list.
  """
	def calculate do
	  GenServer.call(__MODULE__, {:calculate, {}})
	end
  
  @doc """
  Calculates the GPA from the grades in the list
  and sends it to the process with the specified pid.
  """
	def calculate_and_send(pid) do
	  GenServer.call(__MODULE__, {:calculate_and_send, {pid}})
	end
  
	##### HANDLING #####
  def handle_call(:terminate, _from, grade_list), do: {:stop, :normal, :ok, grade_list}
  
  def handle_call(:get, _from, grade_list), do: {:reply, {:ok, grade_list}, grade_list}
  
	def handle_call({:add, {course, weight, grade}}, _from, grade_list) do
	  handle_helper(grade_list, {:new, GPACalculator.add_grade(course, weight, grade, grade_list)})
	end
  
	def handle_call({:remove, {course}}, _from, grade_list) do
	  handle_helper(grade_list, {:new, GPACalculator.remove_grade(course, grade_list)})
	end
  
	def handle_call({:reset, {}}, _from, grade_list) do
	  handle_helper(grade_list, {:new, GPACalculator.reset(grade_list)})
	end
  
	def handle_call({:calculate, {}}, _from, grade_list) do
	  handle_helper(grade_list, {:res, GPACalculator.calculate(grade_list)})
	end
  
	def handle_call({:calculate_and_send, {pid}}, _from, grade_list) do
	  handle_helper(grade_list, {:res, GPACalculator.calculate_and_send(pid, grade_list)})
	end
  
  
	def handle_helper(old_list, {_, {:error, mes}}), do: {:reply, {:error, mes}, old_list}
	
	def handle_helper(_, {:new, {:ok, new_list}}), do: {:reply, :ok, new_list}
	
	def handle_helper(old_list, {:res, res}), do: {:reply, res, old_list}
  
	def terminate(:normal, _) do
	  IO.puts("KONIEC")
	  :ok
	end
end