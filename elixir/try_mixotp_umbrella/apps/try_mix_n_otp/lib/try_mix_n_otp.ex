defmodule TRYMIXNOTP do
  use Application
  @moduledoc """
  Documentation for TRYMIXNOTP.
  """

  @doc """
  Hello world.

  ## Examples

      iex> TRYMIXNOTP.hello
      :world

  """
  def hello do
    :world
  end

  def start(_type, _args) do
    TRYMIXNOTP.Supervisor.start_link
  end
end

