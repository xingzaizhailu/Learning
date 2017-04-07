defmodule HelloPhoenix.Users do 
  use Ecto.Schema

  schema "users" do 
    field :first_name, :string
    field :last_name, :string
    field :age, :integer
  end

  def changeset(users, params \\%{}) do
    users
    |> Ecto.Changeset.cast(params, [:first_name, :last_name, :age])
    |> Ecto.Changeset.validate_required([:first_name, :last_name])
  end
end
