defmodule TRYMIXNOTP.RegistryTest do
  use ExUnit.Case, async: true

  setup context do
    {:ok, registry} = TRYMIXNOTP.Registry.start_link(context.test)
    {:ok, registry: registry}
  end

  test "spawns buckets", %{registry: registry} do
    assert TRYMIXNOTP.Registry.lookup(registry, "shopping") == :error

    TRYMIXNOTP.Registry.create(registry, "shopping")
    assert {:ok, bucket} = TRYMIXNOTP.Registry.lookup(registry, "shopping") 

    TRYMIXNOTP.Bucket.put(bucket, "milk", 1)
    assert TRYMIXNOTP.Bucket.get(bucket, "milk") == 1
  end

  test "remove buckets on exit", %{registry: registry} do
    TRYMIXNOTP.Registry.create(registry, "shopping")
    {:ok, bucket} = TRYMIXNOTP.Registry.lookup(registry, "shopping")
    Agent.stop(bucket)
    assert TRYMIXNOTP.Registry.lookup(registry, "shopping") == :error
  end
end
