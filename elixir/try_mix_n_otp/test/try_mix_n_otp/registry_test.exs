defmodule TRYMIXNOTP.RegistryTest do
  use ExUnit.Case, async: true

  setup context do
    # change to ETS
    #{:ok, registry} = TRYMIXNOTP.Registry.start_link(context.test)
    #{:ok, registry: registry}
    {:ok, _} = TRYMIXNOTP.Registry.start_link(context.test)
    {:ok, registry: context.test}
  end

  test "spawns buckets", %{registry: registry} do
    assert TRYMIXNOTP.Registry.lookup(registry, "shopping") == :error

    TRYMIXNOTP.Registry.create(registry, "shopping")
    assert {:ok, bucket} = TRYMIXNOTP.Registry.lookup(registry, "shopping") 

    TRYMIXNOTP.Bucket.put(bucket, "milk", 1)
    assert TRYMIXNOTP.Bucket.get(bucket, "milk") == 1
  end

  test "removes buckets on exit", %{registry: registry} do
    TRYMIXNOTP.Registry.create(registry, "shopping")
    {:ok, bucket} = TRYMIXNOTP.Registry.lookup(registry, "shopping")
    Agent.stop(bucket)

    # Do a call to ensure the registry processed the DOWN message
    # _ = TRYMIXNOTP.Registry.create(registry, "bogus")
    assert TRYMIXNOTP.Registry.lookup(registry, "shopping") == :error
  end

  test "removes buckets on crash", %{registry: registry} do
    TRYMIXNOTP.Registry.create(registry, "shopping")
    {:ok, bucket} = TRYMIXNOTP.Registry.lookup(registry, "shopping")

    # Stop the bucket with non-normal reason
    ref = Process.monitor(bucket)

    # Kill the bucket and wait for the notification
    Process.exit(bucket, :shutdown)

    # Wait until the bucket is dead
    assert_receive {:DOWN, ^ref, _, _, _}

    # Do a call to ensure the registry processed the DOWN message
    # _ = TRYMIXNOTP.Registry.create(registry, "bogus")
    assert TRYMIXNOTP.Registry.lookup(registry, "shopping") == :error
  end
end

