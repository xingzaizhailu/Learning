defmodule TRYMIXNOTP.BucketTest do
  use ExUnit.Case, async: true    
  # async: true - makes the test case run in paraller with other :async test cases.
  # async must only be set if the test case does not rely on or change any globalvalues.

  #test "stores values by key" do
  #  {:ok, bucket} = TRYMIXNOTP.Bucket.start_link
  #  assert TRYMIXNOTP.Bucket.get(bucket, "milk") ==nil

  #  TRYMIXNOTP.Bucket.put(bucket, "milk", 3)
  #  assert TRYMIXNOTP.Bucket.get(bucket, "milk") == 3
  #end

  # Rewrite version
  setup do
    {:ok, bucket} = TRYMIXNOTP.Bucket.start_link
    {:ok, bucket: bucket}
  end

  test "stores values by key", %{bucket: bucket} do
    assert TRYMIXNOTP.Bucket.get(bucket, "milk") ==nil

    TRYMIXNOTP.Bucket.put(bucket, "milk", 3)
    assert TRYMIXNOTP.Bucket.get(bucket, "milk") == 3

    milk = TRYMIXNOTP.Bucket.delete(bucket, "milk")
    assert milk == 3
    assert TRYMIXNOTP.Bucket.get(bucket, "milk") == nil
  end
end
