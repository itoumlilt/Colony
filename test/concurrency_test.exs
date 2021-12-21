defmodule LruCacheNif.ConcurrencyTest do
    use ExUnit.Case

    #    alias Concurrency
    #
    #    @moduletag :capture_log
    #
    #    doctest Concurrency

    defmodule Operator do
        def spawn(name, target \\ nil) do
            target = target || self()
            Kernel.spawn(fn -> gather_loop(name, target, []) end)
        end

        def call(operator, f, a) do
            ref = make_ref()
            send(operator, {:"$call", ref, self(), {f, a}})
            receive do
                {:reply, ^ref, result} -> result
            end
        end

        def stop(operator) do
            send(operator, :stop)
        end

        def gather_loop(name, target, gathered) do
            receive do
                {:"$call", ref, pid, {f, a}} ->
                    result = retry_on_lock(f, a, 0)
                    send(pid, {:reply, ref, result})
                    gathered
                :stop ->
                    :stop
                :flush ->
                    results =
                        gathered
                        |> Enum.reverse()
                        |> Enum.reduce(
                               [],
                               fn {f, a}, results ->
                                   [retry_on_lock(f, a, 0) | results]
                               end
                           )
                        |> Enum.reverse()

                    send(target, {:results, name, results})

                    gather_loop(name, target, [])

                other ->
                    gather_loop(name, target, [other | gathered])
            end
        end

        def retry_on_lock(f, a, 10) do
            flunk("Unable to execute #{inspect(f)}(#{inspect(a)}) after 10 tries")
        end

        def retry_on_lock(f, a, count) do
            case apply(LruCacheNif, f, a) do
                {:error, :lock_fail} ->
                    retry_on_lock(f, a, count + 1)

                other ->
                    other
            end
        end
    end

    describe "operator" do
#        test "gathers operations and executes them in order" do
#            operator = Operator.spawn(:a)
#            {:ok, cache} = LruCacheNif.new(5)
#
#            send(operator, {:put, [cache, 1, 100]})
#            send(operator, {:to_list, [cache]})
#            send(operator, {:put, [cache, 2, 200]})
#            send(operator, {:to_list, [cache]})
#            send(operator, {:put, [cache, 3, 300]})
#            send(operator, {:to_list, [cache]})
#
#            refute_receive {:results, :a, _}
#
#            send(operator, :flush)
#
#            assert_receive {
#                :results,
#                :a,
#                [
#                    {:ok, nil},
#                    {:ok, [{1, 100}]},
#                    {:ok, nil},
#                    {:ok, [{2, 200}, {1, 100}]},
#                    {:ok, nil},
#                    {:ok, [{3, 300}, {2, 200}, {1, 100}]}
#                ]
#            }
#        end

        test "use cache from dead process" do
            operator = Operator.spawn(:a)
            {:ok, cache} = Operator.call(operator, :new, [5])
            Operator.stop(operator)
            #            :erlang.exit(operator, :normal)

            assert LruCacheNif.put(cache, 1, 100) == {:ok, nil}
            assert LruCacheNif.put(cache, 2, 200) == {:ok, nil}
            assert LruCacheNif.put(cache, 3, 300) == {:ok, nil}
            assert LruCacheNif.put(cache, 4, 400) == {:ok, nil}
            assert LruCacheNif.put(cache, 5, 500) == {:ok, nil}
            assert LruCacheNif.put(cache, 6, 600) == {:ok, {1, 100}}
            IO.puts "------------#{__ENV__.file}:#{__ENV__.line}-----------"
            IO.puts "is_process_alive(operator) = #{inspect :erlang.is_process_alive(operator)}"
            IO.puts "LruCacheNif.len(cache) = #{inspect LruCacheNif.len(cache)}"
            IO.puts "1111 = #{:erlang.system_time(:millisecond)}, #{inspect self()}"

            :timer.sleep(2000)

            IO.puts "2222 = #{:erlang.system_time(:millisecond)}, #{inspect self()}"

            IO.puts "-----------------------------------------------------------~n"
        end

        test "xxxxxxx" do
            IO.puts "3333 = #{:erlang.system_time(:millisecond)}, #{inspect self()}"

            :timer.sleep(2000)

            IO.puts "4444 = #{:erlang.system_time(:millisecond)}, #{inspect self()}"

            :timer.sleep(2000)
            IO.puts "5555 = #{:erlang.system_time(:millisecond)}, #{inspect self()}"
        end

        #        test "execute asynchronously" do
        #            operator = Operator.spawn(:a)
        #            {:ok, cache} = Operator.call(operator, {:new, 5})
        #
        #            operator = Operator.spawn(:b)
        #
        #            send(operator, {:put, [cache, 1, 100]})
        #            send(operator, {:to_list, [cache]})
        #            send(operator, {:put, [cache, 2, 200]})
        #            send(operator, {:to_list, [cache]})
        #            send(operator, {:put, [cache, 3, 300]})
        #            send(operator, {:to_list, [cache]})
        #
        #            send(operator, :flush)
        #        end
    end
end
