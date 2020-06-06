defmodule Sender do
    def send_ping(pid) do
        send(pid, :ping)
    end
end

defmodule Functionality do
    use ExUnit.Case
    import ExUnit.CaptureIO

    doctest Add

    setup_all do
        {:ok, ala: :ma_kota}
    end

    test "Assertions" do
        assert 5 > 4
        refute 4 > 5
        assert_raise ArithmeticError, fn -> 1/0 end       
    end

    test "assert_received" do
        Sender.send_ping(self())
        assert_received :ping
    end

    test "CaptureIO" do
        assert capture_io(fn -> IO.puts("Witaj") end) == "Witaj\n"
    end

    test "Callbacks", state do
        assert :ma_kota == state[:ala]
    end
end
