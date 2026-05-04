open! Core
open Hardcaml
open Hardcaml_waveterm
open Hardcaml_uart
open Hardcaml_io_framework
open! Bits

let debug = false

let test ~name ~clock_frequency ~baud_rate ~include_parity_bit ~stop_bits ~packets =
  let module Config = struct
    (* This should trigger a switch every other cycle. *)
    let config =
      { Hardcaml_uart.Config.clock_frequency; baud_rate; include_parity_bit; stop_bits }
    ;;
  end
  in
  let module Uart_tx = Uart_tx.Make (Config) in
  let module Uart_rx = Uart_rx.Make (Config) in
  let module Serial_to_packet =
    Serial_to_packet.Make
      (struct
        let header = 'Q'
      end)
      (Axi8)
  in
  let module Machine = struct
    open Signal

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; data_in_valid : 'a
        ; data_in : 'a [@bits 8]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { data_out_valid : 'a
        ; data_out : 'a [@bits 8]
        ; last : 'a
        ; parity_error : 'a
        ; tx_idle : 'a
        }
      [@@deriving hardcaml]
    end

    let create (scope : Scope.t) { I.clock; clear; data_in_valid; data_in } =
      let { Uart_tx.O.uart_tx; idle = tx_idle; _ } =
        Uart_tx.hierarchical
          scope
          { Uart_tx.I.clock = { clock; clear }; data_in_valid; data_in }
      in
      let { Uart_rx.O.data_out_valid; data_out; parity_error } =
        Uart_rx.hierarchical
          scope
          { Uart_rx.I.clock = { clock; clear }; uart_rx = uart_tx }
      in
      let { Serial_to_packet.O.dn; up_ready = _ } =
        Serial_to_packet.hierarchical
          scope
          { Serial_to_packet.I.clock = { clock; clear }
          ; in_valid = data_out_valid
          ; in_data = data_out
          ; dn = { tready = vdd }
          }
      in
      { O.data_out_valid = dn.tvalid
      ; data_out = dn.tdata
      ; last = dn.tlast
      ; parity_error
      ; tx_idle
      }
    ;;
  end
  in
  let open Hardcaml_step_testbench in
  let module Tb = Functional.Cyclesim.Make (Machine.I) (Machine.O) in
  let create_sim () =
    let module Sim = Cyclesim.With_interface (Machine.I) (Machine.O) in
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Machine.create
         (Scope.create ~auto_label_hierarchical_ports:true ~flatten_design:true ()))
  in
  let sim = create_sim () in
  let waveform, sim = Waveform.create sim in
  let rec send_inputs handler = function
    | [] -> ()
    | packet :: xs ->
      let input_bytes =
        (* We add the header and then the packet length before the packet *)
        let packet = String.to_list packet in
        let packet_size = List.length packet in
        let packet_len_msb = packet_size land 0xFF00 in
        let packet_len_lsb = packet_size land 0x00FF in
        [ Char.to_int 'Q'; packet_len_msb; packet_len_lsb ]
        @ List.map ~f:Char.to_int packet
      in
      let rec consume_bytes handler = function
        | [] -> ()
        | x :: xs ->
          Tb.cycle handler
            { Tb.input_zero with
              data_in_valid = vdd
            ; data_in = of_int_trunc ~width:8 x
            }
          |> ignore;
          let rec wait_until_idle handler =
            let output = Tb.cycle handler Tb.input_hold in
            if to_bool output.after_edge.tx_idle then () else wait_until_idle handler
          in
          wait_until_idle handler;
          consume_bytes handler xs
      in
      consume_bytes handler input_bytes;
      send_inputs handler xs
  in
  let receive_packet handler =
    let output_bytes = ref [] in
    let cycles = ref 0 in
    let rec loop handler =
      let output = Tb.cycle handler Tb.input_hold in
      Core.incr cycles;
      let output = output.before_edge in
      if to_bool output.data_out_valid
      then output_bytes := to_int_trunc output.data_out :: !output_bytes;
      if to_bool output.data_out_valid && to_bool output.last
      then (
        let result =
          List.rev !output_bytes |> List.map ~f:Char.of_int_exn |> String.of_char_list
        in
        print_s [%message "Received packet in" ~cycles:(!cycles : int)];
        result)
      else loop handler
    in
    loop handler
  in
  let receive_packets handler ~n =
    let rec inner handler ~n =
      if n = 0
      then []
      else (
        let next = receive_packet handler in
        print_s [%message "Next packet" (next : string)];
        let succ = inner handler ~n:(n - 1) in
        next :: succ)
    in
    inner handler ~n
  in
  let testbench handler _initial_output =
    Tb.cycle handler { Tb.input_zero with clear = vdd } |> ignore;
    let result_event =
      Tb.spawn handler
        (fun spawned_handler _output ->
          receive_packets spawned_handler ~n:(List.length packets))
    in
    send_inputs handler packets;
    let result = Tb.wait_for handler result_event in
    if debug then Waveform.Serialize.marshall waveform name;
    if not (List.equal String.equal result packets)
    then
      raise_s
        [%message
          "BUG: Inputs and outputs diverge"
            (packets : string list)
            (result : string list)];
    print_s [%message "PASSED"];
    ()
  in
  Option.value_exn (Tb.run_with_timeout ~timeout:5000 ~simulator:sim ~testbench ())
;;

let%expect_test "test" =
  test
    ~name:"/tmp/test_serial_to_packet_no_parity_hello_world"
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:false
    ~stop_bits:1
    ~packets:[ "Hello world"; "Goodbye world"; "Yet another packet!!!!!!" ];
  [%expect
    {|
    ("Received packet in" (cycles 575))
    ("Next packet" (next "Hello world"))
    ("Received packet in" (cycles 656))
    ("Next packet" (next "Goodbye world"))
    ("Received packet in" (cycles 1107))
    ("Next packet" (next "Yet another packet!!!!!!"))
    PASSED
    |}];
  test
    ~name:"/tmp/test_serial_to_packet_parity_hello_world"
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:true
    ~stop_bits:1
    ~packets:[ "Hello world"; "Goodbye world"; "Yet another packet!!!!!!" ];
  [%expect
    {|
    ("Received packet in" (cycles 631))
    ("Next packet" (next "Hello world"))
    ("Received packet in" (cycles 720))
    ("Next packet" (next "Goodbye world"))
    ("Received packet in" (cycles 1215))
    ("Next packet" (next "Yet another packet!!!!!!"))
    PASSED
    |}];
  test
    ~name:"/tmp/test_serial_to_packet_no_parity_a"
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:false
    ~stop_bits:1
    ~packets:[ "Hello world"; "Goodbye world"; "Yet another packet!!!!!!" ];
  [%expect
    {|
    ("Received packet in" (cycles 575))
    ("Next packet" (next "Hello world"))
    ("Received packet in" (cycles 656))
    ("Next packet" (next "Goodbye world"))
    ("Received packet in" (cycles 1107))
    ("Next packet" (next "Yet another packet!!!!!!"))
    PASSED
    |}];
  test
    ~name:"/tmp/test_serial_to_packet_parity_a"
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:true
    ~stop_bits:1
    ~packets:[ "Hello world"; "Goodbye world"; "Yet another packet!!!!!!" ];
  [%expect
    {|
    ("Received packet in" (cycles 631))
    ("Next packet" (next "Hello world"))
    ("Received packet in" (cycles 720))
    ("Next packet" (next "Goodbye world"))
    ("Received packet in" (cycles 1215))
    ("Next packet" (next "Yet another packet!!!!!!"))
    PASSED
    |}]
;;
