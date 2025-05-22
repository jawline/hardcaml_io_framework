open! Core
open Hardcaml
open Hardcaml_axi
open Hardcaml_uart
open Hardcaml_io_framework
open Hardcaml_test_harness
open! Bits

module Internal_bus = Internal_bus.Make (struct
    let data_bits = 32
    let addr_bits = 32
  end)

let test ~clock_frequency ~baud_rate ~include_parity_bit ~stop_bits ~packet =
  let all_inputs =
    (* We add the header and then the packet length before the packet *)
    let packet = String.to_list packet in
    let packet_len_parts =
      Bits.of_int_trunc ~width:16 (List.length packet)
      |> split_msb ~part_width:8
      |> List.map ~f:Bits.to_int_trunc
    in
    [ Char.to_int 'Q' ] @ packet_len_parts @ List.map ~f:Char.to_int packet
  in
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
  let module Register_interface = Register_interface.Make (Axi8) (Internal_bus) in
  let module Machine = struct
    open Signal

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; data_in_valid : 'a
        ; data_in : 'a [@bits 8]
        ; slave_to_master : 'a Internal_bus.Slave_to_master.t
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { dn : 'a Axi8.Source.t
        ; master_to_slave : 'a Internal_bus.Master_to_slave.t
        }
      [@@deriving hardcaml]
    end

    let create
          (scope : Scope.t)
          { I.clock; clear; data_in_valid; data_in; slave_to_master }
      =
      let { Uart_tx.O.uart_tx; _ } =
        Uart_tx.hierarchical scope { Uart_tx.I.clock; clear; data_in_valid; data_in }
      in
      let { Uart_rx.O.data_out_valid; data_out; parity_error = _ } =
        Uart_rx.hierarchical scope { Uart_rx.I.clock; clear; uart_rx = uart_tx }
      in
      let { Serial_to_packet.O.dn; up_ready = _ } =
        Serial_to_packet.hierarchical
          scope
          { Serial_to_packet.I.clock
          ; clear
          ; in_valid = data_out_valid
          ; in_data = data_out
          ; dn = { tready = vdd }
          }
      in
      let register_interface =
        Register_interface.hierarchical
          scope
          { Register_interface.I.clock
          ; clear
          ; up = dn
          ; dn = { tready = vdd }
          ; slave_to_master
          }
      in
      { O.dn = register_interface.dn
      ; master_to_slave = register_interface.master_to_slave
      }
    ;;
  end
  in
  let module Harness = Cyclesim_harness.Make (Machine.I) (Machine.O) in
  let create_sim f =
    Harness.run
      ~waves_config:Waves_config.No_waves
      ~create:Machine.create
      (fun ~inputs:_ ~outputs:_ sim -> f sim)
  in
  create_sim (fun sim ->
    let inputs : _ Machine.I.t = Cyclesim.inputs sim in
    let outputs : _ Machine.O.t = Cyclesim.outputs ~clock_edge:Before sim in
    let check_regif_state () =
      inputs.slave_to_master.read_ready := gnd;
      if Bits.to_bool !(outputs.master_to_slave.read_valid)
      then (
        inputs.slave_to_master.read_ready := vdd;
        inputs.slave_to_master.read_data
        := Bits.of_int_trunc
             ~width:32
             (Bits.to_int_trunc !(outputs.master_to_slave.address) + 1));
      if Bits.to_bool !(outputs.master_to_slave.write_valid)
      then (
        inputs.slave_to_master.write_ready := vdd;
        print_s
          [%message
            "WR request "
              ~address:(to_int_trunc !(outputs.master_to_slave.address) : Int.Hex.t)
              ~data:(to_int_trunc !(outputs.master_to_slave.write_data) : Int.Hex.t)]);
      ()
    in
    let check_dn () =
      if Bits.to_bool !(outputs.dn.tvalid)
      then
        print_s
          [%message
            ""
              ~_:
                (Axi8.Source.map ~f:(fun t -> Bits.to_int_trunc !t) outputs.dn
                 : int Axi8.Source.t)];
      ()
    in
    (* The fifo needs a clear cycle to initialize *)
    inputs.clear := vdd;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    inputs.clear := gnd;
    let rec loop_for n =
      if n = 0
      then ()
      else (
        check_regif_state ();
        check_dn ();
        Cyclesim.cycle sim;
        loop_for (n - 1))
    in
    List.iter
      ~f:(fun input ->
        inputs.data_in_valid := vdd;
        inputs.data_in := of_int_trunc ~width:8 input;
        check_regif_state ();
        check_dn ();
        Cyclesim.cycle sim;
        inputs.data_in_valid := gnd;
        loop_for 44)
      all_inputs;
    loop_for 200)
;;

let%expect_test "read tests" =
  test
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:false
    ~stop_bits:1
    ~packet:"\x00\x00\x00\x00\x00";
  [%expect
    {|
    ((tvalid 1) (tdata 0) (tkeep 1) (tstrb 1) (tlast 0) (tuser 0))
    ((tvalid 1) (tdata 0) (tkeep 1) (tstrb 1) (tlast 0) (tuser 0))
    ((tvalid 1) (tdata 0) (tkeep 1) (tstrb 1) (tlast 0) (tuser 0))
    ((tvalid 1) (tdata 1) (tkeep 1) (tstrb 1) (tlast 1) (tuser 0))
    |}];
  test
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:false
    ~stop_bits:1
    ~packet:"\x00\x00\x00\x00\x01";
  [%expect
    {|
    ((tvalid 1) (tdata 0) (tkeep 1) (tstrb 1) (tlast 0) (tuser 0))
    ((tvalid 1) (tdata 0) (tkeep 1) (tstrb 1) (tlast 0) (tuser 0))
    ((tvalid 1) (tdata 0) (tkeep 1) (tstrb 1) (tlast 0) (tuser 0))
    ((tvalid 1) (tdata 2) (tkeep 1) (tstrb 1) (tlast 1) (tuser 0))
    |}];
  test
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:false
    ~stop_bits:1
    ~packet:"\x00\x0A\x0B\x0C\x02";
  [%expect
    {|
    ((tvalid 1) (tdata 10) (tkeep 1) (tstrb 1) (tlast 0) (tuser 0))
    ((tvalid 1) (tdata 11) (tkeep 1) (tstrb 1) (tlast 0) (tuser 0))
    ((tvalid 1) (tdata 12) (tkeep 1) (tstrb 1) (tlast 0) (tuser 0))
    ((tvalid 1) (tdata 3) (tkeep 1) (tstrb 1) (tlast 1) (tuser 0))
    |}]
;;

let%expect_test "write tests" =
  test
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:false
    ~stop_bits:1
    ~packet:"\x01\x00\x00\x00\x00\x00\x00\x00\x01";
  [%expect
    {|
    ("WR request " (address 0x0) (data 0x1))
    ("WR request " (address 0x0) (data 0x1))
    |}];
  test
    ~clock_frequency:200
    ~baud_rate:50
    ~include_parity_bit:false
    ~stop_bits:1
    ~packet:"\x01\x00\xc0\x00\x00\x11\x00\x00\x01";
  [%expect
    {|
    ("WR request " (address 0xc00000) (data 0x11000001))
    ("WR request " (address 0xc00000) (data 0x11000001))
    |}]
;;
