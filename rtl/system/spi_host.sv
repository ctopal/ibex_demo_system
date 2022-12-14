// Copyright lowRISC contributors.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

module spi_host #(
  parameter SPI_FRQ_MHZ = 25,
  parameter CPOL = 0,
  parameter CPHA = 0
)(
    input clk_i,
    input rst_ni,

    input  logic spi_rx_i,
    output logic spi_tx_o,
    output logic sck_o,

    input  logic start_i,
    input  logic [7:0] byte_data_i,
    output logic [7:0] byte_data_o,
    output logic next_tx_byte_o
  );

  localparam int Divisor = 50 / SPI_FRQ_MHZ;

  typedef enum logic[1:0] {
    IDLE,
    START,
    SEND,
    STOP
  } spi_state_t;

  spi_state_t state_q, state_d;

  logic [2:0] bit_counter_q, bit_counter_d;
  logic [7:0] current_byte_q, current_byte_d, recieved_byte_d, recieved_byte_q;

  // 50Mhz sys clock divided into 25MHz SPI clock
  logic clk_spi, clk_fast_spi;
  prim_clock_div #(
    .Divisor(Divisor)
  ) u_spi_fast_div (
    .clk_i          (clk_i),
    .rst_ni         (1'b1),
    .step_down_req_i('0),
    .step_down_ack_o(),
    .test_en_i      ('0),
    .clk_o          (clk_fast_spi)
  );

  prim_clock_div #(
    .Divisor(2)
  ) u_spi_div (
    .clk_i          (clk_fast_spi),
    .rst_ni         (1'b1),
    .step_down_req_i('0),
    .step_down_ack_o(),
    .test_en_i      ('0),
    .clk_o          (clk_spi)
  );

  // Use faster clock to catch the negedge of the slower clock
  logic sck_negedge_active;
  always_ff @(posedge clk_fast_spi) begin
    sck_negedge_active <= ~sck_negedge_active;
  end

  always_comb begin
    spi_tx_o         = 1'b0;
    bit_counter_d  = bit_counter_q;
    current_byte_d = current_byte_q;
    next_tx_byte_o = 1'b0;
    state_d        = state_q;
    byte_data_o    = '0;

    case (state_q)
      IDLE: begin
        spi_tx_o = 1'b1;
        if (start_i) begin
          state_d = START;
        end
      end
      START: begin
        state_d        = SEND;
        bit_counter_d  = 3'd7;
        current_byte_d = byte_data_i;
        // Set this earlier to prevent timing errors for the first bit of transmission
        spi_tx_o = byte_data_i[7];
      end
      SEND: begin
        spi_tx_o = current_byte_q[7];
        current_byte_d = {current_byte_q[6:0], 1'b0};
        if (bit_counter_q == 3'd0) begin
          state_d = STOP;
        end else begin
          bit_counter_d = bit_counter_q - 3'd1;
        end
      end
      STOP: begin
        spi_tx_o = 1'b1;
        next_tx_byte_o   = 1'b1;
        byte_data_o = recieved_byte_q;
        state_d = IDLE;
      end
    endcase
  end

  generate
    // If CPHA is HIGH, incoming data will be sampled on the falling edge while outgoing
    // data will get shifted out on the rising edge.
    if (CPHA) begin
      always_ff @(posedge clk_spi or negedge rst_ni) begin : data_shift_out
        if (!rst_ni) begin
          state_q <= IDLE;
          bit_counter_q <= '0;
          recieved_byte_q <= '0;
        end else begin
          state_q <= state_d;
          bit_counter_q <= bit_counter_d;
          recieved_byte_q <= recieved_byte_d;
        end
      end
      always_ff @(posedge clk_fast_spi or negedge rst_ni) begin : data_sample_in
        if (!rst_ni) begin
          current_byte_q <= '0;
        end else if (sck_negedge_active) begin
          if (state_q == SEND) begin
            recieved_byte_d <= {recieved_byte_q[6:0], spi_rx_i};
          end
          // Change the current byte on a "negedge" to ensure transmitted data gets
          // filled before the rising edge of the SCK
          current_byte_q <= current_byte_d;
        end
      end
    end
    // If CPHA is LOW, incoming data will be sampled on the rising edge while outgoing
    // data will get shifted out on the falling edge.
    else begin
      always_ff @(posedge clk_spi or negedge rst_ni) begin : data_sample_in
        if (!rst_ni) begin
          current_byte_q <= '0;
        end else begin
          // Change the current byte on a "posedge" to ensure transmitted data gets
          // filled before the falling edge of the SCK
          current_byte_q <= current_byte_d;
        end
        if (state_q == SEND) begin
          recieved_byte_d <= {recieved_byte_q[6:0], spi_rx_i};
        end
      end
      always_ff @(posedge clk_fast_spi or negedge rst_ni) begin : data_shift_out
        if (!rst_ni) begin
          state_q <= IDLE;
          recieved_byte_q <= '0;
          bit_counter_q <= '0;
        end else if (sck_negedge_active) begin
          bit_counter_q <= bit_counter_d;
          recieved_byte_q <= recieved_byte_d;
          state_q <= state_d;
        end
      end
    end
    // If CPOL is HIGH, clock in Idle state is HIGH.
    logic sck_en;
    assign sck_en = (state_d == SEND);
    if (CPOL) begin
      logic sck_d;
      prim_clock_gating sck_gate_i (
      .clk_i    (clk_spi),
      .en_i     (sck_en),
      .test_en_i('0),
      .clk_o    (sck_d)
      );
      assign sck_o = sck_en ? sck_d : '1;
    end else begin
      prim_clock_gating sck_gate_i (
      .clk_i    (clk_spi),
      .en_i     (sck_en),
      .test_en_i('0),
      .clk_o    (sck_o)
      );
    end
  endgenerate

endmodule
