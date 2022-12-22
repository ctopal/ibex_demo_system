// Copyright lowRISC contributors.
// Licensed under the Apache License, Version 2.0, see LICENSE for details.
// SPDX-License-Identifier: Apache-2.0

// This is the top level SystemVerilog file that connects the IO on the board to the Ibex Demo System.
module top_artya7 (
  // These inputs are defined in data/pins_artya7.xdc
  input               IO_CLK,
  input               IO_RST_N,
  input  [ 3:0]       SW,
  input  [ 3:0]       BTN,
  output [ 3:0]       LED,
  output [11:0]       RGB_LED,
  output [3:0]        DISP_CTRL,
  output              UART_TX,
  input               SPI_RX,
  output              SPI_TX,
  output              SPI_SCK
);
  parameter SRAMInitFile = "";

  logic clk_sys, rst_sys_n;

  `ifdef VERILATOR
    assign clk_sys = IO_CLK;
    assign rst_sys_n = IO_RST_N;
  `else
    // Generating the system clock and reset for the FPGA.
    clkgen_xil7series
      clkgen(
        .IO_CLK,
        .IO_RST_N,
        .clk_sys,
        .rst_sys_n
      );
  `endif

  // Instantiating the Ibex Demo System.
  ibex_demo_system #(
    .GpiWidth(8),
    .GpoWidth(4),
    .PwmWidth(12),
    .SRAMInitFile(SRAMInitFile)
  ) u_ibex_demo_system (
    //input
    .clk_sys_i(clk_sys),
    .rst_sys_ni(rst_sys_n),
    .gp_i({SW, BTN}),

    //output
    .gp_o(LED),
    .pwm_o(RGB_LED),
    .uart_tx_o(UART_TX),

    .spi_rx_i(SPI_RX),
    .spi_tx_o(SPI_TX),
    .spi_sck_o(SPI_SCK),
    .spi_ctrl_o(DISP_CTRL)
  );

endmodule
