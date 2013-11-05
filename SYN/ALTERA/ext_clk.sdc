create_clock -name Oscillator_Clock -period 20.000 [get_ports {50MHZ}]

derive_pll_clocks