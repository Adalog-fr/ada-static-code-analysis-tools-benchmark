# edc_client

Implementation of a client library to use the [embedded dashboard console](https://github.com/hgrodriguez/embedded-dashboard-console).

There are [examples](https://github.com/hgrodriguez/edc_client_examples) available for Raspberry Pico and ItsyBitsy RP2040.

## Overall idea
The code for the client itself is platform agnostic and uses standard Ada 2012 and only depends on [HAL](https://github.com/Fabien-Chouteau/hal).

This should enable any embedded platform supported by HAL to use the client library to connect to the dashboard.

## Connection capabilities
Currently the dashboard itself offers only UART connectivity.

As soon as the connectivity is extended in the dashboard, this library will be updated to ensure, that the communication is working for any communication link offered by the [embedded dashboard console](https://github.com/hgrodriguez/embedded-dashboard-console).

##  Versioning scheme
Major.Minor.Patch

### Major
1 = UART; Status: **implemented**

2 = UART + SPI

3 = UART + SPI + I2C

### Minor

1 = LEDs; Status: **implemented**

2 = LEDs + Matrix 16-bit Word HEX display; Status: **implemented**

3 = LEDs + Matrix 16-bit Word HEX Display + Matrix 32-bit DWord HEX Display; Status: **implemented**

4 = LEDs + Matrix 16-bit Word HEX Display + Matrix 32-bit DWord HEX Display + Character Display **implemented**

5 = LEDs + Matrix 16-bit Word HEX Display + Matrix 32-bit DWord HEX Display + Character Display + Horizontal Scrolling Character Display

6 = OLED
