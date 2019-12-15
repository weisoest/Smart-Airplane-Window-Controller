# Smart-Airplane-Window-Controller
An embedded system that simulates asynchronous opaque level control on multiple airplane windows.

Requirements
Hardware: AVR lab board and power supply
Software: Atmel Studio 7.0, Arduino

Instructions:
1. Open the project under directory "\proj1\proj1.asmproj" using Atmel Studio 7.0
2. Click "build solution" to generate the binary file (.hex) of the project
3. Prerequisite: (1) Arduino installed. (2) lab board connect to laptop using usb
4. Change directory to the "programmer" folder running a batch to download .hex file to the AVR lab board
5. Type the command download2 COM[x] [proj_name].hex where COM[x] is the port that communicates with the
   lab board so that the program flash to the lab board
