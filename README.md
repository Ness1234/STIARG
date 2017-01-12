# STIARG
## Shit This Is A Racing Game

## Documentation

The back-end (e.g., events creation, timing behaviour,...) is already implemented in the provided project library. To help you getting started with your own FritzLight project, we provide a sample program which allows the user to move a single pixel over the screen (output) by pressing the h, j, k, or l key (input).


In the main function, four parts are of importance for your work: The IP address (which you will have to adapt according to the emulator device you want to communicate with2, the delay between each sent frame (choose a value between 33000 (33ms) and 500000 (500ms)), the actual event function for your own processing of input and generation of output (repeatedly called after the desired delay), and the initial state of the system. The system state is not necessarily a two-element-tuple, but can be any data type that you want to use for data which is supposed to be persistent between events, such as the positions of dynamic pixels or letters (only make sure that the type corresponds to the one you are expecting in the contract of your event function).
The main functionality in this example is implemented via two functions: 

The eventTest function processes keyboard input (obtained from the list of input events), and moves the highlighted pixel accordingly. The toFrame function takes the information about the highlighted pixel and creates a complete assignment (ListFrame) of every single pixel to its desired RGB color data (in this case, the pixel to be highlighted is set to white (0xff 0xff 0xff), while the remaining ones are set to black (0 0 0).

If you need random data in any of your functions, replace the runMate function in main with runMateRandom, and add an additional first argument to your event function that will contain an infinite list of random Int values
	eventTest :: [Int] -> [Event String] -> (Int, Int) -> (ListFrame, (Int, Int))).

    
## Which possibilities do I have to output my pixel data graphically?
1. Online using the emulator at <http://pixelkino.haskell.de>
* The IP address of the online emulator is <134.28.70.172>
* Use a browser to view the available screens, click on your IP.
2. In room E4.036: On the SmartBoard, or the FritzLight hardware
Connect (wired) to the switch.
* Use the emulator on the SmartBoard, available at <192.168.23.2>
(PC with mouse is in grey shelf, use guest account, open firefox)
* Control the FritzLight at <192.168.23.1>
* You may also execute sample programs using the FritzLight PC 
3. Install the emulator on your own system (advanced)


### Useful links for the project
* Online MateLight emulator on <http://pixelkino.haskell.de>

* MateLightlibraryon >https://github.com/bigmac2k/HSMateLight>, and
MateLightEmu on <https://github.com/bigmac2k/MateLightEmu>
