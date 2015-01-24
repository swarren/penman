This repo contains data, code, etc. related to the Penman GR1500 plotter;
manufactured around 1984!

bom/
    Images of the device, and a list of the chips.

data-file/
    Files containing pre-rendered command scripts.

py/
    Python code to draw shapes, send pre-rendered data files.

vb/
    Visual Basic code from http://tnotes.de, used as reference.

References:
http://tnotes.de/PenMan
http://tnotes.de/HPGL2penman
http://tnotes.de/penman/PenMan.zip
http://tnotes.de/penman/PlotListen.zip
https://www.youtube.com/watch?v=mR8enP09RPI
http://www.javadocexamples.com/java_source/__/pe/Penman.java.html

It seems like the device is less and less accurate the longer it's been
turned on, e.g. after just a few minutes. I guess the motors overheat or
something.

The home ("H") command doesn't seem to work well. I'm not sure if there's some
requirement on the initial position of the device before it's executed, or
whether my device has a hardware issue.
