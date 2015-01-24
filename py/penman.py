import math
import os
import fdpexpect
import serial
import sys

# TODO:
# Unify serial I/O logic with send.py
# Split out Turtle wrapper class from Penman low-level serial I/O class

_debug_cmds = True

class Penman(object):
    def __init__(self):
        # FIXME: Need configuration of the port name:
        self._s = serial.Serial("/dev/ttyUSB0", 9600)
        self._exp = fdpexpect.fdspawn(self._s.fileno())
        self._pen = 'U'
        self._pos = [0, 0]
        self._heading = 0
        self._do_command('W1,') # Self-correct wrap?
        self._do_command('P2,') # Select pen

    def pen_up(self):
        self._pen = 'U'

    def pen_down(self):
        self._pen = 'D'

    def goto(self, x, y):
        self._pos = [x, y]
        self._do_command(self._pen)
        self._do_command('A')
        # FIXME: Correctly scale/clip/center the drawing within the penman's
        # co-ordinate range.
        xp = str(int(self._pos[0]) + 500)
        yp = str(int(self._pos[1]) + 500)
        self._do_command('M' + xp + ',' + yp + ',')

    def forward(self, distance):
        self._move(distance)

    def backward(self, distance):
        self._move(-distance)

    def left(self, angle):
        self._heading -= angle
        self._heading %= 360

    def right(self, angle):
        self._heading += angle
        self._heading %= 360

    def _move(self, distance):
        #print("move(%f)" % distance)
        #print("heading=%f" % self._heading)
        deltax = distance * math.cos(math.radians(self._heading))
        #print("deltax=%f" % deltax)
        deltay = distance * math.sin(math.radians(self._heading))
        #print("deltay=%f" % deltay)
        self.goto(self._pos[0] + deltax, self._pos[1] + deltay)

    def _do_command(self, c):
        if _debug_cmds: print(str(c))
        self._exp.send(c)
        self._exp.expect('!')
