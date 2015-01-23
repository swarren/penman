#!/usr/bin/env python3

import penman
import sys

app = sys.argv.pop(0)
sides = int(sys.argv.pop(0))
slen = int(sys.argv.pop(0))

turn_angle = 360 / sides

t = penman.Penman()

t.pen_down()
for i in range(sides):
  t.forward(slen)
  t.right(turn_angle)
