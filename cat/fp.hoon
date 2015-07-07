::
::::  /hoon/fp/cat
  ::
/?    314
/-    *fp
/+    fp
::
::::
!:
::  [11 -24 29] [24 -149 253] [53 -1074 2045] [113 -16494 32765]
=+  fs==>(fl .(p 24, v -149, w 253, r %n))
=+  fq==>(fl .(p 113, v -16.494, w 32.765, r %n))
::
|=  [* [[a=[@s @u] ~] ~]]
:::-  %noun  (drg:fs (agm:e:fs [%f & a] [%f & b]))
=+  s=(ned:m:fs (tan:e:fs [%f & a]))
=+  q=(ned:m:fq (tan:e:fq [%f & a]))
:-  %noun  (drg:fq q)
::
::|=  [* [[a=[? @s @u] b=[? @s @u] ~] ~]]
::=+  [fu=fs(r %u) fd=fs(r %d) fz=fs(r %z) fa=fs(r %a)]
:::-  %noun  "n: {<(add:fs [%f a] [%f b])>} || u: {<(add:fu [%f a] [%f b])>} || d: {<(add:fd [%f a] [%f b])>} || z: {<(add:fz [%f a] [%f b])>} || a: {<(add:fa [%f a] [%f b])>}"
