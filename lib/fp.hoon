::
::::  /hoon/fp/lib
  ::  
/?    314
/-    *fp
!:
::
::::
|%
++  fl
  =+  ^-  [[p=@u v=@s w=@u] r=?(%n %u %d %z %a) d=?(%d %f %i)]
    [[113 -16.494 32.765] %n %d]
  |%
  ::  p=precision:     number of bits in arithmetic form; must be at least 2
  ::  v=min exponent:  minimum value of e
  ::  w=width:         max - min value of e, 0 is fixed point
  ::  r=rounding mode: nearest (ties to even), up, down, to zero, away from zero
  ::  d=behavior:      return denormals, flush denormals to zero,
  ::                   infinite exponent range
  ++  rou
    |=  [a=fn]  ^-  fn
    ?.  ?=([%f *] a)  a
    ?~  a.a  [%f s.a zer:m]
    ?:  s.a  (rou:m +>.a)
    =.(r swr:m (fli (rou:m +>.a)))
  ::
  ++  fli
    |=  [a=fn]  ^-  fn
    ?-(-.a %f a(s !s.a), %i a(s !s.a), %n a)
  ::
  ++  abs
    |=  [a=fn]  ^-  fn
    ?:  ?=([%f *] a)  [%f & e.a a.a]
    ?:  ?=([%i *] a)  [%i &]  [%n ~]
  ::
  ++  add
    |=  [a=fn b=fn]  ^-  fn
    ?:  |(?=([%n *] a) ?=([%n *] b))  [%n ~]
    ?:  |(?=([%i *] a) ?=([%i *] b))
      ?:  &(?=([%i *] a) ?=([%i *] b))
        ?:  =(a b)  a  [%n ~]
      ?:  ?=([%i *] a)  a  b
    ?:  |(=(a.a 0) =(a.b 0))
      ?.  &(=(a.a 0) =(a.b 0))  %-  rou  ?~(a.a b a)
      [%f ?:(=(r %d) &(s.a s.b) |(s.a s.b)) zer:m]
    %-  |=  [a=fn]
        ?.  ?=([%f *] a)  a
        ?.  =(a.a 0)  a
        [%f !=(r %d) zer:m]
    ?:  =(s.a s.b)
      ?:  s.a  (add:m +>.a +>.b |)
      =.(r swr:m (fli (add:m +>.a +>.b |)))
    ?:  s.a  (sub:m +>.a +>.b |)
    (sub:m +>.b +>.a |)
  ::
  ++  ead                                               ::  exact add
    |=  [a=fn b=fn]  ^-  fn
    ?:  |(?=([%n *] a) ?=([%n *] b))  [%n ~]
    ?:  |(?=([%i *] a) ?=([%i *] b))
      ?:  &(?=([%i *] a) ?=([%i *] b))
        ?:  =(a b)  a  [%n ~]
      ?:  ?=([%i *] a)  a  b
    ?:  |(=(a.a 0) =(a.b 0))
      ?.  &(=(a.a 0) =(a.b 0))  ?~(a.a b a)
      [%f ?:(=(r %d) &(s.a s.b) |(s.a s.b)) zer:m]
    ?:  =(s.a s.b)
      ?:  s.a  (add:m +>.a +>.b &)
      (fli (add:m +>.a +>.b &))
    ?:  s.a  (sub:m +>.a +>.b &)
    (sub:m +>.b +>.a &)
  ::
  ++  sub
    |=  [a=fn b=fn]  ^-  fn  (add a (fli b))
  ::
  ++  mul
    |=  [a=fn b=fn]  ^-  fn
    ?:  |(?=([%n *] a) ?=([%n *] b))  [%n ~]
    ?:  ?=([%i *] a)
      ?:  ?=([%i *] b)  [%i =(s.a s.b)]
      ?:  =(a.b 0)  [%n ~]  [%i =(s.a s.b)]
    ?:  ?=([%i *] b)
      ?:  =(a.a 0)  [%n ~]  [%i =(s.a s.b)]
    ?:  |(=(a.a 0) =(a.b 0))  [%f =(s.a s.b) zer:m]
    ?:  =(s.a s.b)  (mul:m +>.a +>.b)
    =.(r swr:m (fli (mul:m +>.a +>.b)))
  ::
  ++  emu                                               ::  exact multiply
    |=  [a=fn b=fn]  ^-  fn
    ?:  |(?=([%n *] a) ?=([%n *] b))  [%n ~]
    ?:  ?=([%i *] a)
      ?:  ?=([%i *] b)  [%i =(s.a s.b)]
      ?:  =(a.b 0)  [%n ~]  [%i =(s.a s.b)]
    ?:  ?=([%i *] b)
      ?:  =(a.a 0)  [%n ~]  [%i =(s.a s.b)]
    ?:  |(=(a.a 0) =(a.b 0))  [%f =(s.a s.b) zer:m]
    [%f =(s.a s.b) (sum:si e.a e.b) (^mul a.a a.b)]
  ::
  ++  div
    |=  [a=fn b=fn]  ^-  fn
    ?:  |(?=([%n *] a) ?=([%n *] b))  [%n ~]
    ?:  ?=([%i *] a)
      ?:  ?=([%i *] b)  [%n ~]  [%i =(s.a s.b)]
    ?:  ?=([%i *] b)  [%f =(s.a s.b) zer:m]
    ?:  =(a.a 0)  ?:  =(a.b 0)  [%n ~]  [%f =(s.a s.b) zer:m]
    ?:  =(a.b 0)  [%i =(s.a s.b)]
    ?:  =(s.a s.b)  (div:m +>.a +>.b)
    =.(r swr:m (fli (div:m +>.a +>.b)))
  ::
  ++  fma                                               ::  a * b + c
    |=  [a=fn b=fn c=fn]  ^-  fn
    (add (emu a b) c)
  ::
  ++  sqt                                               ::  square root
    |=  [a=fn]  ^-  fn
    ?:  ?=([%n *] a)  [%n ~]
    ?:  ?=([%i *] a)  ?:(s.a a [%n ~])
    ?~  a.a  [%f s.a zer:m]
    ?:  s.a  (sqt:m +>.a)  [%n ~]
  ::
  ++  isr                                               ::  inverse square root
    |=  [a=fn]  ^-  fn
    ?:  ?=([%n *] a)  [%n ~]
    ?:  ?=([%i *] a)  [%n ~]
    ?~  a.a  [%n ~]
    ?:  s.a  (isr:m +>.a)  [%n ~]
  ::
  ++  inv
    |=  [a=fn]  ^-  fn
    (div [%f & --0 1] a)
  ::
  ++  sun
    |=  [a=@u]  ^-  fn
    (rou [%f & --0 a])
  ::
  ++  san
    |=  [a=@s]  ^-  fn
    =+  b=(old:si a)
    (rou [%f -.b --0 +.b])
  ::
  ++  lth
    |=  [a=fn b=fn]  ^-  (unit ,?)
    ?:  |(?=([%n *] a) ?=([%n *] b))  ~  :-  ~
    ?:  =(a b)  |
    ?:  ?=([%i *] a)  !s.a  ?:  ?=([%i *] b)  s.b
    ?:  |(=(a.a 0) =(a.b 0))
      ?:  &(=(a.a 0) =(a.b 0))  |
      ?:  =(a.a 0)  s.b  !s.a
    ?:  !=(s.a s.b)  s.b
    ?:  s.a  (lth:m +>.a +>.b)  (lth:m +>.b +>.a)
  ::
  ++  lte
    |=  [a=fn b=fn]  ^-  (unit ,?)
    ?:  |(?=([%n *] a) ?=([%n *] b))  ~  :-  ~
    ?:  =(a b)  &
    ?:  ?=([%i *] a)  !s.a  ?:  ?=([%i *] b)  s.b
    ?:  |(=(a.a 0) =(a.b 0))
      ?:  &(=(a.a 0) =(a.b 0))  &
      ?:  =(a.a 0)  s.b  !s.a
    ?:  !=(s.a s.b)  s.b
    ?:  s.a  (lte:m +>.a +>.b)  (lte:m +>.b +>.a)
  ::
  ++  equ
    |=  [a=fn b=fn]  ^-  (unit ,?)
    ?:  |(?=([%n *] a) ?=([%n *] b))  ~  :-  ~
    ?:  =(a b)  &
    ?:  |(?=([%i *] a) ?=([%i *] b))  |
    ?:  |(=(a.a 0) =(a.b 0))
      ?:  &(=(a.a 0) =(a.b 0))  &  |
    ?:  |(=(e.a e.b) !=(s.a s.b))  |
    (equ:m +>.a +>.b)
  ::
  ++  gte
    |=  [a=fn b=fn]  ^-  (unit ,?)  (lte b a)
  ::
  ++  gth
    |=  [a=fn b=fn]  ^-  (unit ,?)  (lth b a)
  ::
  ++  drg                                               ::  float to decimal
    |=  [a=fn]  ^-  dn
    ?:  ?=([%n *] a)  [%n ~]
    ?:  ?=([%i *] a)  [%i s.a]
    ?~  a.a  [%d s.a --0 0]
    [%d s.a (drg:m +>.a)]
  ::
  ++  grd                                               ::  decimal to float
    |=  [a=dn]  ^-  fn
    ?:  ?=([%n *] a)  [%n ~]
    ?:  ?=([%i *] a)  [%i s.a]
    =>  .(r %n)                                         ::  always rnd nearest
    =+  q=(abs:si e.a)
    ?:  (syn:si e.a)
      (mul [%f s.a --0 a.a] [%f & e.a (pow:m 5 q)])
    (div [%f s.a --0 a.a] [%f & (sun:si q) (pow:m 5 q)])
  ::
  ++  c                                                 ::  mathematical constants
    |%
    ++  pi
      =+  np=prc:m
      |.  ^-  fn
      =>  .(p np)
      ?:  (^lth prc:m 1.200)
        =+  ^=  ap  0wOg~qE.y5EMz.j4NCa.bwdMs.QiA2j.wyapY.NQ0wK.-FzIj.
                      CO9hi.wxVzz.g4Tu-.l6rfd.eAcrc.2Iarv.9v53t.fUjlJ.
                      rl72h.ui5Jn.pynDX.6Z4N2.WqoTX.mIb~R.OSZ0q.TXuUU.
                      q~Jqy.p-BHF.YA4nN.b7-p9.a6phX.ehrfs.80vby.xoXY5.
                      CdF8d.xNlQV.FF5z-.E~ijf.nUdBn.ifsEW.Sm76b.Ply25
        (rau:m [-1.198 ap] |)
      =-
        =+  wp=(^add prc:m 16)
        =+  nc=16
        |-
        ?:  (^gth wp mxp:m)
          ~|  %very-large-precision  !!
        =+  x=(bnd:m (ka(r %n, p wp, d %i)))
        ?~  x  $(wp (^add wp nc), nc (^mul nc 2))
        +.x
      ::
      ^=  ka  |.  ^-  [fn fn]
      =+  [a=`fn`[%f & --0 1] b=`fn`[%f & -1 1]]
      =+  [d=`fn`[%f & -2 1] la=a k=0]
      |-
      =+  s=(shf:m (add a b) -2)
      =+  lb=(sqt b)
      =.  la  (shf:m (add la lb) -1)
      =.  a  (mul la la)
      =.  b  (shf:m (sub a s) --1)
      =+  e=(ned:m (ead a (fli b)))
      =.  d  (sub d e(e (sum:si e.e (sun:si k))))
      =+  f=(dif:si (sun:si k) (sun:si p))
      ?:  (need (gth (abs e) [%f & f 1]))
        $(k +(k))
      =+  ^=  g
        (dif:si (sun:si (^add (^mul k 2) 8)) (sun:si p))
      [(div b d) [%f & g 1]]
    ::
    ++  log2                                            ::  natural logarithm of 2
      =+  np=prc:m
      |.  ^-  fn
      =>  .(p np)
      ?:  (^lth prc:m 1.200)
        =+  ^=  ap  0wIn8nZ.Z7fuq.L9UXe.o0~bS.HQ3Pg.OpOCb.oJywQ.nmUKG.
                      -yLDK.7owru.KIC5m.lkLJa.-xIgX.iWKds.4U8kg.DlPIF.
                      4mCU9.jWmOx.oyiKz.56ILq.4j5Ye.7WVWF.L3Ijp.weOlL.
                      EeNSl.~tbsI.W7IpR.BicHR.TWqZe.30Oi6.lvEos.L8eey.
                      SySnN.gY~Rs.o7ZcE.h-RLX.A64dc.fyfVl.6yXBq.trvMu
        (rau:m [-1.200 ap] |)
      =-
        =+  wp=(^add prc:m 16)
        =+  nc=16
        |-
        ?:  (^gth wp mxp:m)
          ~|  %very-large-precision  !!
        =+  x=(bnd:m (ka(r %n, p wp, d %i)))
        ?~  x  $(wp (^add wp nc), nc (^mul nc 2))
        +.x
      ::
      ^=  ka  |.  ^-  [fn fn]
      =+  n=+((^div p 3))
      =+  o=(dec (^mul n 2))
      =+  ^=  q  %-  sun
        %+  ^mul  4
        %+  ^mul  (bex (dec n))
        %-  fac:m  [0 o]
      =+  ^=  t  %-  sun
        %+  ^mul  3
        =+  [c=0 d=0]
        |-  ?:  =(c n)  d
        =+  ^=  e
          =+  f=(fac:m 0 c)
          %+  ^mul  (^mul f f)
          %+  ^mul  (bex (^sub (dec n) c))
          %+  fac:m  +((^mul c 2))  o
        $(c +(c), d ?~((end 0 1 c) (^add d e) (^sub d e)))
      [(div t q) [%f & (dif:si --2 (sun:si p)) 1]]
    --
  ::
  ++  e                                                 ::  elementary functions
    |%
    ++  cos
      |=  [a=fn]  ^-  fn
      ?.  ?=([%f *] a)  [%n ~]
      ?:  =(a.a 0)  (rou [%f & --0 1])
      =-
        =+  wp=(^add prc:m 16)
        =+  nc=16
        |-
        ?:  (^gth wp mxp:m)
          ~|  %very-large-precision  !!
        =+  x=(bnd:m (ka(r %n, p wp, d %i)))
        ?~  x  $(wp (^add wp nc), nc (^mul nc 2))
        +.x
      ::
      =+  op=prc:m
      ^=  ka  |.  ^-  [fn fn]
      =.  a
        =+  q=(ibl:m +>.a)
        ?:  =((cmp:si q --1) -1)  a
        =+  ^=  pi
          (shf:m (pi:c (^add prc:m (abs:si q))) --1)
        (ned:m (rem:m a pi))
      =+  k=-:(itr:m (^div op 2))
      =+  ^=  i  %+  shf:m  =>(.(r %u) (mul a a))
        (new:si | (^mul k 2))
      =+  [s=`fn`[%f & --0 1] t=`fn`[%f & --0 1] l=1]
      |-
      ?>  ?=([%f *] t)
      ?.  ?|
            =(a.t 0)
            =+  q=(dif:si (ibl:m +>.t) --1)
            =((cmp:si q (new:si | p)) -1) 
          ==
        =.  t  (ned:m =>(.(r %u) (mul t i)))
        =+  ^=  q
          =+  j=(^mul l 2)
          (^mul j (dec j))
        =.  t  (ned:m =>(.(r %u) (div t [%f & --0 q])))
        =+  u=?~((dis 1 l) t (fli t))
        =.  s  (ned:m =>(.(r %d) (add s u)))
        $(l +(l))
      =+  w=k
      |-  ?~  k  :-  s
        =+  q=(dif:si (sun:si (^mul w 2)) (sun:si p))
        [%f & q +((^mul l 2))]
      =.  s
        =+  q=(ned:m =>(.(r %u) (mul s s)))
        (sub q(e (sum:si e.q --1)) [%f & --0 1])
      $(k (dec k))
    ::
    ++  sin
      |=  [a=fn]  ^-  fn
      ?.  ?=([%f *] a)  [%n ~]
      ?:  =(a.a 0)  [%f & zer:m]
      =-
        =+  wp=(^add prc:m 16)
        =+  nc=16
        |-
        ?:  (^gth wp mxp:m)
          ~|  %very-large-precision  !!
        =+  x=(bnd:m (ka(r %n, p wp, d %i)))
        ?~  x  $(wp (^add wp nc), nc (^mul nc 2))
        +.x
      ::
      ^=  ka  |.  ^-  [fn fn]
      =.  a
        =+  q=(ibl:m +>.a)
        ?:  =((cmp:si q --1) -1)  a
        =+  ^=  pi
          (shf:m (pi:c (^add prc:m (abs:si q))) --1)
        (ned:m (rem:m a pi))
      =+  c==>(.(r %a) (cos a))
      =+  t==>(.(r %a) (mul c c))
      =+  u==>(.(r %z) (sub [%f & --0 1] t))
      =+  s=(ned:m =>(.(r %z) (sqt u)))
      :-  s(s +<.a)
      =+  e=(sum:si (sun:si (^mul p 2)) e.s)
      [%f & (dif:si --3 e) 1]
    ::
    ++  tan
      |=  [a=fn]  ^-  fn
      ?.  ?=([%f *] a)  [%n ~]
      =-
        =+  wp=(^add prc:m 8)
        =+  nc=8
        |-
        ?:  (^gth wp mxp:m)
          ~|  %very-large-precision  !!
        =+  x=(bnd:m (ka(r %n, p wp, d %i)))
        ?~  x  $(wp (^add wp nc), nc (^mul nc 2))
        +.x
      ::
      ^=  ka  |.  ^-  [fn fn]
      =+  [s=(sin a) c=(cos a)]
      =+  t=(div s c)
      ?.  ?=([%f *] t)  [t [%f & zer:m]]
      [t [%f & e.t 4]]
    ::
    ++  acos
      |=  [a=fn]  ^-  fn
      !!
    ::
    ++  asin
      |=  [a=fn]  ^-  fn
      !!
    ::
    ++  atan
      |=  [a=fn]  ^-  fn
      !!
    ::
    ++  cosh
      |=  [a=fn]  ^-  fn
      !!
    ::
    ++  sinh
      |=  [a=fn]  ^-  fn
      !!
    ::
    ++  tanh
      |=  [a=fn]  ^-  fn
      !!
    ::
    ++  acosh
      |=  [a=fn]  ^-  fn
      !!
    ::
    ++  asinh
      |=  [a=fn]  ^-  fn
      !!
    ::
    ++  atanh
      |=  [a=fn]  ^-  fn
      !!
    ::
    ++  exp
      |=  [a=fn]  ^-  fn
      ?:  ?=([%n *] a)  [%n ~]
      ?:  ?=([%i *] a)  ?:(s.a [%i &] [%f & zer:m])
      ?~  a.a  (rou [%f & --0 1])
      =-
        =+  wp=(^add prc:m 16)
        =+  nc=16
        |-
        ?:  (^gth wp mxp:m)
          ~|  %very-large-precision  !!
        =+  x=(bnd:m (ka(r %n, p wp, d %i)))
        ?~  x  $(wp (^add wp nc), nc (^mul nc 2))
        +.x
      ::
      ^=  ka  |.  ^-  [fn fn]
      !!
    ::
    ++  log
      |=  [a=fn]  ^-  fn
      ?:  ?=([%n *] a)  [%n ~]
      ?:  ?=([%i *] a)  ?:(s.a [%i &] [%n ~])
      ?~  a.a  [%i |]  ?.  s.a  [%n ~]
      ?:  (need (equ a [%f & --0 1]))  [%f & zer:m]
      =-
        =+  wp=(^add prc:m 16)
        =+  nc=16
        |-
        ?:  (^gth wp mxp:m)
          ~|  %very-large-precision  !!
        =+  x=(bnd:m (ka(r %n, p wp, d %i)))
        ?~  x  $(wp (^add wp nc), nc (^mul nc 2))
        +.x
      ::
      ^=  ka  |.  ^-  [fn fn]
      =+  ^=  n
        =+  q=(sun:si (^div +(prc:m) 2))
        (sum:si (dif:si q (ibl:m +>.a)) --2)
      =.  a  (ned:m (shf:m a n))
      =.  a  (ned:m (agm [%f & --0 1] (div [%f & --0 4] a)))
      =.  a  (ned:m (shf:m a --1))
      =.  a  (ned:m (div (pi:c) a))
      =+  j=(old:si n)
      =+  q=(mul [%f -.j --0 +.j] (log2:c))
      =+  b=(ned:m (sub a q))
      =+  e=(dif:si (ibl:m +>.a) (ibl:m +>.b))
      :-  b  [%f & (sum:si e.b e) 11]
    ::
    ++  log2
      |=  [a=fn]  ^-  fn
      ?:  ?=([%n *] a)  [%n ~]
      ?:  ?=([%i *] a)  ?:(s.a [%i &] [%n ~])
      ?~  a.a  [%i |]  ?.  s.a  [%n ~]
      =+  q=(ibl:m +>.a)
      ?:  (need (equ a [%f & q 1]))
        (rou [%f (syn:si q) --0 (abs:si q)])
      =-
        =+  wp=(^add prc:m 8)
        =+  nc=8
        |-
        ?:  (^gth wp mxp:m)
          ~|  %very-large-precision  !!
        =+  x=(bnd:m (ka(r %n, p wp, d %i)))
        ?~  x  $(wp (^add wp nc), nc (^mul nc 2))
        +.x
      ::
      ^=  ka  |.  ^-  [fn fn]
      =+  q==>(.(r %d) (log2:c))
      =+  z=(ned:m (div (log a) q))
      :-  z  [%f & e.z 5]
    ::
    ++  log10
      |=  [a=fn]  ^-  fn
      !!
    ::
    ++  pow
      |=  [a=fn b=fn]  ^-  fn
      !!
    ::
    ++  agm                                             ::  arithmetic-geometric mean
      |=  [a=fn b=fn]  ^-  fn
      ?:  |(?=([%n *] a) ?=([%n *] b))  [%n ~]
      ?:  &(?=([%i *] a) ?=([%i *] b))
        ?:  &(=(s.a s.b) s.a)  a  [%n ~]
      ?:  ?=([%i *] a)  ?>  ?=([%f *] b)
        ?:  |(=(a.b 0) !s.a)  [%n ~]  [%i &]
      ?:  ?=([%i *] b)  ?>  ?=([%f *] a)
        ?:  |(=(a.a 0) !s.b)  [%n ~]  [%i &]
      ?:  |(=(a.a 0) =(a.b 0))  [%f & zer:m]
      ?.  &(s.a s.b)  [%n ~]
      =-
        =+  wp=(^add prc:m 16)
        =+  nc=16
        |-
        ?:  (^gth wp mxp:m)
          ~|  %very-large-precision  !!
        =+  x=(bnd:m (ka(r %n, p wp, d %i)))
        ?~  x  $(wp (^add wp nc), nc (^mul nc 2))
        +.x
      ::
      ^=  ka  |.  ^-  [fn fn]
      =+  s=(ned:m (mul a b))
      =+  u=(ned:m (sqt s))
      =+  ^=  v
        =+  q=(ned:m (add a b))
        q(e (dif:si e.q --1))
      =+  n=1  |-
      =+  j=(ned:m (ead v (fli u)))
      =+  ^=  y  |.  %+  cmp:si  %-  need  (cmp2:m v j)
        (sun:si (^sub p 2))
      ?:  |(=(a.j 0) =((y) --1))
        [v [%f & e.v (^add (^mul n 18) 51)]]            ::  XX error bounds correct?
      =+  ^=  nv
        =+  q=(ned:m (add u v))
        q(e (dif:si e.q --1))
      $(v nv, u (ned:m (sqt (mul u v))), n +(n))
    ::
    ++  ran                                             ::  range reduction, p=a-qb
      |=  [a=fn b=$+(@ fn) c=?]  ^-  [p=fn q=@u]        ::  b: accepts precision & produces const
      ?.  ?=([%f *] a)  [[%n ~] 0]                      ::  c: congruence modulo (|p| < b/2)
      ?~  a.a  [[%f & zer:m] 0]
      |-  ^-  [p=fn q=@u]  ?.  s.a
        =.  r  swr:m
        =+  q=$(s.a &)
        [(fli -.q) +.q]
      =-
        =+  wp=(^add prc:m 16)
        =+  nc=16
        |-
        ?:  (^gth wp mxp:m)
          ~|  %very-large-precision  !!
        =+  y=(ka(r %n, p wp, d %i))
        =+  x=(bnd:m -<.y +.y)
        ?~  x  $(wp (^add wp nc), nc (^mul nc 2))
        [+.x ->.y]
      ::
      ^=  ka  |.  ^-  [[fn @u] fn]
      =+  b=(ned:m (b prc:m))
      =+  [ma=(met 0 a.a) mb=(met 0 a.b)]
      =+  ^=  q
        ?.  =((cmp:si e.a e.b) -1)  --0
        (dif:si e.b e.a)
      =+  al=a(a (end 0 (abs:si q) a.a))
      =+  ah=a(a (rsh 0 (abs:si q) a.a), e (sum:si e.a q))
      =+  w=(abs:si (dif:si e.ah e.b))
      =+  z=(dod:m (bex w) a.b)
      =+  g=(^mul a.ah +.z)
      =+  ^=  x  ^-  [[? @u] @u]
        ?:  c
          =+  q=(cmd:m (^mul a.ah -.z) a.b)
          [(old:si -.q) (^add +.q g)]
        =+  q=(dod:m (^mul a.ah -.z) a.b)
        [[& -.q] (^add +.q g)]
      :_  [%f & e.b +(+.x)]  :_  +.x
      =+  r=`fn`[%f -<.x e.b ->.x]
      ?:  |((need (lth r b(e (dif:si e.b --1)))) =(a.al 0))
        (add r al)
      (sub al r)
    --
  ::
  ++  m                                                 ::  internal functions, constants
    |%                                                  ::  don't put 0s into [@s @u] args
    ++  rou
      |=  [a=[e=@s a=@u]]  ^-  fn  (rau a &)
    ::
    ++  rau
      |=  [a=[e=@s a=@u] t=?]  ^-  fn
      ?-  r
        %z  (lug %fl a t)  %d  (lug %fl a t)
        %a  (lug %ce a t)  %u  (lug %ce a t)
        %n  (lug %ne a t)
      ==
    ::
    ++  add
      |=  [a=[e=@s a=@u] b=[e=@s a=@u] e=?]  ^-  fn
      =+  q=(dif:si e.a e.b)
      |-  ?.  (syn:si q)  $(b a, a b, q +(q))           ::  a has larger exponent
      ?:  e
        [%f & e.b (^^add (lsh 0 (abs:si q) a.a) a.b)]
      =+  [ma=(met 0 a.a) mb=(met 0 a.b)]
      =+  ^=  w  %+  dif:si  e.a  %-  sun:si            ::  expanded exponent of a
        ?:  (^gth prc ma)  (^^sub prc ma)  0
      =+  ^=  x  %+  sum:si  e.b  (sun:si mb)           ::  highest exponent that b reaches
      ?:  =((cmp:si w x) --1)                           ::  don't actually need to add
        ?-  r
          %z  (lag %fl a)  %d  (lag %fl a)
          %a  (lag %lg a)  %u  (lag %lg a)
          %n  (lag %na a)
        ==
      (rou [e.b (^^add (lsh 0 (abs:si q) a.a) a.b)])
    ::
    ++  sub
      |=  [a=[e=@s a=@u] b=[e=@s a=@u] e=?]  ^-  fn
      =+  q=(dif:si e.a e.b)
      |-  ?.  (syn:si q)
        (fli $(b a, a b, q +(q), r swr))
      =+  [ma=(met 0 a.a) mb=(met 0 a.b)]
      =+  ^=  w  %+  dif:si  e.a  %-  sun:si
        ?:  (^gth prc ma)  (^^sub prc ma)  0
      =+  ^=  x  %+  sum:si  e.b  (sun:si mb)
      ?:  &(!e =((cmp:si w x) --1))
        ?-  r
          %z  (lag %sm a)  %d  (lag %sm a)
          %a  (lag %ce a)  %u  (lag %ce a)
          %n  (lag %nt a)
        ==
      =+  j=(lsh 0 (abs:si q) a.a)
      |-  ?.  (^gte j a.b)
        (fli $(a.b j, j a.b, r swr))
      =+  i=(^^sub j a.b)
      ?~  i  [%f & zer]
      ?:  e  [%f & e.b i]  (rou [e.b i])
    ::
    ++  mul
      |=  [a=[e=@s a=@u] b=[e=@s a=@u]]  ^-  fn
      (rou (sum:si e.a e.b) (^^mul a.a a.b))
    ::
    ++  div
      |=  [a=[e=@s a=@u] b=[e=@s a=@u]]  ^-  fn
      =+  [ma=(met 0 a.a) mb=(met 0 a.b)]
      =+  v=(dif:si (sun:si ma) (sun:si +((^^add mb prc))))
      =.  a  ?:  (syn:si v)  a
      a(e (sum:si v e.a), a (lsh 0 (abs:si v) a.a))
      =+  [j=(dif:si e.a e.b) q=(^^div a.a a.b)]
      ?+  r  (rou [j q])
        %u  ?~  (mod a.a a.b)  (lag %ce [j q])  (lag %lg [j q])
        %a  ?~  (mod a.a a.b)  (lag %ce [j q])  (lag %lg [j q])
        %n  ?~  (mod a.a a.b)  (lag %ne [j q])  (lag %na [j q])
      ==
    ::
    ++  fma
      |=  [a=[e=@s a=@u] b=[e=@s a=@u] c=[e=@s a=@u]]  ^-  fn
      (add [(sum:si e.a e.b) (^^mul a.a a.b)] c |)
    ::
    ++  fms
      |=  [a=[e=@s a=@u] b=[e=@s a=@u] c=[e=@s a=@u] d=?]  ^-  fn
      ?:  d  (sub [(sum:si e.a e.b) (^^mul a.a a.b)] c |)
      (sub c [(sum:si e.a e.b) (^^mul a.a a.b)] |)
    ::
    ::  integer square root w/sticky bit
    ++  itr
      |=  [a=@]  ^-  [@ ?]
      =+  [q=(^^div (dec (xeb a)) 2) r=0]
      =+  ^=  c
        |-  =+  s=(^^add r (bex q))
        =+  (^^mul s s)
        ?:  =(q 0)
          ?:  (^^lte - a)  [s -]  [r (^^mul r r)]
        ?:  (^^lte - a)  $(r s, q (dec q))  $(q (dec q))
      [-.c =(+.c a)]
    ::
    ::  integer inverse square root w/shift amount & sticky bit
    ++  iir
      |=  [a=@]  ^-  [@ @ ?]
      =+  [sa=(dec (xeb a))]
      =+  [q=(^^div (xeb a) 2) z=(bex (^^mul sa 2)) r=0]
      =+  ^=  c
        |-  =+  s=(^^add r (bex q))
        =+  (^^mul a (^^mul s s))
        ?:  =(q 0)
          ?:  (^^lte - z)  [s -]  [r (^^mul a (^^mul r r))]
        ?:  (^^lte - z)  $(r s, q (dec q))  $(q (dec q))
      [-.c sa =(+.c z)]
    ::
    ++  frd                                             ::  a/2, rounds to -inf
      |=  [a=@s]
      =+  b=(old:si a)
      ?:  |(-.b =((end 0 1 +.b) 0))
        (new:si -.b (rsh 0 1 +.b))
      (new:si -.b +((rsh 0 1 +.b)))
    ::
    ++  sqt
      |=  [a=[e=@s a=@u]]  ^-  fn
      =.  a
        =+  [w=(met 0 a.a) x=(^^mul +(prc) 2)]
        =+  ?:((^^lth w x) (^^sub x w) 0)
        =+  ?:  =((dis - 1) (dis (abs:si e.a) 1))  -
          (^^add - 1)                                   ::  enforce even exponent
        a(e (dif:si e.a (sun:si -)), a (lsh 0 - a.a))
      =+  [y=(itr a.a) z=(frd e.a)]
      (rau [z -.y] +.y)
    ::
    ++  isr
      |=  [a=[e=@s a=@u]]  ^-  fn
      =.  a
        =+  [w=(met 0 a.a) x=(^^mul +(prc) 2)]
        =+  ?:((^^lth w x) (^^sub x w) 0)
        =+  ?:  =((dis - 1) (dis (abs:si e.a) 1))  -
          (^^add - 1)
        a(e (dif:si e.a (sun:si -)), a (lsh 0 - a.a))
      =+  [y=(iir a.a) z=(frd e.a)]
      =+  q=(new:si !(syn:si z) (abs:si z))
      (rau [(dif:si q (sun:si +<.y)) -.y] +>.y)
    ::
    ++  lth
      |=  [a=[e=@s a=@u] b=[e=@s a=@u]]  ^-  ?
      ?:  =(e.a e.b)  (^^lth a.a a.b)
      =+  c=(cmp:si (ibl a) (ibl b))
      ?:  =(c -1)  &  ?:  =(c --1)  |
      ?:  =((cmp:si e.a e.b) -1)
        (^^lth (rsh 0 (abs:si (dif:si e.a e.b)) a.a) a.b)
      (^^lth (lsh 0 (abs:si (dif:si e.a e.b)) a.a) a.b)
    ::
    ++  lte
      |=  [a=[e=@s a=@u] b=[e=@s a=@u]]  ^-  ?
      ?:  =(e.a e.b)  (^^lte a.a a.b)
      =+  c=(cmp:si (ibl a) (ibl b))
      ?:  =(c -1)  &  ?:  =(c --1)  |
      ?:  =((cmp:si e.a e.b) -1)
        (^^lte a.a (lsh 0 (abs:si (dif:si e.a e.b)) a.b))
      (^^lte (lsh 0 (abs:si (dif:si e.a e.b)) a.a) a.b)
    ::
    ++  equ
      |=  [a=[e=@s a=@u] b=[e=@s a=@u]]  ^-  ?
      ?.  =((ibl a) (ibl b))  |
      ?:  =((cmp:si e.a e.b) -1)
        =((lsh 0 (abs:si (dif:si e.a e.b)) a.b) a.a)
      =((lsh 0 (abs:si (dif:si e.a e.b)) a.a) a.b)
    ::
    ::  integer binary logarithm: 2^ibl(a) <= |a| < 2^(ibl(a)+1)
    ++  ibl
      |=  [a=[e=@s a=@u]]  ^-  @s
      (sum:si (sun:si (dec (met 0 a.a))) e.a)
    ::
    ++  uni
      |=  [a=[e=@s a=@u]]
      ?<  =(a.a 0)
      |-  ?:  =((end 0 1 a.a) 1)  a
      $(a.a (rsh 0 1 a.a), e.a (sum:si e.a --1))
    ::
    ++  unj                                             ::  used internally by rounding
      |=  [a=[e=@s a=@u]]
      =+  ma=(met 0 a.a)
      ?:  =(ma +(prc))
        a(a (rsh 0 1 a.a), e (sum:si e.a --1))
      ?>  ?|
            =(ma prc)
            &(!=(den %i) =(e.a emn) (^^lth ma prc))
          ==
      a
    ::
    ::  assumes that (met 0 a.a) <= prc!!
    ++  xpd
      |=  [a=[e=@s a=@u]]
      =+  ?:  =(den %i)  (^^sub prc (met 0 a.a))
          =+  ^=  q
            =+  w=(dif:si e.a emn)
            ?:  (syn:si w)  (abs:si w)  0
          (min q (^^sub prc (met 0 a.a)))
      a(e (dif:si e.a (sun:si -)), a (lsh 0 - a.a))
    ::
    ::  required precision if sticky bit
    ++  rpr
      |=  [a=@s]
      ?:  |(=(den %i) =((cmp:si emn a) -1))  +(prc)
      =+  b=(abs:si (dif:si emn a))
      ?:  (^^lte b prc)  (^^add (^^sub prc b) 2)  1
    ::
    ::  in order: floor, ceiling, nearest (even, away from 0, toward 0), larger, smaller
    ++  lag
      |=  [t=?(%fl %ce %ne %na %nt %lg %sm) a=[e=@s a=@u]]  ^-  fn
      (lug t a &)
    ::
    ::  t=sticky bit
    ++  lug
      |=  [t=?(%fl %ce %ne %na %nt %lg %sm) a=[e=@s a=@u] s=?]  ^-  fn
      ::
      =-                                                ::  if !den, flush denormals to zero
        ?.  =(den %f)  -
        ?.  ?=([%f *] -)  -
        ?:  =((met 0 ->+>) prc)  -  [%f & zer]
      ::
      =+  m=(met 0 a.a)
      ?>  |(s (^gte m (rpr e.a)))                       ::  sticky bit requires precision
      =+  ^=  q
        =+  ^=  f                                       ::  reduce precision
          ?:  (^gth m prc)  (^^sub m prc)  0
        =+  ^=  g  %-  abs:si                           ::  enforce min. exp
          ?:  =(den %i)  --0
          ?:  =((cmp:si e.a emn) -1)  (dif:si emn e.a)  --0
        (max f g)
      =^  b  a  :-  (end 0 q a.a)
        a(e (sum:si e.a (sun:si q)), a (rsh 0 q a.a))
      ::
      ?~  a.a
        ?<  =(den %i)
        ?-  t
          %fl  [%f & zer]  %sm  [%f & zer]
          %ce  [%f & spd]  %lg  [%f & spd]
          %ne  ?:  s  [%f & ?:((^^lte b (bex (dec q))) zer spd)]
               [%f & ?:((^^lth b (bex (dec q))) zer spd)]
          %nt  ?:  s  [%f & ?:((^^lte b (bex (dec q))) zer spd)]
               [%f & ?:((^^lth b (bex (dec q))) zer spd)]
          %na  [%f & ?:((^^lth b (bex (dec q))) zer spd)]
        ==
      ::
      =.  a  (xpd a)                                    ::  expand
      ::
      =.  a  %-  unj
        ?-  t
          %fl  a
          %lg  a(a +(a.a))
          %sm  ?.  &(=(b 0) s)  a
               ?:  &(=(e.a emn) !=(den %i))  a(a (dec a.a))
               =+  y=(dec (^^mul a.a 2))
               ?.  (^^lte (met 0 y) prc)  a(a (dec a.a))
               [(dif:si e.a --1) y]
          %ce  ?:  &(=(b 0) s)  a  a(a +(a.a))
          %ne  ?~  b  a
               =+  y=(bex (dec q))
               ?:  &(=(b y) s)                          ::  halfway rounds to even
                 ?~  (dis a.a 1)  a  a(a +(a.a))
               ?:  (^^lth b y)  a  a(a +(a.a))
          %na  ?~  b  a
               =+  y=(bex (dec q))
               ?:  (^^lth b y)  a  a(a +(a.a))
          %nt  ?~  b  a
               =+  y=(bex (dec q))
               ?:  =(b y)  ?:  s  a  a(a +(a.a))
               ?:  (^^lth b y)  a  a(a +(a.a))
        ==
      ?~  a.a  [%f & zer]
      ::
      ?:  =(den %i)  [%f & a]
      ?:  =((cmp:si emx e.a) -1)  [%i &]  [%f & a]      ::  enforce max. exp
    ::
    ++  drg                                             ::  dragon4
      |=  [a=[e=@s a=@u]]  ^-  [@s @u]
      =.  a  ?:  (^^lth (met 0 a.a) prc)  (xpd a)  a
      =+  r=(lsh 0 ?:((syn:si e.a) (abs:si e.a) 0) a.a)
      =+  s=(lsh 0 ?.((syn:si e.a) (abs:si e.a) 0) 1)
      =+  m=(lsh 0 ?:((syn:si e.a) (abs:si e.a) 0) 1)
      =+  [k=--0 q=(^^div (^^add s 9) 10)]
      |-  ?:  (^^lth r q)
        %=  $
          k  (dif:si k --1)
          r  (^^mul r 10)
          m  (^^mul m 10)
        ==
      |-  ?:  (^gte (^^add (^^mul r 2) m) (^^mul s 2))
        $(s (^^mul s 10), k (sum:si k --1))
      =+  [u=0 o=0]
      |-  =>  %=  .
          k  (dif:si k --1)
          u  (^^div (^^mul r 10) s)
          r  (mod (^^mul r 10) s)
          m  (^^mul m 10)
        ==
      ?>  (^^lth u 10)
      =+  l=(^^lth (^^mul r 2) m)
      =+  ^=  h
        ?|  (^^lth (^^mul s 2) m)
            (^gth (^^mul r 2) (^^sub (^^mul s 2) m))
        ==
      ?:  &(!l !h)
        $(o (^^add (^^mul o 10) u))
      =+  q=|(&(!l h) &(=(l h) (^gte (^^mul r 2) s)))
      =.  o  (^^add (^^mul o 10) ?:(q +(u) u))
      [k o]
    ::
    ++  pow                                             ::  a^b
      |=  [a=@ b=@]
      ?:  =(b 0)  1
      |-  ?:  =(b 1)  a
      =+  c=$(b (^^div b 2))
      =+  d=(^^mul c c)
      ?:  =((end 0 1 b) 1)
        (^^mul d a)
      d
    ::
    ++  fac                                             ::  b! / a!
      |=  [a=@ b=@]
      =+  x=(^^sub b a)
      ?:  =(x 0)  1
      ?:  =(x 1)  b
      ?:  =(x 2)  (^^mul b (dec b))
      =+  y=(^^div (^^add a b) 2)
      (^^mul $(b y) $(a y))
    ::
    ++  bnd
      |=  [a=fn b=fn]  ^-  (unit fn)
      =+  x=(^add a b)
      =+  y=(^sub a b)
      ?:  =(x y)  [~ x]  ~
    ::
    ++  chb                                             ::  l <= a <= h
      |=  [a=fn l=fn h=fn]  ^-  ?
      &((fall (^lte l a) |) (fall (^lte a h) |))
    ::
    ++  ned
      |=  [a=fn]  ^-  [%f s=? e=@s a=@u]
      ?:  ?=([%f *] a)  a
      ~|  %need-float  !!
    ::
    ++  dod
      |=  [a=@u b=@u]  ^-  [@u @u]
      [(mod a b) (^^div a b)]
    ::
    ++  cmd
      |=  [a=@u b=@u]  ^-  [@s @u]
      =+  c=(^^div a b)
      =+  d=(mod a b)
      =+  e=(^^mul d 2)
      =+  ^=  f
        ?:  (^^lth e b)  c
        ?.  =(e b)  +(c)
        ?~  (end 0 1 c)  c  +(c)
      :_  f
      (dif:si (sun:si a) (sun:si (^^mul b f)))
    ::
    ++  rem
      |=  [a=fn b=fn]                                   ::  a cmod b
      =.  b  (abs b)
      ?:  |(?=([%n *] a) ?=([%n *] b))  [%n ~]
      ?:  |(?=([%i *] a) ?=([%i *] b))  [%n ~]
      ?~  a.a  [%f & zer:m]  ?~  a.b  [%n ~]
      |-  ?.  s.a  =.(r swr (fli $(s.a &)))
      =+  [ma=(met 0 a.a) mb=(met 0 a.b)]
      =+  ^=  q
        ?.  =((cmp:si e.a e.b) -1)  --0
        (dif:si e.b e.a)
      =+  al=a(a (end 0 (abs:si q) a.a))
      =+  ah=a(a (rsh 0 (abs:si q) a.a), e (sum:si e.a q))
      =+  w=(abs:si (dif:si e.ah e.b))
      =+  z=(mod (bex w) a.b)
      =+  x=(old:si -:(cmd:m (^^mul a.ah z) a.b))
      =+  r=`fn`[%f -.x e.b +.x]
      ?:  |((need (^lth r b(e (dif:si e.b --1)))) =(a.al 0))
        (^add r al)
      (^sub al r)
    ::
    ++  shf
      |=  [a=fn b=@s]
      ?:  |(?=([%n *] a) ?=([%i *] a))  a
      a(e (sum:si e.a b))
    ::
    ++  cmp2                                            ::  impl. of cmp2 as in
      |=  [a=fn b=fn]  ^-  (unit ,@s)                   ::  mpfr's algorithms manual
      ?>  &(?=([%f *] a) ?=([%f *] b))                  ::  XX unoptimized
      ?~  a.a  !!
      =+  c=(ned (ead a (fli b)))
      ?~  a.c  ~  :-  ~  (dif:si (ibl +>.a) (ibl +>.b))
    ::
    ++  swr  ?+(r r %d %u, %u %d)
    ++  prc  ?>((^gth p 1) p)
    ++  mxp  20.000                                     ::  max precision for some stuff
    ++  den  d
    ++  emn  v
    ++  emm  (sum:si emn (sun:si (dec prc)))
    ++  emx  (sum:si emn (sun:si w))
    ++  spd  [emn 1]                                    ::  smallest "denormal"
    ++  spn  [emn (bex (dec prc))]                      ::  smallest "normal"
    ++  lfn  [emx (fil 0 prc 1)]                        ::  largest
    ++  zer  [--0 0]                                    ::  zero
    --
  --
::
++  ff                                                  ::  ieee754 format
  |_  [[w=@u p=@u b=@s f=?] r=?(%n %u %d %z %a)]
  ::
  ++  sz  +((^add w p))
  ++  sb  (bex (^add w p))
  ::
  ++  pa
    =+  i=(dif:si (dif:si --1 b) (sun:si p))
    %*(. fl p +(p), v i, w (^sub (bex w) 3), d ?:(f %f %d), r r)
  ::
  ++  sea
    |=  [a=@r]  ^-  fn
    =+  f=(cut 0 [0 p] a)
    =+  e=(cut 0 [p w] a)
    =+  s==(0 (cut 0 [(^add p w) 1] a))
    ?:  =(e 0)
      ?:  =(f 0)  [%f s --0 0]  [%f s (dif:si --1 b) f]
    ?:  =(e (fil 0 w 1))
      ?:  =(f 0)  [%i s]  [%n ~]
    =+  q=(dif:si (sun:si e) (sum:si b (sun:si p)))
    =+  r=(^add f (bex p))
    [%f s q r]
  ::
  ++  bit  |=  [a=fn]  (bif (rou:pa a))
  ::
  ++  bif
    |=  [a=fn]  ^-  @r
    ?:  ?=([%i *] a)
      =+  q=(lsh 0 p (fil 0 w 1))
      ?:  s.a  q  (^add q sb)
    ?:  ?=([%n *] a)  (lsh 0 (dec p) (fil 0 +(w) 1))
    ?~  a.a  ?:  s.a  `@r`0  sb
    =+  ma=(met 0 a.a)
    ?.  =(ma +(p))
      ?>  =(e.a (dif:si --1 b))
      ?>  (^lth ma +(p))
      ?:  s.a  `@r`a.a  (^add a.a sb)
    =+  q=(sum:si (sum:si e.a (sun:si p)) b)
    =+  r=(^add (lsh 0 p (abs:si q)) (end 0 p a.a))
    ?:  s.a  r  (^add r sb)
  ::
  ++  sig
    |=  [a=@r]  ^-  ?
    =(0 (cut 0 [(^add p w) 1] a))
  ::
  ++  exp
    |=  [a=@r]  ^-  @s
    (dif:si (sun:si (cut 0 [p w] a)) b)
  ::
  ++  add  |=  [a=@r b=@r]  (bif (add:pa (sea a) (sea b)))
  ++  sub  |=  [a=@r b=@r]  (bif (sub:pa (sea a) (sea b)))
  ++  mul  |=  [a=@r b=@r]  (bif (mul:pa (sea a) (sea b)))
  ++  div  |=  [a=@r b=@r]  (bif (div:pa (sea a) (sea b)))
  ++  fma  |=  [a=@r b=@r c=@r]  (bif (fma:pa (sea a) (sea b) (sea c)))
  ++  sqt  |=  [a=@r]  (bif (sqt:pa (sea a)))
  ++  sun  |=  [a=@u]  (bit [%f & --0 a])
  ++  san  |=  [a=@s]  (bit [%f (syn:si a) --0 (abs:si a)])
  ++  lth  |=  [a=@r b=@r]  (lth:pa (sea a) (sea b))
  ++  lte  |=  [a=@r b=@r]  (lte:pa (sea a) (sea b))
  ++  equ  |=  [a=@r b=@r]  (equ:pa (sea a) (sea b))
  ++  gte  |=  [a=@r b=@r]  (gte:pa (sea a) (sea b))
  ++  gth  |=  [a=@r b=@r]  (gth:pa (sea a) (sea b))
  ++  drg  |=  [a=@r]  (drg:pa (sea a))
  ++  grd  |=  [a=dn]  (bif (grd:pa a))
  --
::
++  rlyd  |=  a=@rd  ^-  dn  (drg:rd a)
++  rlys  |=  a=@rs  ^-  dn  (drg:rs a)
++  rlyh  |=  a=@rh  ^-  dn  (drg:rh a)
++  rlyq  |=  a=@rq  ^-  dn  (drg:rq a)
++  ryld  |=  a=dn  ^-  @rd  (grd:rd a)
++  ryls  |=  a=dn  ^-  @rs  (grd:rs a)
++  rylh  |=  a=dn  ^-  @rh  (grd:rh a)
++  rylq  |=  a=dn  ^-  @rq  (grd:rq a)
::
++  rd
  |%
  ++  ma
    =>(ff .(w 11, p 52, b --1.023, f %.n, r %n))
  ++  sea
    |=  [a=@rd]  (sea:ma a)
  ++  bit
    |=  [a=fn]  ^-  @rd  (bit:ma a)
  ++  sig
    |=  [a=@rd]  (sig:ma a)
  ++  exp
    |=  [a=@rd]  (exp:ma a)
  ++  add
    |=  [a=@rd b=@rd]  ^-  @rd  (add:ma a b)
  ++  sub
    |=  [a=@rd b=@rd]  ^-  @rd  (sub:ma a b)
  ++  mul
    |=  [a=@rd b=@rd]  ^-  @rd  (mul:ma a b)
  ++  div
    |=  [a=@rd b=@rd]  ^-  @rd  (div:ma a b)
  ++  fma
    |=  [a=@rd b=@rd c=@rd]  ^-  @rd  (fma:ma a b c)
  ++  sqt
    |=  [a=@rd]  ^-  @rd  (sqt:ma a)
  ++  sun
    |=  [a=@u]  ^-  @rd  (sun:ma a)
  ++  san
    |=  [a=@s]  ^-  @rd  (san:ma a)
  ++  lth
    |=  [a=@rd b=@rd]  (lth:ma a b)
  ++  lte
    |=  [a=@rd b=@rd]  (lte:ma a b)
  ++  equ
    |=  [a=@rd b=@rd]  (equ:ma a b)
  ++  gte
    |=  [a=@rd b=@rd]  (gte:ma a b)
  ++  gth
    |=  [a=@rd b=@rd]  (gth:ma a b)
  ++  drg
    |=  [a=@rd]  (drg:ma a)
  ++  grd
    |=  [a=dn]  (grd:ma a)
  --
::
++  rs
  |%
  ++  ma
    =>(ff .(w 8, p 23, b --127, f %.n, r %n))
  ++  sea
    |=  [a=@rs]  (sea:ma a)
  ++  bit
    |=  [a=fn]  ^-  @rs  (bit:ma a)
  ++  sig
    |=  [a=@rs]  (sig:ma a)
  ++  exp
    |=  [a=@rs]  (exp:ma a)
  ++  add
    |=  [a=@rs b=@rs]  ^-  @rs  (add:ma a b)
  ++  sub
    |=  [a=@rs b=@rs]  ^-  @rs  (sub:ma a b)
  ++  mul
    |=  [a=@rs b=@rs]  ^-  @rs  (mul:ma a b)
  ++  div
    |=  [a=@rs b=@rs]  ^-  @rs  (div:ma a b)
  ++  fma
    |=  [a=@rs b=@rs c=@rs]  ^-  @rs  (fma:ma a b c)
  ++  sqt
    |=  [a=@rs]  ^-  @rs  (sqt:ma a)
  ++  sun
    |=  [a=@u]  ^-  @rs  (sun:ma a)
  ++  san
    |=  [a=@s]  ^-  @rs  (san:ma a)
  ++  lth
    |=  [a=@rs b=@rs]  (lth:ma a b)
  ++  lte
    |=  [a=@rs b=@rs]  (lte:ma a b)
  ++  equ
    |=  [a=@rs b=@rs]  (equ:ma a b)
  ++  gte
    |=  [a=@rs b=@rs]  (gte:ma a b)
  ++  gth
    |=  [a=@rs b=@rs]  (gth:ma a b)
  ++  drg
    |=  [a=@rs]  (drg:ma a)
  ++  grd
    |=  [a=dn]  (grd:ma a)
  --
::
++  rh
  |%
  ++  ma
    =>(ff .(w 5, p 10, b --15, f %.n, r %n))
  ++  sea
    |=  [a=@rh]  (sea:ma a)
  ++  bit
    |=  [a=fn]  ^-  @rh  (bit:ma a)
  ++  sig
    |=  [a=@rh]  (sig:ma a)
  ++  exp
    |=  [a=@rh]  (exp:ma a)
  ++  add
    |=  [a=@rh b=@rh]  ^-  @rh  (add:ma a b)
  ++  sub
    |=  [a=@rh b=@rh]  ^-  @rh  (sub:ma a b)
  ++  mul
    |=  [a=@rh b=@rh]  ^-  @rh  (mul:ma a b)
  ++  div
    |=  [a=@rh b=@rh]  ^-  @rh  (div:ma a b)
  ++  fma
    |=  [a=@rh b=@rh c=@rh]  ^-  @rh  (fma:ma a b c)
  ++  sqt
    |=  [a=@rh]  ^-  @rh  (sqt:ma a)
  ++  sun
    |=  [a=@u]  ^-  @rh  (sun:ma a)
  ++  san
    |=  [a=@s]  ^-  @rh  (san:ma a)
  ++  lth
    |=  [a=@rh b=@rh]  (lth:ma a b)
  ++  lte
    |=  [a=@rh b=@rh]  (lte:ma a b)
  ++  equ
    |=  [a=@rh b=@rh]  (equ:ma a b)
  ++  gte
    |=  [a=@rh b=@rh]  (gte:ma a b)
  ++  gth
    |=  [a=@rh b=@rh]  (gth:ma a b)
  ++  drg
    |=  [a=@rh]  (drg:ma a)
  ++  grd
    |=  [a=dn]  (grd:ma a)
  --
::
++  rq
  |%
  ++  ma
    =>(ff .(w 15, p 112, b --16.383, f %.n, r %n))
  ++  sea
    |=  [a=@rq]  (sea:ma a)
  ++  bit
    |=  [a=fn]  ^-  @rq  (bit:ma a)
  ++  sig
    |=  [a=@rq]  (sig:ma a)
  ++  exp
    |=  [a=@rq]  (exp:ma a)
  ++  add
    |=  [a=@rq b=@rq]  ^-  @rq  (add:ma a b)
  ++  sub
    |=  [a=@rq b=@rq]  ^-  @rq  (sub:ma a b)
  ++  mul
    |=  [a=@rq b=@rq]  ^-  @rq  (mul:ma a b)
  ++  div
    |=  [a=@rq b=@rq]  ^-  @rq  (div:ma a b)
  ++  fma
    |=  [a=@rq b=@rq c=@rq]  ^-  @rq  (fma:ma a b c)
  ++  sqt
    |=  [a=@rq]  ^-  @rq  (sqt:ma a)
  ++  sun
    |=  [a=@u]  ^-  @rq  (sun:ma a)
  ++  san
    |=  [a=@s]  ^-  @rq  (san:ma a)
  ++  lth
    |=  [a=@rq b=@rq]  (lth:ma a b)
  ++  lte
    |=  [a=@rq b=@rq]  (lte:ma a b)
  ++  equ
    |=  [a=@rq b=@rq]  (equ:ma a b)
  ++  gte
    |=  [a=@rq b=@rq]  (gte:ma a b)
  ++  gth
    |=  [a=@rq b=@rq]  (gth:ma a b)
  ++  drg
    |=  [a=@rq]  (drg:ma a)
  ++  grd
    |=  [a=dn]  (grd:ma a)
  --
--
