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
  |_  [[p=@u v=@s w=@u] r=?(%n %u %d %z %a)]
  ::  p=precision: number of bits in arithmetic form, including "hidden bit"
  ::               must be at least 2
  ::  v=minimum value of e
  ::  w=width: max - min value of e, w=0 is fixed point
  ::  r=rounding mode: round to nearest (ties to even), round up, round down,
  ::                   round to zero, round away from zero
  ::  binary32: [24 -149 253 r] (-149 = -126 - 24 + 1)
  ::  binary16: [11 -24 29 r] binary64: [53 -1074 2045 r]
  ::  binary128: [113 -16494 32765 r]
  ::
  ++  rou
    |=  [a=fn]  ^-  fn
    ?.  ?=([%f *] a)  a
    ?~  a.a  [%f s.a --0 0]
    ?:  s.a  (rou:m +>.a)
    =.(r swr:m (fli (rou:m +>.a)))
  ::
  ++  fli
    |=  [a=fn]  ^-  fn
    ?-(-.a %f a(s !s.a), %i a(s !s.a), %n a)
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
      [%f ?:(=(r %d) &(s.a s.b) |(s.a s.b)) --0 0]
    ?:  =(s.a s.b)
      ?:  s.a  (add:m +>.a +>.b)
      =.(r swr:m (fli (add:m +>.a +>.b)))
    ?:  s.a  (sub:m +>.a +>.b)
    (sub:m +>.b +>.a)
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
    ?:  |(=(a.a 0) =(a.b 0))  [%f =(s.a s.b) --0 0]
    ?:  =(s.a s.b)  (mul:m +>.a +>.b)
    =.(r swr:m (fli (mul:m +>.a +>.b)))
  ::
  ++  div
    |=  [a=fn b=fn]  ^-  fn
    ?:  |(?=([%n *] a) ?=([%n *] b))  [%n ~]
    ?:  ?=([%i *] a)
      ?:  ?=([%i *] b)  [%n ~]  [%i =(s.a s.b)]
    ?:  ?=([%i *] b)  [%f =(s.a s.b) --0 0]
    ?:  =(a.a 0)  ?:  =(a.b 0)  [%n ~]  [%f =(s.a s.b) --0 0]
    ?:  =(a.b 0)  [%i =(s.a s.b)]
    ?:  =(s.a s.b)  (div:m +>.a +>.b)
    =.(r swr:m (fli (div:m +>.a +>.b)))
  ::
  ++  fma
    |=  [a=fn b=fn c=fn]  ^-  fn                        ::  a * b + c
    ?:  |(?=([%n *] a) ?=([%n *] b) ?=([%n *] c))  [%n ~]
    =+  ^=  x
      ?:  ?=([%i *] a)
        ?:  ?=([%i *] b)  [%i =(s.a s.b)]
        ?:  =(a.b 0)  [%n ~]  [%i =(s.a s.b)]
      ?:  ?=([%i *] b)
        ?:  =(a.a 0)  [%n ~]  [%i =(s.a s.b)]
      ?:  |(=(a.a 0) =(a.b 0))  [%f =(s.a s.b) --0 0]
      [%f =(s.a s.b) (sum:si e.a e.b) (^mul a.a a.b)]
    (add x c)
  ::
  ++  sqt
    |=  [a=fn]  ^-  fn
    ?:  ?=([%n *] a)  [%n ~]
    ?:  ?=([%i *] a)  ?:(s.a a [%n ~])
    ?~  a.a  [%f s.a --0 0]
    ?:  s.a  (sqt:m +>.a)  [%n ~]
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
    =+  q=(abs:si e.a)
    ?:  (syn:si e.a)
      (mul [%f s.a --0 a.a] [%f & e.a (pow:m 5 q)])
    (div [%f s.a --0 a.a] [%f & (sun:si q) (pow:m 5 q)])
  ::
  ++  m                                                 ::  internal functions, constants
    |%                                                  ::  don't put 0s into these
    ++  rou
      |=  [a=[e=@s a=@u]]  ^-  fn  (rau a %e)
    ::
    ++  rau
      |=  [a=[e=@s a=@u] f=?(%e %d %h %u)]  ^-  fn
      ?-  r
        %z  (lug %fl a f)  %d  (lug %fl a f)
        %a  (lug %ce a f)  %u  (lug %ce a f)
        %n  (lug %ne a f)
      ==
    ::
    ++  add
      |=  [a=[e=@s a=@u] b=[e=@s a=@u]]  ^-  fn
      =+  q=(dif:si e.a e.b)
      |-  ?.  (syn:si q)  $(b a, a b, q +(q))           ::  a has larger exponent
      =+  [ma=(met 0 a.a) mb=(met 0 a.b)]
      =+  ^=  w  %+  dif:si  e.a  %-  sun:si            ::  expanded exponent of a
        ?:  (^gth prc ma)  (^^sub prc ma)  0
      =+  ^=  x  %+  sum:si  e.b  (sun:si mb)           ::  highest exponent that b reaches
      ?:  &(=((cmp:si w x) --1))                        ::  don't actually need to add
        ?-  r
          %z  (lag %fl a)  %d  (lag %fl a)
          %a  (lag %lg a)  %u  (lag %lg a)
          %n  (lag %na a)
        ==
      (rou [e.b (^^add (lsh 0 (abs:si q) a.a) a.b)])
    ::
    ++  sub
      |=  [a=[e=@s a=@u] b=[e=@s a=@u]]  ^-  fn
      =+  q=(dif:si e.a e.b)
      |-  ?.  (syn:si q)
      (fli $(b a, a b, q +(q), r swr))
      =+  [ma=(met 0 a.a) mb=(met 0 a.b)]
      =+  ^=  w  %+  dif:si  e.a  %-  sun:si
        ?:  (^gth prc ma)  (^^sub prc ma)  0
      =+  ^=  x  %+  sum:si  e.b  (sun:si mb)
      ?:  &(=((cmp:si w x) --1))
        ?-  r
          %z  (lag %sm a)  %d  (lag %sm a)
          %a  (lag %ce a)  %u  (lag %ce a)
          %n  (lag %nt a)
        ==
      =+  j=(lsh 0 (abs:si q) a.a)
      |-  ?.  (^gte j a.b)  (fli $(a.b j, j a.b, r swr))
      =+  i=(^^sub j a.b)
      ?~  i  [%f & zer]  (rou [e.b i])
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
      (add [(sum:si e.a e.b) (^^mul a.a a.b)] c)
    ::
    ++  fms
      |=  [a=[e=@s a=@u] b=[e=@s a=@u] c=[e=@s a=@u] d=?]  ^-  fn
      ?:  d  (sub [(sum:si e.a e.b) (^^mul a.a a.b)] c)
      (sub c [(sum:si e.a e.b) (^^mul a.a a.b)])
    ::
    ::  integer square root w/rounding info: sqrt((rsh 1 b a))
    ++  itr
      |=  [a=@ b=@]  ^-  [@ ?(%e %d %h %u)]
      =>  .(a (lsh 1 1 a), b +(b))
      =+  [q=(^^div (dec (xeb a)) 2) r=0]
      =+  ^=  c
        |-  =+  s=(^^add r (bex q))
        =+  (^^mul s s)
        ?:  |(=(q 0) =(q b))
          ?:  (^^lte - a)  [s -]  [r (^^mul r r)]
        ?:  (^^lte - a)  $(r s, q (dec q))  $(q (dec q))
      =+  z=(rsh 0 b -.c)
      ?:  =(+.c a)  [z %e]
      =+  v=(^^add -.c (lsh 0 (dec b) 1))
      =+  y=(^^mul v v)
      ?:  =(y a)  [z %h]  ?:((^^lth y a) [z %u] [z %d])
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
      =+  v=(ibl a)
      ?:  =((cmp:si (frd v) (dif:si emn --1)) -1)
        [%f & ?:(=(r %u) spd zer)]
      =.  a
        =+  [w=(met 0 a.a) x=(^^mul prc 2)]
        =+  ?:((^^lth w x) (^^sub x w) 0)
        =+  ?:  =((dis - 1) (dis (abs:si e.a) 1))  -
          (^^add - 1)                                   ::  enforce even exponent
        a(e (dif:si e.a (sun:si -)), a (lsh 0 - a.a))
      =+  x=(^^sub (^^div (met 0 a.a) 2) prc)
      =+  [y=(itr a.a x) z=(sum:si (sun:si x) (frd e.a))]
      (rau [z -.y] +.y)
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
    ++  unj
      |=  [a=[e=@s a=@u]]
      =+  ma=(met 0 a.a)
      ?:  =(ma +(prc))
        a(a (rsh 0 1 a.a), e (sum:si e.a --1))
      ?>  |(=(ma prc) &(=(e.a emn) (^^lth ma prc)))
      a
    ::
    ::  assumes that (met 0 a.a) <= prc and emn <= e.a!!
    ++  xpd
      |=  [a=[e=@s a=@u]]
      =+  (min (abs:si (dif:si e.a emn)) (^^sub prc (met 0 a.a)))
      a(e (dif:si e.a (sun:si -)), a (lsh 0 - a.a))
    ::
    ::  required precision for %d, %h, %u
    ++  rpr
      |=  [a=@s]
      ?.  =((cmp:si a emn) -1)  prc
      =+  b=(abs:si (dif:si emn a))
      ?:  (^^lth b prc)  (^^sub prc b)  1
    ::
    ::  in order: floor, ceiling, nearest (even, away from 0, toward 0), larger, smaller
    ++  lag
      |=  [t=?(%fl %ce %ne %na %nt %lg %sm) a=[e=@s a=@u]]  ^-  fn
      (lug t a %e)
    ::
    ::  using %e, %d, %h, %u allows us to round numbers without knowing all digits
    ::  %e: a =  a.a    *2^e.a ||  %d:  a.a    *2^e.a < a < (a.a+.5)*2^e.a
    ::  %h: a = (a.a+.5)*2^e.a ||  %u: (a.a+.5)*2^e.a < a < (a.a+ 1)*2^e.a
    ++  lug
      |=  [t=?(%fl %ce %ne %na %nt %lg %sm) a=[e=@s a=@u] f=?(%e %d %h %u)]  ^-  fn
      =+  m=(met 0 a.a)
      ?>  |(=(f %e) (^gte m (rpr e.a)))                 ::  if not %e, need sufficient precision
      =+  ^=  q
        =+  ^=  f                                       ::  reduce precision
          ?:  (^gth m prc)  (^^sub m prc)  0
        =+  ^=  g  %-  abs:si                           ::  enforce min. exp
          ?:  =((cmp:si e.a emn) -1)  (dif:si emn e.a)  --0
        (max f g)
      =^  b  a  :-  (end 0 q a.a)
        a(e (sum:si e.a (sun:si q)), a (rsh 0 q a.a))
      ::
      ?~  a.a
        ?-  t
          %fl  [%f & zer]  %sm  [%f & zer]
          %ce  [%f & spd]  %lg  [%f & spd]
          %ne  ?:  =(f %e)  [%f & ?:((^^lte b (bex (dec q))) zer spd)]
               [%f & ?:((^^lth b (bex (dec q))) zer spd)]
          %nt  ?:  =(f %e)  [%f & ?:((^^lte b (bex (dec q))) zer spd)]
               [%f & ?:((^^lth b (bex (dec q))) zer spd)]
          %na  [%f & ?:((^^lth b (bex (dec q))) zer spd)]
        ==
      ::
      =.  a  (xpd a)                                    ::  expand
      ::
      =.  a  %-  unj
        ?-  t
          %fl  a  %lg  a(a +(a.a))
          %sm  ?.  &(=(b 0) =(f %e))  a
               ?:  =(e.a emn)  a(a (dec a.a))           ::  this is a bit awkward
               =+  y=(dec (^^mul a.a 2))
               ?.  (^^lte (met 0 y) prc)  a(a (dec a.a))
               [(dif:si e.a -1) y]
          %ce  ?:  &(=(b 0) =(f %e))  a  a(a +(a.a))
          %ne  ?~  b  ?.  =(q 0)  a
                 ?-  f
                   %e  a  %d  a  %u  a(a +(a.a))
                   %h  ?~  (dis a.a 1)  a  a(a +(a.a))
                 ==
               =+  y=(bex (dec q))
               ?:  &(=(b y) =(f %e))                    ::  halfway rounds to even
                 ?~  (dis a.a 1)  a  a(a +(a.a))
               ?:  (^^lth b y)  a  a(a +(a.a))
          %na  ?~  b  ?.  =(q 0)  a  ?+(f a(a +(a.a)) %e a, %d a)
               ?:  (^^lth b (bex (dec q)))  a  a(a +(a.a))
          %nt  ?~  b  ?.  =(q 0)  a  ?+(f a %u a(a +(a.a)))
               =+  y=(bex (dec q))
               ?:  =(b y)  ?:  =(f %e)  a  a(a +(a.a))
               ?:  (^^lth b y)  a  a(a +(a.a))
        ==
      ?~  a.a  [%f & zer]
      ::
      =+  x=(dif:si e.a emx)
      ?:  (syn:si x)  [%i &]  [%f & a]                  ::  enforce max. exp
    ::
    ++  drg                                             ::  dragon4
      |=  [a=[e=@s a=@u]]  ^-  [@s @u]
      =.  a
        ?:  ?&  (^^lth (met 0 a.a) prc)
                (syn:si (dif:si e.a emn))
            ==
          (xpd a)  a
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
    ++  swr  ?+(r r %d %u, %u %d)
    ++  prc  ?>((^gth p 1) p)
    ++  emn  v
    ++  emm  (sum:si emn (sun:si (dec prc)))
    ++  emx  (sum:si emn (sun:si w))
    ++  spd  [emn 1]                                    ::  smallest "denormal"
    ++  spn  [emm 1]                                    ::  smallest "normal"
    ++  lfn  [emx (fil 0 prc 1)]                        ::  largest
    ++  zer  [--0 0]                                    ::  zero
    --
  --
::
++  ff                                                  ::  ieee754 format
  |_  [w=@u p=@u b=@s r=?(%n %u %d %z %a)]
  ::
  ++  sz  +((^add w p))
  ++  sb  (bex (^add w p))
  ::
  ++  pa
    =+  i=(dif:si --1 b)
    =+  q=fl
    q(p +(p), v i, w (^sub (bex w) 3), r r)
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
  ++  add  |=  [a=@r b=@r]  (bif (add:pa (sea a) (sea b)))
  ++  sub  |=  [a=@r b=@r]  (bif (sub:pa (sea a) (sea b)))
  ++  mul  |=  [a=@r b=@r]  (bif (mul:pa (sea a) (sea b)))
  ++  div  |=  [a=@r b=@r]  (bif (div:pa (sea a) (sea b)))
  ++  fma  |=  [a=@r b=@r c=@r]  (bif (fma:pa (sea a) (sea b) (sea c)))
  ++  sqt  |=  [a=@r]  (bif (sqt:pa (sea a)))
  ++  sun  |=  [a=@u]  (bit [%f & --0 a])
  ++  lth  |=  [a=@r b=@r]  (lth:pa (sea a) (sea b))
  ++  lte  |=  [a=@r b=@r]  (lte:pa (sea a) (sea b))
  ++  equ  |=  [a=@r b=@r]  (equ:pa (sea a) (sea b))
  ++  gte  |=  [a=@r b=@r]  (gte:pa (sea a) (sea b))
  ++  gth  |=  [a=@r b=@r]  (gth:pa (sea a) (sea b))
  --
::
++  rh  =>  ff  .(w 5, p 10, b --15, r %n)
++  rs  =>  ff  .(w 8, p 23, b --127, r %n)
++  rq  =>  ff  .(w 15, p 112, b --16.383, r %n)
::
++  rd
  =+  ma==>(ff .(w 11, p 52, b --1.023, r %n))
  |%
  ++  sea
    |=  [a=@rd]  (sea:ma a)
  ++  bit
    |=  [a=fn]  ^-  @rd  (bit:ma a)
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
  --
--
