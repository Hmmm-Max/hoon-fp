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
  ++  syn
    |=  [a=fn]  ^-  ?
    ?-(-.a %f s.a, %i s.a, %n &)
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
          %z  (lug %fl a &)  %d  (lug %fl a &)
          %a  (lug %lg a &)  %u  (lug %lg a &)
          %n  (lug %na a &)
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
          %z  (lug %sm a &)  %d  (lug %sm a &)
          %a  (lug %ce a &)  %u  (lug %ce a &)
          %n  (lug %nt a &)
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
      =+  k=(mod a.a a.b)
      (rau [j q] =(k 0))
    ::
    ++  fma
      |=  [a=[e=@s a=@u] b=[e=@s a=@u] c=[e=@s a=@u]]  ^-  fn
      (add [(sum:si e.a e.b) (^^mul a.a a.b)] c |)
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
    ::  in order: floor, ceiling, nearest (even, away from 0, toward 0), larger, smaller
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
    ++  ned
      |=  [a=fn]  ^-  [%f s=? e=@s a=@u]
      ?:  ?=([%f *] a)  a
      ~|  %need-float  !!
    ::
    ++  shf
      |=  [a=fn b=@s]
      ?:  |(?=([%n *] a) ?=([%i *] a))  a
      a(e (sum:si e.a b))
    ::
    ++  swr  ?+(r r %d %u, %u %d)
    ++  prc  ?>((^gth p 1) p)
    ++  mxp  20.000                                     ::  max precision for some stuff
    ++  den  d
    ++  emn  v
    ++  emm  (sum:si emn (sun:si (dec prc)))
    ++  emx  (sum:si emn (sun:si w))
    ++  spd  [e=emn a=1]                                ::  smallest "denormal"
    ++  spn  [e=emn a=(bex (dec prc))]                  ::  smallest "normal"
    ++  lfn  [e=emx a=(fil 0 prc 1)]                    ::  largest
    ++  lfe  (sum:si emx (sun:si prc))                  ::  2^lfe is larger than all floats
    ++  zer  [e=--0 a=0]
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
  ++  lth  |=  [a=@r b=@r]  (fall (lth:pa (sea a) (sea b)) |)
  ++  lte  |=  [a=@r b=@r]  (fall (lte:pa (sea a) (sea b)) |)
  ++  equ  |=  [a=@r b=@r]  (fall (equ:pa (sea a) (sea b)) |)
  ++  gte  |=  [a=@r b=@r]  (fall (gte:pa (sea a) (sea b)) |)
  ++  gth  |=  [a=@r b=@r]  (fall (gth:pa (sea a) (sea b)) |)
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
  ~%  %rd  +  ~
  |%
  ++  ma
    %*(. ff w 11, p 52, b --1.023, f %.n)
  ++  sea
    |=  [a=@rd]  (sea:ma a)
  ++  bit
    |=  [a=fn]  ^-  @rd  (bit:ma a)
  ++  add  ~/  %add
    |=  [a=@rd b=@rd]  ^-  @rd  (add:ma a b)
  ++  sub  ~/  %sub
    |=  [a=@rd b=@rd]  ^-  @rd  (sub:ma a b)
  ++  mul  ~/  %mul
    |=  [a=@rd b=@rd]  ^-  @rd  (mul:ma a b)
  ++  div  ~/  %div
    |=  [a=@rd b=@rd]  ^-  @rd  (div:ma a b)
  ++  fma  ~/  %fma
    |=  [a=@rd b=@rd c=@rd]  ^-  @rd  (fma:ma a b c)
  ++  sqt  ~/  %sqt
    |=  [a=@rd]  ^-  @rd  (sqt:ma a)
  ::
  ++  sun  |=  [a=@u]  ^-  @rd  (sun:ma a)
  ++  san  |=  [a=@s]  ^-  @rd  (san:ma a)
  ++  lth  ~/  %lth  |=  [a=@rd b=@rd]  (lth:ma a b)
  ++  lte  ~/  %lte  |=  [a=@rd b=@rd]  (lte:ma a b)
  ++  equ  ~/  %equ  |=  [a=@rd b=@rd]  (equ:ma a b)
  ++  gte  ~/  %gte  |=  [a=@rd b=@rd]  (gte:ma a b)
  ++  gth  ~/  %gth  |=  [a=@rd b=@rd]  (gth:ma a b)
  ++  sig  |=  [a=@rd]  (sig:ma a)
  ++  exp  |=  [a=@rd]  (exp:ma a)
  ++  drg  |=  [a=@rd]  (drg:ma a)
  ++  grd  |=  [a=dn]  (grd:ma a)
  --
::
++  rs
  ~%  %rs  +  ~
  |%
  ++  ma
    %*(. ff w 8, p 23, b --127, f %.n)
  ++  sea
    |=  [a=@rs]  (sea:ma a)
  ++  bit
    |=  [a=fn]  ^-  @rs  (bit:ma a)
  ++  add  ~/  %add
    |=  [a=@rs b=@rs]  ^-  @rs  (add:ma a b)
  ++  sub  ~/  %sub
    |=  [a=@rs b=@rs]  ^-  @rs  (sub:ma a b)
  ++  mul  ~/  %mul
    |=  [a=@rs b=@rs]  ^-  @rs  (mul:ma a b)
  ++  div  ~/  %div
    |=  [a=@rs b=@rs]  ^-  @rs  (div:ma a b)
  ++  fma  ~/  %fma
    |=  [a=@rs b=@rs c=@rs]  ^-  @rs  (fma:ma a b c)
  ++  sqt  ~/  %sqt
    |=  [a=@rs]  ^-  @rs  (sqt:ma a)
  ::
  ++  sun  |=  [a=@u]  ^-  @rs  (sun:ma a)
  ++  san  |=  [a=@s]  ^-  @rs  (san:ma a)
  ++  lth  ~/  %lth  |=  [a=@rs b=@rs]  (lth:ma a b)
  ++  lte  ~/  %lte  |=  [a=@rs b=@rs]  (lte:ma a b)
  ++  equ  ~/  %equ  |=  [a=@rs b=@rs]  (equ:ma a b)
  ++  gte  ~/  %gte  |=  [a=@rs b=@rs]  (gte:ma a b)
  ++  gth  ~/  %gth  |=  [a=@rs b=@rs]  (gth:ma a b)
  ++  sig  |=  [a=@rs]  (sig:ma a)
  ++  exp  |=  [a=@rs]  (exp:ma a)
  ++  drg  |=  [a=@rs]  (drg:ma a)
  ++  grd  |=  [a=dn]  (grd:ma a)
  --
::
++  rq
  ~%  %rq  +  ~
  |%
  ++  ma
    %*(. ff w 15, p 112, b --16.383, f %.n)
  ++  sea
    |=  [a=@rq]  (sea:ma a)
  ++  bit
    |=  [a=fn]  ^-  @rq  (bit:ma a)
  ++  add  ~/  %add
    |=  [a=@rq b=@rq]  ^-  @rq  (add:ma a b)
  ++  sub  ~/  %sub
    |=  [a=@rq b=@rq]  ^-  @rq  (sub:ma a b)
  ++  mul  ~/  %mul
    |=  [a=@rq b=@rq]  ^-  @rq  (mul:ma a b)
  ++  div  ~/  %div
    |=  [a=@rq b=@rq]  ^-  @rq  (div:ma a b)
  ++  fma  ~/  %fma
    |=  [a=@rq b=@rq c=@rq]  ^-  @rq  (fma:ma a b c)
  ++  sqt  ~/  %sqt
    |=  [a=@rq]  ^-  @rq  (sqt:ma a)
  ::
  ++  sun  |=  [a=@u]  ^-  @rq  (sun:ma a)
  ++  san  |=  [a=@s]  ^-  @rq  (san:ma a)
  ++  lth  ~/  %lth  |=  [a=@rq b=@rq]  (lth:ma a b)
  ++  lte  ~/  %lte  |=  [a=@rq b=@rq]  (lte:ma a b)
  ++  equ  ~/  %equ  |=  [a=@rq b=@rq]  (equ:ma a b)
  ++  gte  ~/  %gte  |=  [a=@rq b=@rq]  (gte:ma a b)
  ++  gth  ~/  %gth  |=  [a=@rq b=@rq]  (gth:ma a b)
  ++  sig  |=  [a=@rq]  (sig:ma a)
  ++  exp  |=  [a=@rq]  (exp:ma a)
  ++  drg  |=  [a=@rq]  (drg:ma a)
  ++  grd  |=  [a=dn]  (grd:ma a)
  --
::
++  rh
  |%
  ++  ma
    %*(. ff w 5, p 10, b --15, f %.n)
  ++  sea
    |=  [a=@rh]  (sea:ma a)
  ++  bit
    |=  [a=fn]  ^-  @rh  (bit:ma a)
  ::
  ++  sun  |=  [a=@u]  ^-  @rh  (sun:ma a)
  ++  san  |=  [a=@s]  ^-  @rh  (san:ma a)
  ++  lth  |=  [a=@rh b=@rh]  (lth:ma a b)
  ++  lte  |=  [a=@rh b=@rh]  (lte:ma a b)
  ++  equ  |=  [a=@rh b=@rh]  (equ:ma a b)
  ++  gte  |=  [a=@rh b=@rh]  (gte:ma a b)
  ++  gth  |=  [a=@rh b=@rh]  (gth:ma a b)
  ++  sig  |=  [a=@rh]  (sig:ma a)
  ++  exp  |=  [a=@rh]  (exp:ma a)
  ++  drg  |=  [a=@rh]  (drg:ma a)
  ++  grd  |=  [a=dn]  (grd:ma a)
  --
--
