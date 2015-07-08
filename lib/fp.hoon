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
  |_  [[p=@u v=@s w=@u] r=?(%n %u %d %z %a) d=?(%d %f %i)]
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
      =.(r swr:m (fli (add:m +>.a +>.b &)))
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
    =+  q=(abs:si e.a)
    ?:  (syn:si e.a)
      (mul [%f s.a --0 a.a] [%f & e.a (pow:m 5 q)])
    (div [%f s.a --0 a.a] [%f & (sun:si q) (pow:m 5 q)])
  ::
  ++  c                                                 ::  mathematical constants
    |%
    ++  pi
      ~+  |-  ^-  fn
      =-
        =+  wp=(^add prc:m 16)
        =+  nc=16
        |-
        ?:  (^gth wp mxp:m)
          ~|  %very-large-precision  !!
        =+  [x=(bnd:m (ka wp))]
        ?~  x  $(wp (^add wp nc), nc (^mul nc 2))
        +.x
      ::
      ^=  ka  |=  [p=@]  ^-  [fn fn]
      =>  .(r %n, ^p p, d %i)
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
      ~+  |-  ^-  fn
      =-
        =+  wp=(^add prc:m 8)
        =+  nc=8
        |-
        ?:  (^gth wp mxp:m)
          ~|  %very-large-precision  !!
        =+  [x=(bnd:m (ka wp))]
        ?~  x  $(wp (^add wp nc), nc (^mul nc 2))
        +.x
      ::
      ^=  ka  |=  [p=@]  ^-  [fn fn]
      =>  .(r %n, ^p p, d %i)
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
      =-
        =+  wp=(^add prc:m 16)
        =+  nc=32
        |-
        ?:  (^gth wp mxp:m)
          ~|  %very-large-precision  !!
        =+  [x=(bnd:m (ka wp))]
        ?~  x  $(wp (^add wp nc), nc (^mul nc 2))
        +.x
      ::
      ^=  ka  |=  [p=@]  ^-  [fn fn]
      =+  n=prc:m
      =>  .(r %n, ^p p, d %i)
      =.  a  (ned:m (rem:m a (shf:m pi:c --1)))         ::  cmod 2pi
      =+  k=-:(itr:m (^div n 2))
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
      =-
        =+  wp=(^add prc:m 16)
        =+  nc=32
        |-
        ?:  (^gth wp mxp:m)
          ~|  %very-large-precision  !!
        =+  [x=(bnd:m (ka wp))]
        ?~  x  $(wp (^add wp nc), nc (^mul nc 2))
        +.x
      ::
      ^=  ka  |=  [p=@]  ^-  [fn fn]
      =>  .(r %n, ^p p, d %i)
      =.  a  (ned:m (rem:m a (shf:m pi:c --1)))
      =+  c==>(.(r %a) (cos a))
      =+  t==>(.(r %a) (mul c c))
      =+  u==>(.(r %z) (sub [%f & --0 1] t))
      =+  s=(ned:m =>(.(r %z) (sqt u)))
      :-  s(s +<.a)
      =+  e=(sum:si (^mul (sun:si p) 2) e.s)
      [%f & (dif:si --3 e) 1]
    ::
    ++  tan
      |=  [a=fn]  ^-  fn
      ?.  ?=([%f *] a)  [%n ~]
      =-
        =+  wp=(^add prc:m 8)
        =+  nc=32
        |-
        ?:  (^gth wp mxp:m)
          ~|  %very-large-precision  !!
        =+  [x=(bnd:m (ka wp))]
        ?~  x  $(wp (^add wp nc), nc (^mul nc 2))
        +.x
      ::
      ^=  ka  |=  [p=@]  ^-  [fn fn]
      =>  .(r %n, ^p p, d %i)
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
    ++  exp
      |=  [a=fn]  ^-  fn
      !!
    ::
    ++  log
      |=  [a=fn]  ^-  fn
      !!
    ::
    ++  log2
      |=  [a=fn]  ^-  fn
      !!
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
        =+  [x=(bnd:m (ka wp))]
        ?~  x  $(wp (^add wp nc), nc (^mul nc 2))
        +.x
      ::
      ^=  ka  |=  [p=@]  ^-  [fn fn]
      =>  .(r %n, ^p p, d %i)
      =+  s=(ned:m (mul a b))
      =+  u=(ned:m (sqt s))
      =+  ^=  v
        =+  q=(ned:m (add a b))
        q(e (dif:si e.q --1))
      =+  n=1  |-
      =+  j=(ned:m (ead v (fli u)))
      =+  ^=  y  |.  %+  cmp:si                         ::  using trap to delay computation
          (dif:si (ibl:m +>.v) (ibl:m +>.j))            ::  until ensuring a.j != 0
        (sun:si (^sub p 2))
      ?:  |(=(a.j 0) =((y) --1))
        [v [%f & e.v (^add (^mul n 18) 51)]]            ::  XX error bounds correct?
      =+  ^=  nv
        =+  q=(ned:m (add u v))
        q(e (dif:si e.q --1))
      $(v nv, u (ned:m (sqt (mul u v))), n +(n))
    --
  ::
  ++  m                                                 ::  internal functions, constants
    |%                                                  ::  don't put 0s into [@s @u] args
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
    ::  integer square root w/rounding info
    ++  itr
      |=  [a=@]  ^-  [@ ?(%e %d %h %u)]
      =+  [q=(^^div (dec (xeb a)) 2) r=0]
      =+  ^=  c
        |-  =+  s=(^^add r (bex q))
        =+  (^^mul s s)
        ?:  =(q 0)
          ?:  (^^lte - a)  [s -]  [r (^^mul r r)]
        ?:  (^^lte - a)  $(r s, q (dec q))  $(q (dec q))
      ?:  =(+.c a)  [-.c %e]
      =+  v=(^^add (lsh 0 1 -.c) 1)
      =+  y=(^^mul v v)
      ?:  (^^lth y (lsh 0 2 a))  [-.c %u]  [-.c %d]
    ::
    ::  integer inverse square root w/shift amount & rounding info
    ++  iir
      |=  [a=@]  ^-  [@ @ ?(%e %d %h %u)]
      =+  [sa=(dec (xeb a))]
      =+  [q=(^^div (xeb a) 2) z=(bex (^^mul sa 2)) r=0]
      =+  ^=  c
        |-  =+  s=(^^add r (bex q))
        =+  (^^mul a (^^mul s s))
        ?:  =(q 0)
          ?:  (^^lte - z)  [s -]  [r (^^mul a (^^mul r r))]
        ?:  (^^lte - z)  $(r s, q (dec q))  $(q (dec q))
      ?:  =(+.c z)  [-.c sa %e]
      =+  v=(^^add (lsh 0 1 -.c) 1)
      =+  y=(^^mul a (^^mul v v))
      =+  w=(lsh 0 2 z)
      ?:  =(y w)  [-.c sa %h]
      ?:  (^^lth y w)  [-.c sa %u]  [-.c sa %d]
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
        =+  [w=(met 0 a.a) x=(^^mul prc 2)]
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
        =+  [w=(met 0 a.a) x=(^^mul prc 2)]
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
      ::
      =-                                                ::  if !den, flush denormals to zero
        ?.  =(den %f)  -
        ?.  ?=([%f *] -)  -
        ?:  =((met 0 ->+>) prc)  -  [%f & zer]
      ::
      =+  m=(met 0 a.a)
      ?>  |(=(f %e) (^gte m (rpr e.a)))                 ::  if not %e, need sufficient precision
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
          %fl  a
          %lg  a(a +(a.a))
          %sm  ?.  &(=(b 0) =(f %e))  a
               ?:  &(=(e.a emn) !=(den %i))  a(a (dec a.a))
               =+  y=(dec (^^mul a.a 2))
               ?.  (^^lte (met 0 y) prc)  a(a (dec a.a))
               [(dif:si e.a --1) y]
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
    ++  cmd
      |=  [a=@u b=@u]  ^-  @s
      =+  c=(^^div a b)
      =+  d=(mod a b)
      =+  e=(^^mul d 2)
      =+  ^=  f
        ?:  (^^lth e b)  c
        ?.  =(e b)  +(c)
        ?~((end 0 1 c) c +(c))
      (dif:si (sun:si a) (sun:si (^^mul b f)))
    ::
    ++  rem
      |=  [a=fn b=fn]                                   ::  a cmod b
      ?:  |(?=([%n *] a) ?=([%n *] b))  [%n ~]
      ?:  |(?=([%i *] a) ?=([%i *] b))  [%n ~]
      ?~  a.a  [%f & zer:m]  ?~  a.b  [%n ~]
      =+  [ma=(met 0 a.a) mb=(met 0 a.b)]
      =+  ^=  q
        ?.  =((cmp:si e.a e.b) -1)  --0
        (dif:si e.b e.a)
      =+  al=a(a (end 0 (abs:si q) a.a))
      =+  ah=a(a (rsh 0 (abs:si q) a.a), e (sum:si e.a q))
      =+  w=(abs:si (dif:si e.ah e.b))
      =+  z=(mod (bex w) a.b)
      =+  x=(old:si (cmd:m (^^mul a.ah z) a.b))
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
    =+  i=(dif:si --1 b)
    =+  q=fl
    q(p +(p), v i, w (^sub (bex w) 3), d ?:(f %f %d), r r)
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
++  rh  =>  ff  .(w 5, p 10, b --15, f %.n, r %n)
++  rs  =>  ff  .(w 8, p 23, b --127, f %.n, r %n)
++  rq  =>  ff  .(w 15, p 112, b --16.383, f %.n, r %n)
::
++  rd
  =+  ma==>(ff .(w 11, p 52, b --1.023, f %.n, r %n))
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
