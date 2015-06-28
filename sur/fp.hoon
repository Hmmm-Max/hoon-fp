|%
++  fn  ::  float, infinity, or NaN
        ::  s=sign, e=exponent, a=arithmetic form
        ::  (-1)^s * a * 2^e
        $%  [%f s=? e=@s a=@u]
            [%i s=?]
            [%n ~]
        ==
::
++  dn  ::  decimal float, infinity, or NaN
        ::  (-1)^s * a * 10^e
        $%  [%d s=? e=@s a=@u]
            [%i s=?]
            [%n ~]
        ==
--
