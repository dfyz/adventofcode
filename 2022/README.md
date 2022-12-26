Running times of my solutions (most of them are bottlenecked on inefficient algorithms and not anything OCaml-related):
```
PS D:\dfyz\adventofcode\2022\bin> for ($i = 1; $i -le 25; $i++) { echo "==="; echo ("Task #{0:D2}" -f $i); $start=Get-Date; dune exec --display quiet --no-print-directory --profile release ("./sln_{0:D2}.exe" -f $i); $end=Get-Date; echo ("time: {0} seconds" -f ($end - $start).TotalSeconds); }
===
Task #01
easy: 75622
hard: 213159
time: 0.1265527 seconds
===
Task #02
easy: 15422
hard: 15442
time: 0.1104132 seconds
===
Task #03
easy: 7727
hard: 2609
time: 0.1410795 seconds
===
Task #04
easy: 573
hard: 867
time: 0.1094773 seconds
===
Task #05
easy: WHTLRMZRC
hard: GMPMLWNMG
time: 0.110168 seconds
===
Task #06
easy: 1655
hard: 2665
time: 0.1260066 seconds
===
Task #07
easy: 1454188
hard: 4183246
time: 0.1226608 seconds
===
Task #08
easy: 1713
hard: 268464
time: 0.130365 seconds
===
Task #09
easy: 6391
hard: 2593
time: 0.1266449 seconds
===
Task #10
easy: 12980
hard:
###..###....##.#....####.#..#.#....###..
#..#.#..#....#.#....#....#..#.#....#..#.
###..#..#....#.#....###..#..#.#....#..#.
#..#.###.....#.#....#....#..#.#....###..
#..#.#.#..#..#.#....#....#..#.#....#....
###..#..#..##..####.#.....##..####.#....
time: 0.1212895 seconds
===
Task #11
easy: 108240
hard: 25712998901
time: 0.3186719 seconds
===
Task #12
easy: 440
hard: 439
time: 0.3259625 seconds
===
Task #13
easy: 6428
hard: 22464
time: 0.1258768 seconds
===
Task #14
easy: 1078
hard: 30157
time: 12.0084614 seconds
===
Task #15
easy: 6275922
hard: 11747175442119
time: 2.9030745 seconds
===
Task #16
easy: 1767
hard: 2528
time: 7.3514495 seconds
===
Task #17
easy: 3153
hard: 1553665689155
time: 14.4786984 seconds
===
Task #18
easy: 4364
hard: 2508
time: 0.1909158 seconds
===
Task #19
easy: 1650
hard: 5824
time: 538.06387 seconds
===
Task #20
easy: 6387
hard: 2455057187825
time: 6.3395714 seconds
===
Task #21
easy: 80326079210554
hard: 3617613952378
time: 0.1254557 seconds
===
Task #22
easy: 93226
hard: 37415
time: 0.1307068 seconds
===
Task #23
easy: 3780
hard: 930
time: 4.324918 seconds
===
Task #24
easy: 257
hard: 828
time: 5.7111303 seconds
===
Task #25
easy: 2=0--0---11--01=-100
hard: *
time: 0.1265883 seconds
```
