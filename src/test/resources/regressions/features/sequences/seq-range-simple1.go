package pkg

requires xs == ys
func example1(ghost xs seq[int], ghost ys seq[int]) {
}

func example2() {
  assert seq[1 .. 4] == seq[int] { 1, 2, 3 }
}

func example3() {
  assert seq[4 .. 1] == seq[int] { }
}

func example4() {
  assert seq[-4 .. -1] == seq[int] { -4, -3, -2 }
}

func example5() {
  assert |seq[1..4]| == 3
  assert seq[1..4] ++ seq[4..8] == seq[1..8]
}

requires x <= y
func example6(x int, y int) {
  assert |seq[x..y + 1]| == y - x + 1
}
