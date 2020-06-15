package pkg

func example1(ghost s set[int]) {
  ghost t := set(s)
  assert s == t
}

func example2(ghost s set[int]) {
  assert set(set(set(s))) == s
}

ensures set(s union t) == set(s) union set(t)
func example3(ghost s set[int], ghost t set[int]) {
}

requires x in s
ensures x in set(s)
func example4(x int, ghost s set[int]) {
}

func example5() {
  assert set(set[bool] { true, false }) == set[bool] { true, false }
}