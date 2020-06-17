package pkg

func example1(ghost xs seq[int]) {
  ghost s1 := set(xs)
  ghost s2 := set(s1)
  assert s1 == s2
}

func example2(ghost xs seq[int], ghost ys seq[int]) {
  ghost s := set(xs ++ ys)
}

func example3(ghost xs seq[int]) {
  assert |set(xs)| <= |xs|
}

func example4() {
  ghost xs := seq[int] { 1, 2, 3 }
  assert 3 in xs
  assert 3 in set(xs)
  assert 42 in set(seq[1..100])
  assert !(144 in set(seq[1..12]))
}

func example5(ghost xs seq[int], ghost ys seq[int]) {
  assert set(xs ++ ys) == set(ys ++ xs)
}

func example6() {
  assert set(seq[int] { }) == set[int] { }
  assert set(seq[int] { 1, 2, 3 }) == set[int] { 1, 2, 3 }
  assert set(seq[1..4]) == set[int] { 1, 2, 3 }
  assert set(seq[int] { 1, 2, 3, 2, 1 }) == set[int] { 3, 1, 2 }
}

func example7() {
  // TODO could we do without this assertion?
  assert seq[0..10][5:] == seq[5..10]
  assert 8 in set(seq[0..10][5:])
  
  // TODO could we do without this assertion?
  assert seq[0..10][:5] == seq[0..5]
  assert 2 in set(seq[0..10][:5])
}

func example8(ghost xs seq[int], ghost ys seq[int]) {
  assert set(xs) subset set(xs ++ ys)
  assert set(xs) union set(xs ++ ys) == set(xs ++ ys)
  assert set(xs) intersection set(xs ++ ys) == set(xs)
  assert 42 in ys ==> 42 in set(xs ++ ys)
}

func example9(ghost xs seq[int]) {
  assert set(xs ++ xs) == set(xs)
}

func example10(ghost xs seq[int], n int, i int) {
  assert n in xs[:i] ==> n in set(xs)
  assert n in xs[i:] ==> n in set(xs)
  assert n in xs[i:] ==> n in set(xs[i:])
}
