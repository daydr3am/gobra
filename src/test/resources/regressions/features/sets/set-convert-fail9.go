package pkg

func foo(ghost xs seq[int], ghost ys seq[bool]) {
  //:: ExpectedOutput(type_error)
  assert set(xs) == set(ys)
}
