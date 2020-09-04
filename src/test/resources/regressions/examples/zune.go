package zune

pure func isLeapYear(year int) bool {
  return year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
}

const originYear = 1980


func convertDaysBug(totalDays int) (days, year int) {

  days = totalDays
  year = originYear

  for days > 365 {

    ghost variant := days
    assert variant >= 0

    if isLeapYear(year) {
      if days > 366 {
        days, year = days-366, year+1
      }
    } else {
      days, year = days-365, year+1
    }

    //:: ExpectedOutput(assert_error:assertion_error)
    assert days < variant
  }
}


ensures days <= 366
func convertDaysFixed(totalDays int) (days, year int) {

  days = totalDays
  year = originYear

  for (isLeapYear(year) && 366 < days) || (!isLeapYear(year) && 365 < days) {

    ghost variant := days
    assert variant >= 0

    if isLeapYear(year) {
      days -= 366
    } else {
      days -= 365
    }
    year += 1

    assert days < variant
  }
}


ensures days + (year - originYear) * 365 <= totalDays
ensures totalDays <= days + (year - originYear) * 366
ensures days <= 366
func convertDaysFixedWithSomeInvariants(totalDays int) (days, year int) {

  days = totalDays
  year = originYear

  invariant days + (year - originYear) * 365 <= totalDays
  invariant totalDays <= days + (year - originYear) * 366
  for (isLeapYear(year) && 366 < days) || (!isLeapYear(year) && 365 < days) {

    ghost variant := days
    assert variant >= 0

    if isLeapYear(year) {
      days -= 366
    } else {
      days -= 365
    }
    year += 1

    assert days < variant
  }
}